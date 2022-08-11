#include "llvm/ADT/Statistic.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Pass.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Transforms/CryptoUtils.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/Transforms/Utils/Local.h"
#include "llvm/Transforms/Utils/ValueMapper.h"
#include <list>

using namespace llvm;

#define DEBUG_TYPE "obfs-bcf"

static cl::opt<unsigned> ProbabilityRate(
  "obfs-bcf-prob", cl::NotHidden,
  cl::desc("The probability (%) each basic block "
             "will be obfuscated by the obfs-bcf pass"),
  cl::init(70),
  cl::Optional
);

static cl::opt<unsigned> LoopTimes(
  "obfs-bcf-loop", cl::NotHidden,
  cl::desc("Number of times the obfs-bcf pass will loop on a function"),
  cl::init(1),
  cl::Optional
);

static cl::opt<unsigned> ConditionExpressionComplexity(
  "obfs-bcf-cond-compl", cl::NotHidden,
  cl::desc("The complexity of the expression "
           "used to generate branching condition"),
  cl::init(3),
  cl::Optional
);

STATISTIC(BogusBasicBlocksCounter, "Counts number of basic blocks obfuscated");

static Instruction::BinaryOps Ops[] = {
  Instruction::Add, Instruction::Sub,
  Instruction::And, Instruction::Or,
  Instruction::Xor
};

static CmpInst::Predicate Preds[] = {
  CmpInst::ICMP_EQ,  CmpInst::ICMP_NE,
  CmpInst::ICMP_UGT, CmpInst::ICMP_UGE,
  CmpInst::ICMP_ULT, CmpInst::ICMP_ULE
};

namespace {

struct BogusControlFlow : public FunctionPass {
  static char ID; // Pass identification, replacement for typeid
  BogusControlFlow() : FunctionPass(ID) {
    llvm::SharedCryptoUtils->prng_seed();
  }

  /// Shamefully stolen from IPO/StripSymbols.cpp
  /// OnlyUsedBy - Return true if V is only used by Usr.
  bool onlyUsedBy(Value *V, Value *Usr) {
    for (User *U : V->users())
      if (U != Usr)
        return false;

    return true;
  }

  void removeDeadConstant(Constant *C) { // NOLINT(misc-no-recursion)
    assert(C->use_empty() && "Constant is not dead!");
    SmallPtrSet<Constant*, 4> Operands;
    for (Value *Op : C->operands())
      if (onlyUsedBy(Op, C))
        Operands.insert(cast<Constant>(Op));
    if (GlobalVariable *GV = dyn_cast<GlobalVariable>(C)) {
      if (!GV->hasLocalLinkage()) return;   // Don't delete non-static globals.
      GV->eraseFromParent();
    } else if (!isa<Function>(C)) {
      // FIXME: Why does the type of the constant matter here?
      if (isa<StructType>(C->getType()) || isa<ArrayType>(C->getType()) ||
          isa<VectorType>(C->getType()))
        C->destroyConstant();
    }

    // If the constant referenced anything, see if we can delete it as well.
    for (Constant *Co : Operands)
      removeDeadConstant(Co);
  }

  bool runOnFunction(Function &F) override {
    if (LoopTimes == 0) {
      errs() << "obfs-bcf-loop should be greater than 0\n";
      return false;
    }
    if (ProbabilityRate > 100) {
      errs() << "obfs-bcf-prob should be less than 100\n";
      return false;
    }
    doBogusOnFunction(F);
    return true;
  }

  void doBogusOnFunction(Function &F) {

    bool HasBeenModified = false;

    DEBUG_WITH_TYPE(DEBUG_TYPE "-opt",
                    errs() << "Running obfs-bcf on function " << F.getName() << "\n");
    DEBUG_WITH_TYPE(DEBUG_TYPE "-opt",
                    errs() << "  Probability rate: " << ProbabilityRate << "%\n");
    DEBUG_WITH_TYPE(DEBUG_TYPE "-opt",
                    errs() << "  Loop times: " << LoopTimes << "\n");

    unsigned NumLoopTimes = LoopTimes;
    do {

      int LoopIndex = LoopTimes - NumLoopTimes + 1;
      DEBUG_WITH_TYPE(DEBUG_TYPE "-cfg",
                      errs() << "CFG of function " << F.getName() << " before loop #" << LoopIndex << ": \n");
      DEBUG_WITH_TYPE(DEBUG_TYPE "-cfg", F.viewCFG());

      std::list<BasicBlock *> BasicBlocks;
      for (BasicBlock &BB : F) {
        if (!BB.isEHPad() && !BB.isLandingPad() && !basicBlockContainsInlineAsm(BB))
          BasicBlocks.push_back(&BB);
      }

      DEBUG_WITH_TYPE(DEBUG_TYPE "-gen",
                      errs() << "  Iterating on the basic blocks of function " << F.getName() << "\n");
      while (!BasicBlocks.empty()) {
        BogusBasicBlocksCounter++;
        if (llvm::SharedCryptoUtils->get_range(100) < ProbabilityRate) {
          DEBUG_WITH_TYPE(DEBUG_TYPE "-opt",
                          errs() << "    Obfuscating basic block #" << BogusBasicBlocksCounter << "\n");
          addBogusFlow(BasicBlocks.front(), F);
          HasBeenModified = true;
        } else {
          DEBUG_WITH_TYPE(DEBUG_TYPE "-opt",
                          errs() << "    Skipping basic block #" << BogusBasicBlocksCounter << "\n");
        }
        BasicBlocks.pop_front();
      }
      DEBUG_WITH_TYPE(DEBUG_TYPE "-cfg",
                      errs() << "  Finished obfuscating the basic blocks of function " << F.getName() << "\n");

      if (HasBeenModified) {
        DEBUG_WITH_TYPE(DEBUG_TYPE "-cfg",
                        errs() << "  Function " << F.getName() << " has been modified during loop #" << LoopIndex << "\n");
        DEBUG_WITH_TYPE(DEBUG_TYPE "-cfg", F.viewCFG());
        HasBeenModified = false;
      } else {
        DEBUG_WITH_TYPE(DEBUG_TYPE "-cfg",
                        errs() << "  Function " << F.getName() << " has not been modified during loop #" << LoopIndex << "\n");
      }
    } while (--NumLoopTimes);
  }

  bool basicBlockContainsInlineAsm(BasicBlock &BB) {
    for (Instruction &I : BB) {
      for (unsigned Oi = 0; Oi < I.getNumOperands(); Oi++) {
        if (isa<InlineAsm>(I.getOperand(Oi))) {
          return true;
        }
      }
    }
    return false;
  }

  void addBogusFlow(BasicBlock *InputBB, Function &F) {

    // Split the block:
    // First part with only the phi nodes and debug info and
    // terminator created by splitBasicBlock (-> No instruction).
    // Second part with every instruction from the original block.
    // We do this way, so we don't have to adjust all the phi nodes, metadata
    // and so on for the first block. We have to let the phi nodes in the first
    // part, because they actually are updated in the second part according to
    // them.
    BasicBlock::iterator I1 = InputBB->begin();
    if (InputBB->getFirstNonPHIOrDbgOrLifetime())
      I1 = (BasicBlock::iterator)InputBB->getFirstNonPHIOrDbgOrLifetime();
    Twine *Var;
    Var = new Twine("OriginalBB");
    BasicBlock *OriginalBB = InputBB->splitBasicBlock(I1, *Var);
    DEBUG_WITH_TYPE(DEBUG_TYPE "-gen",
                    errs() << "    First and original basic block: ok\n");

    // Creating the altered basic block on which the first basicBlock will jump to.
    Twine *Var3 = new Twine("AlteredBB");
    BasicBlock *AlteredBB = createAlteredBasicBlock(OriginalBB, *Var3, &F);
    DEBUG_WITH_TYPE(DEBUG_TYPE "-gen",
                    errs() << "    Altered basic block: ok\n");

    // Now that all the blocks are created,
    // we modify the terminators to adjust the control flow.
    AlteredBB->getTerminator()->eraseFromParent();
    InputBB->getTerminator()->eraseFromParent();
    DEBUG_WITH_TYPE(DEBUG_TYPE "-gen",
                    errs() << "    Terminator removed from the altered and first basic block\n");

    // Preparing a condition.
    // For now, the condition is an always true comparison between 2 float.
    // This will be complicated after the pass (in doFinalization()).

    // We need to use ConstantInt instead of ConstantFP as ConstantFP results in strange dead-loop
    // when injected into Xcode.
    Value *LHS = ConstantInt::get(Type::getInt32Ty(F.getContext()), 1);
    Value *RHS = ConstantInt::get(Type::getInt32Ty(F.getContext()), 1);
    DEBUG_WITH_TYPE(DEBUG_TYPE "-gen", errs() << "    Value LHS and RHS created\n");

    // The placeholder of an always true condition. End of the first block.
    ICmpInst *AlwaysTrueCondition =
      new ICmpInst(*InputBB, ICmpInst::ICMP_EQ, LHS, RHS, "BCFPlaceholderPred");
    DEBUG_WITH_TYPE(DEBUG_TYPE "-gen", errs() << "    Always true condition created\n");

    // Jump to the original basic block if the condition is true or
    // to the altered block if false.
    BranchInst::Create(OriginalBB, AlteredBB, (Value *)AlwaysTrueCondition, InputBB);
    DEBUG_WITH_TYPE(DEBUG_TYPE "-gen",
                    errs() << "    Terminator in first basic block: ok\n");

    // The altered block loop back on the original one.
    BranchInst::Create(OriginalBB, AlteredBB);
    DEBUG_WITH_TYPE(DEBUG_TYPE "-gen",
                    errs() << "    Terminator in altered block: ok\n");

    // The end of the originalBB is modified to give the impression that
    // sometimes it continues in the loop, and sometimes it return the desired
    // value. Of course, it's always true, so it always use the original
    // terminator. but this will be obfuscated too. ;-)

    // Iterate on instruction just before the terminator of the OriginalBB.
    BasicBlock::iterator I = OriginalBB->end();

    // Split at this point.
    // We only want the terminator in the second part.
    Twine *Var5 = new Twine("BCFOriginalBBPart2");
    BasicBlock *OriginalBBPart2 = OriginalBB->splitBasicBlock(--I, *Var5);
    DEBUG_WITH_TYPE(DEBUG_TYPE "-gen",
                    errs() << "    Terminator of the original basic block is isolated\n");

    // The first part go either on the return statement or on the beginning
    // of the altered block. So we erase the terminator created when splitting.
    OriginalBB->getTerminator()->eraseFromParent();

    // Add a new always true condition at the end.
    ICmpInst *AlwaysTrueCondition2 =
      new ICmpInst(*OriginalBB, CmpInst::ICMP_EQ, LHS, RHS,"BCFPlaceholderPred");

    // Do random behavior to avoid pattern recognition.
    // This is achieved by jumping to a random BB.
    switch (llvm::SharedCryptoUtils->get_uint16_t() % 2) {
    case 1: {
      BranchInst::Create(OriginalBBPart2, AlteredBB, AlwaysTrueCondition2,
                         OriginalBB);
      break;
    }
    default: {
      BranchInst::Create(OriginalBBPart2, OriginalBB, AlwaysTrueCondition2,
                         OriginalBB);
      break;
    }
    }

    DEBUG_WITH_TYPE(DEBUG_TYPE "-gen",
                    errs() << "    Terminator in original basic block: ok\n");
    DEBUG_WITH_TYPE(DEBUG_TYPE "-gen",
                    errs() << "    END addBogusFlow()\n");
  }

  /* This function return a basic block similar to a given one.
   * It's inserted just after the given basic block.
   * The instructions are similar but junk instructions are added between
   * the cloned one. The cloned instructions' phi nodes, metadata, uses and
   * debug locations are adjusted to fit in the cloned basic block and
   * behave nicely.
   */
  BasicBlock *createAlteredBasicBlock(BasicBlock *InputBB, const Twine &Name = "gen", Function *F = nullptr) {

    // Useful to remap the information concerning instructions
    ValueToValueMapTy VMap;
    BasicBlock *AlteredBB = llvm::CloneBasicBlock(InputBB, VMap, Name, F);
    DEBUG_WITH_TYPE(DEBUG_TYPE "-gen",
                    errs() << "      Original basic block cloned\n");

    // Remap operands
    BasicBlock::iterator Ji = InputBB->begin();
    for (BasicBlock::iterator Ai = AlteredBB->begin(), Ae = AlteredBB->end();
         Ai != Ae; ++Ai) {

      // Loop over the operands of the instruction
      for (User::op_iterator Oi = Ai->op_begin(), Oe = Ai->op_end(); Oi != Oe; ++Oi) {

        // Get the value for the operand
        Value *V = MapValue(*Oi, VMap, RF_None, 0);
        if (V != 0) {
          *Oi = V;
          DEBUG_WITH_TYPE(DEBUG_TYPE "-gen",
                          errs() << "      Operand of value set\n");
        }
      }
      DEBUG_WITH_TYPE(DEBUG_TYPE "-gen",
                      errs() << "      Operands remapped\n");

      // Remap phi nodes' incoming blocks.
      if (PHINode *Pn = dyn_cast<PHINode>(Ai)) {
        for (unsigned Pj = 0, Pe = Pn->getNumIncomingValues(); Pj != Pe; ++Pj) {
          Value *V = MapValue(Pn->getIncomingBlock(Pj), VMap, RF_None, 0);
          if (V != nullptr) {
            Pn->setIncomingBlock(Pj, cast<BasicBlock>(V));
          }
        }
      }
      DEBUG_WITH_TYPE(DEBUG_TYPE "-gen",
                      errs() << "      PHI nodes remapped\n");

      // Remap attached metadata
      SmallVector<std::pair<unsigned, MDNode *>, 4> MDs;
      Ai->getAllMetadata(MDs);
      DEBUG_WITH_TYPE(DEBUG_TYPE "-gen",
                      errs() << "      Metadata remapped\n");

      // Important for compiling with DWARF, using option -g.
      Ai->setDebugLoc(Ji->getDebugLoc());
      Ji++;
      DEBUG_WITH_TYPE(DEBUG_TYPE "-gen",
                      errs() << "      Debug information location set\n");
    } // The information of instructions are now all correct.

    DEBUG_WITH_TYPE(DEBUG_TYPE "-gen",
                    errs() << "      The cloned basic block is now correct\n");
    DEBUG_WITH_TYPE(DEBUG_TYPE "-gen",
                    errs() << "      Will add junk code in the cloned block\n");

    // Add random instruction in the middle of the block.
    // This part can be improved.
    for (BasicBlock::iterator Ai = AlteredBB->begin(), Ae = AlteredBB->end();
         Ai != Ae; ++Ai) {

      // In the case we find binary operator, we modify slightly this part by
      // randomly insert some instructions.

      // Binary instructions
      if (Ai->isBinaryOp()) {

        unsigned Opcode = Ai->getOpcode();
        Instruction *Op, *Op1 = NULL;
        Twine *Var = new Twine("_");

        // Treat differently Float or Int
        // Binary Int
        if (Opcode == Instruction::Add  || Opcode == Instruction::Sub  ||
            Opcode == Instruction::Mul  || Opcode == Instruction::UDiv ||
            Opcode == Instruction::SDiv || Opcode == Instruction::URem ||
            Opcode == Instruction::SRem || Opcode == Instruction::Shl  ||
            Opcode == Instruction::LShr || Opcode == Instruction::AShr ||
            Opcode == Instruction::And  || Opcode == Instruction::Or   ||
            Opcode == Instruction::Xor)
        {
          for (int Ri = (int)llvm::SharedCryptoUtils->get_range(10); Ri < 10; ++Ri)
          {
            switch (llvm::SharedCryptoUtils->get_range(4)) {
            case 0:
              break;
            case 1:
              Op = BinaryOperator::CreateNeg(Ai->getOperand(0), *Var, &*Ai);
              BinaryOperator::Create(Instruction::Add, Op, Ai->getOperand(1),
                                     "gen", &*Ai);
              break;
            case 2:
              Op1 = BinaryOperator::Create(Instruction::Sub, Ai->getOperand(0),
                                           Ai->getOperand(1), *Var, &*Ai);
              BinaryOperator::Create(Instruction::Mul, Op1, Ai->getOperand(1),
                                     "gen", &*Ai);
              break;
            case 3:
              BinaryOperator::Create(Instruction::Shl, Ai->getOperand(0),
                                     Ai->getOperand(1), *Var, &*Ai);
              break;
            }
          }
        }

        // Binary Float
        if (Opcode == Instruction::FAdd || Opcode == Instruction::FSub ||
            Opcode == Instruction::FMul || Opcode == Instruction::FDiv ||
            Opcode == Instruction::FRem)
        {
          for (int Ri = (int)llvm::SharedCryptoUtils->get_range(10); Ri < 10; ++Ri)
          {
            switch (llvm::SharedCryptoUtils->get_range(3)) {
            case 0:
              break;
            case 1:
              Op = UnaryOperator::CreateFNeg(Ai->getOperand(0), *Var, &*Ai);
              BinaryOperator::Create(Instruction::FAdd, Op, Ai->getOperand(1),
                                     "gen", &*Ai);
              break;
            case 2:
              Op = BinaryOperator::Create(Instruction::FSub, Ai->getOperand(0),
                                          Ai->getOperand(1), *Var, &*Ai);
              BinaryOperator::Create(Instruction::FMul, Op, Ai->getOperand(1),
                                     "gen", &*Ai);
              break;
            }
          }
        }

        // Condition (Int)
        if (Opcode == Instruction::ICmp) {
          ICmpInst *CurrentI = (ICmpInst *)(&Ai);
          switch (llvm::SharedCryptoUtils->get_range(3)) {
          case 0:
            break;
          case 1:
            CurrentI->swapOperands();
            break;
          case 2: // randomly change the predicate
            switch (llvm::SharedCryptoUtils->get_range(10)) {
            case 0:
              CurrentI->setPredicate(ICmpInst::ICMP_EQ);
              break; // equal
            case 1:
              CurrentI->setPredicate(ICmpInst::ICMP_NE);
              break; // not equal
            case 2:
              CurrentI->setPredicate(ICmpInst::ICMP_UGT);
              break; // unsigned greater than
            case 3:
              CurrentI->setPredicate(ICmpInst::ICMP_UGE);
              break; // unsigned greater or equal
            case 4:
              CurrentI->setPredicate(ICmpInst::ICMP_ULT);
              break; // unsigned less than
            case 5:
              CurrentI->setPredicate(ICmpInst::ICMP_ULE);
              break; // unsigned less or equal
            case 6:
              CurrentI->setPredicate(ICmpInst::ICMP_SGT);
              break; // signed greater than
            case 7:
              CurrentI->setPredicate(ICmpInst::ICMP_SGE);
              break; // signed greater or equal
            case 8:
              CurrentI->setPredicate(ICmpInst::ICMP_SLT);
              break; // signed less than
            case 9:
              CurrentI->setPredicate(ICmpInst::ICMP_SLE);
              break; // signed less or equal
            }
            break;
          }
        }

        // Condition (Float)
        if (Opcode == Instruction::FCmp) {
          FCmpInst *CurrentI = (FCmpInst *)(&Ai);
          switch (llvm::SharedCryptoUtils->get_range(3)) {
          case 0:
            break;
          case 1:
            CurrentI->swapOperands();
            break;
          case 2: // randomly change the predicate
            switch (llvm::SharedCryptoUtils->get_range(10)) {
            case 0:
              CurrentI->setPredicate(FCmpInst::FCMP_OEQ);
              break; // ordered and equal
            case 1:
              CurrentI->setPredicate(FCmpInst::FCMP_ONE);
              break; // ordered and operands are unequal
            case 2:
              CurrentI->setPredicate(FCmpInst::FCMP_UGT);
              break; // unordered or greater than
            case 3:
              CurrentI->setPredicate(FCmpInst::FCMP_UGE);
              break; // unordered, or greater than, or equal
            case 4:
              CurrentI->setPredicate(FCmpInst::FCMP_ULT);
              break; // unordered or less than
            case 5:
              CurrentI->setPredicate(FCmpInst::FCMP_ULE);
              break; // unordered, or less than, or equal
            case 6:
              CurrentI->setPredicate(FCmpInst::FCMP_OGT);
              break; // ordered and greater than
            case 7:
              CurrentI->setPredicate(FCmpInst::FCMP_OGE);
              break; // ordered and greater than or equal
            case 8:
              CurrentI->setPredicate(FCmpInst::FCMP_OLT);
              break; // ordered and less than
            case 9:
              CurrentI->setPredicate(FCmpInst::FCMP_OLE);
              break; // ordered or less than, or equal
            }
            break;
          }
        }
      }
    }

    // Remove DIs from AlteredBB
    std::vector<CallInst *> InstToRemove;
    std::vector<Constant *> DeadConstants;
    for (Instruction &Ai : *AlteredBB) {
      if (CallInst *CI = dyn_cast<CallInst>(&Ai)) {
        if (CI->getCalledFunction() != nullptr &&
            CI->getCalledFunction()->getName().startswith("llvm.dbg")) {
          InstToRemove.push_back(CI);
        }
      }
    }

    // Shamefully stolen from IPO/StripSymbols.cpp
    for (CallInst *CI : InstToRemove) {
      Value *Arg1 = CI->getArgOperand(0);
      Value *Arg2 = CI->getArgOperand(1);
      assert(CI->use_empty() && "llvm.dbg intrinsic should have void result");
      CI->eraseFromParent();
      if (Arg1->use_empty()) {
        if (Constant *C = dyn_cast<Constant>(Arg1)) {
          DeadConstants.push_back(C);
        } else {
          RecursivelyDeleteTriviallyDeadInstructions(Arg1);
        }
      }
      if (Arg2->use_empty()) {
        if (Constant *C = dyn_cast<Constant>(Arg2)) {
          DeadConstants.push_back(C);
        }
      }
    }

    // Remove dead constants
    while (!DeadConstants.empty()) {
      Constant *C = DeadConstants.back();
      DeadConstants.pop_back();
      if (GlobalVariable *GV = dyn_cast<GlobalVariable>(C)) {
        if (GV->hasLocalLinkage())
          removeDeadConstant(GV);
      } else
        removeDeadConstant(C);
    }

    return AlteredBB;
  }

  /* Overwrite FunctionPass method to apply the transformations to the whole
   * module. This part obfuscate all the always true predicates of the module.
   * More precisely, the condition which predicate is FCMP_TRUE.
   * It also removes all the functions' basic blocks' and instructions' names.
   */
  bool doFinalization(Module &M) override {

    // In this part we extract all always-true predicate and replace them with
    // opaque predicate: For this, we declare two global values: x and y, and
    // replace the FCMP_TRUE predicate with (y < 10 || x * (x + 1) % 2 == 0).
    // A better way to obfuscate the predicates would be welcome.
    // In the meantime we will erase the name of the basic blocks,
    // the instructions and the functions.
    DEBUG_WITH_TYPE(DEBUG_TYPE "-gen",
                    errs() << "Starting doFinalization()...\n");

    std::vector<Instruction *> InstToEdit, InstToDelete;

    // Looking for the conditions and branches to transform
    for (Module::iterator Mi = M.begin(), Me = M.end(); Mi != Me; ++Mi) {
      for (Function::iterator Fi = Mi->begin(), Fe = Mi->end(); Fi != Fe;
           ++Fi) {
        Instruction *InstTerm = Fi->getTerminator();
        if (InstTerm != nullptr && InstTerm->getOpcode() == Instruction::Br) {
          BranchInst *InstBr = (BranchInst *)(InstTerm);
          if (InstBr->isConditional()) {
            ICmpInst *InstCmp = (ICmpInst *)InstBr->getCondition();
            unsigned Opcode = InstCmp->getOpcode();
            if (Opcode == Instruction::ICmp) {
              if (InstCmp->getPredicate() == ICmpInst::ICMP_EQ &&
                  InstCmp->getName().startswith("BCFPlaceholderPred")) {
                DEBUG_WITH_TYPE(DEBUG_TYPE "-gen",
                                errs() << "    Found an always true predicate\n");
                InstToDelete.push_back(InstCmp); // The condition
                InstToEdit.push_back(InstTerm); // The branch using the condition
              }
            }
          }
        }
      }
    }

    // Replacing all the branches we found
    for (std::vector<Instruction *>::iterator Ii = InstToEdit.begin(),
                                              Ie = InstToEdit.end();
         Ii != Ie; ++Ii)
    {
      // Previously We Use LLVM EE To Calculate LHS and RHS.
      // Since IRBuilder<> uses ConstantFolding to fold constants.
      // The return instruction is already returning constants
      // The variable names below are the artifact from the Emulation Era.
      Type *I32Ty = Type::getInt32Ty(M.getContext());
      Module EmuModule("HikariBCFEmulator", M.getContext());
      EmuModule.setDataLayout(M.getDataLayout());
      EmuModule.setTargetTriple(M.getTargetTriple());
      Function *EmuFunction =
        Function::Create(FunctionType::get(I32Ty, false),
                         GlobalValue::LinkageTypes::PrivateLinkage,
                         "BeginExecution", &EmuModule);
      BasicBlock *EntryBlock =
        BasicBlock::Create(M.getContext(), "", EmuFunction);

      Instruction *TmpInst = &*((*Ii)->getParent()->getFirstInsertionPt());
      IRBuilder<> IRBReal(TmpInst);
      IRBuilder<> IRBEmu(EntryBlock);

      // First, construct a real RHS that will be used in the actual condition
      Constant *RealRHS = ConstantInt::get(I32Ty, llvm::SharedCryptoUtils->get_uint32_t());

      // Prepare initial LHS and RHS to bootstrap the emulator
      Constant *LHSC = ConstantInt::get(I32Ty, llvm::SharedCryptoUtils->get_uint32_t());
      Constant *RHSC = ConstantInt::get(I32Ty, llvm::SharedCryptoUtils->get_uint32_t());
      GlobalVariable *LHSGV =
        new GlobalVariable(M, Type::getInt32Ty(M.getContext()), false,
                           GlobalValue::PrivateLinkage, LHSC, "LHSGV");
      GlobalVariable *RHSGV =
        new GlobalVariable(M, Type::getInt32Ty(M.getContext()), false,
                           GlobalValue::PrivateLinkage, RHSC, "RHSGV");
      LoadInst *LHS = IRBReal.CreateLoad(LHSGV, "Initial LHS");
      LoadInst *RHS = IRBReal.CreateLoad(RHSGV, "Initial RHS");

      // To speed up evaluation
      Value *EmuLHS = LHSC;
      Value *EmuRHS = RHSC;
      Instruction::BinaryOps InitialOp = Ops[llvm::SharedCryptoUtils->get_uint32_t() %
                                             (sizeof(Ops) / sizeof(Ops[0]))];
      Value *EmuLast =
        IRBEmu.CreateBinOp(InitialOp, EmuLHS, EmuRHS, "EmuInitialCondition");
      Value *Last =
        IRBReal.CreateBinOp(InitialOp, LHS, RHS, "InitialCondition");
      for (unsigned Ei = 0; Ei < ConditionExpressionComplexity; Ei++) {
        Constant *NewTmpConst = ConstantInt::get(I32Ty, llvm::SharedCryptoUtils->get_uint32_t());
        Instruction::BinaryOps ComplexInitialOp =
          Ops[llvm::SharedCryptoUtils->get_uint32_t() %
              (sizeof(Ops) / sizeof(Ops[0]))];
        EmuLast = IRBEmu.CreateBinOp(ComplexInitialOp, EmuLast, NewTmpConst,
                                     "EmuInitialCondition");
        Last = IRBReal.CreateBinOp(ComplexInitialOp, Last, NewTmpConst, "InitialCondition");
      }

      // Randomly Generate Predicate
      CmpInst::Predicate Pred = Preds[llvm::SharedCryptoUtils->get_uint32_t() %
                                      (sizeof(Preds) / sizeof(Preds[0]))];
      Last = IRBReal.CreateICmp(Pred, Last, RealRHS);
      EmuLast = IRBEmu.CreateICmp(Pred, EmuLast, RealRHS);
      ReturnInst *RI = IRBEmu.CreateRet(EmuLast);
      ConstantInt *EmuCI = cast<ConstantInt>(RI->getReturnValue());
      uint64_t EmulateResult = EmuCI->getZExtValue();

      // Start to prepare IndirectBranching
      std::vector<BasicBlock *> BBs;
      if (EmulateResult == 1) {
        // ConstantExpr evaluates to true
        BranchInst::Create(((BranchInst *)*Ii)->getSuccessor(0),
                           ((BranchInst *)*Ii)->getSuccessor(1), (Value *)Last,
                           ((BranchInst *)*Ii)->getParent());
      } else {
        // False, swap operands
        BranchInst::Create(((BranchInst *)*Ii)->getSuccessor(1),
                           ((BranchInst *)*Ii)->getSuccessor(0), (Value *)Last,
                           ((BranchInst *)*Ii)->getParent());
      }

      EntryBlock->eraseFromParent();
      EmuFunction->eraseFromParent();
      DEBUG_WITH_TYPE(DEBUG_TYPE "-gen",
                      errs() << "    Erase branch instruction: " << *((BranchInst *)*Ii) << "\n");
      (*Ii)->eraseFromParent(); // erase the branch
    }

    // Erase all the associated conditions we found
    for (std::vector<Instruction *>::iterator Ii = InstToDelete.begin(),
                                              Ie = InstToDelete.end();
         Ii != Ie; ++Ii)
    {
      DEBUG_WITH_TYPE(DEBUG_TYPE "-gen",
                      errs() << "    Erase condition instruction: " << *((Instruction *)*Ii) << "\n");
      (*Ii)->eraseFromParent();
    }

    // Only for debug
    DEBUG_WITH_TYPE(DEBUG_TYPE "-cfg",
                    errs() << "    END doFinalization()\n");

    return true;
  }
};

} // namespace

char BogusControlFlow::ID = 0;
static RegisterPass<BogusControlFlow>
  X(DEBUG_TYPE, "Enable Bogus Control Flow (BCF) obfuscation");