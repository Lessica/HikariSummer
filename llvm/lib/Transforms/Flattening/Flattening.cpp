#include "llvm/ADT/Statistic.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Pass.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Transforms/CryptoUtils.h"
#include "llvm/Transforms/Utils.h"
#include "llvm/Transforms/Utils/Local.h"

using namespace llvm;

#define DEBUG_TYPE "obfs-cff"

STATISTIC(FlattenedFunctions, "Number of functions flattened");

namespace {

struct Flattening : public FunctionPass {
  static char ID; // Pass identification, replacement for typeid
  Flattening() : FunctionPass(ID) { llvm::SharedCryptoUtils->prng_seed(); }

  bool runOnFunction(Function &F) override { return doFlattening(&F); }

  bool doFlattening(Function *F) {
    std::vector<BasicBlock *> OrigBBs;

    BasicBlock *LoopEntryBB;
    BasicBlock *LoopEndBB;
    LoadInst *LoadI;
    SwitchInst *SwitchI;
    AllocaInst *SwitchVar;

    std::map<uint32_t, uint32_t> ScramblingKey;

    legacy::FunctionPassManager FPM(F->getParent());
    FPM.add(createLowerSwitchPass());
    FPM.run(*F);

    // Save all original basic blocks
    for (Function::iterator Bi = F->begin(), Be = F->end(); Bi != Be; ++Bi) {
      BasicBlock *BB = &*Bi;
      if (BB->isEHPad() || BB->isLandingPad()) {
        errs() << "Function " << F->getName() << " "
               << "contains exception handling instructions and is unsupported "
                  "for flattening in the open-source version of "
                  "HikariObfuscator.\n";
        return false;
      }
      if (!isa<BranchInst>(BB->getTerminator()) &&
          !isa<ReturnInst>(BB->getTerminator())) {
        return false;
      }
      OrigBBs.push_back(BB);
    }

    // Noting to do if there is only one basic block
    if (OrigBBs.size() <= 1) {
      return false;
    }

    FlattenedFunctions++;

    // Remove the first BB
    OrigBBs.erase(OrigBBs.begin());

    // Get a pointer to the first BB
    Function::iterator TmpI = F->begin();
    BasicBlock *InsertBB = &*TmpI;

    // If main begins with a branch
    BranchInst *BranchI = NULL;
    if (isa<BranchInst>(InsertBB->getTerminator())) {
      BranchI = cast<BranchInst>(InsertBB->getTerminator());
    }

    // Create a new basic block to insert before the first BB
    if ((BranchI != NULL && BranchI->isConditional()) ||
        InsertBB->getTerminator()->getNumSuccessors() > 1) {
      BasicBlock::iterator Bi = InsertBB->end();
      --Bi;

      if (InsertBB->size() > 1) {
        --Bi;
      }

      BasicBlock *TmpBB = InsertBB->splitBasicBlock(Bi, "ObfsCFFFirstBB");
      OrigBBs.insert(OrigBBs.begin(), TmpBB);
    }

    // Remove jump to the first BB
    Instruction *OldTerm = InsertBB->getTerminator();

    // Create switch variable and set as it
    SwitchVar = new AllocaInst(Type::getInt32Ty(F->getContext()), 0,
                               "ObfsCFFSwitchVar", OldTerm);
    OldTerm->eraseFromParent();

    new StoreInst(
      ConstantInt::get(Type::getInt32Ty(F->getContext()),
                       llvm::SharedCryptoUtils->scramble32(0, ScramblingKey)),
      SwitchVar, InsertBB);

    // Create main loop entry
    LoopEntryBB = BasicBlock::Create(F->getContext(), "ObfsCFFLoopEntry", F, InsertBB);
    LoopEndBB = BasicBlock::Create(F->getContext(), "ObfsCFFLoopEnd", F, InsertBB);
    LoadI = new LoadInst(SwitchVar->getType()->getElementType(), SwitchVar,
                         "ObfsCFFSwitchVar", LoopEntryBB);

    // Move first BB on top of the loop entry
    InsertBB->moveBefore(LoopEntryBB);
    BranchInst::Create(LoopEntryBB, InsertBB);

    // Jump from the loop end to the loop entry
    BranchInst::Create(LoopEntryBB, LoopEndBB);

    // Create switch BB
    BasicBlock *SwitchDefaultBB =
      BasicBlock::Create(F->getContext(), "ObfsCFFSwitchDefault", F, LoopEndBB);
    BranchInst::Create(LoopEndBB, SwitchDefaultBB);

    // Create switch instruction
    SwitchI = SwitchInst::Create(&*F->begin(), SwitchDefaultBB, 0, LoopEntryBB);
    SwitchI->setCondition(LoadI);

    // Remove branch jump from the first BB and make a jump to the main loop entry
    F->begin()->getTerminator()->eraseFromParent();
    BranchInst::Create(LoopEntryBB, &*F->begin());

    // Put all BB into the switch
    for (std::vector<BasicBlock *>::iterator Bi = OrigBBs.begin(),
                                             Be = OrigBBs.end();
         Bi != Be; ++Bi) {
      BasicBlock *BB = *Bi;
      ConstantInt *NumCase = NULL;

      // Move the BB inside the switch (only visual, no code logic)
      BB->moveBefore(LoopEndBB);

      // Add case to switch
      NumCase = cast<ConstantInt>(
        ConstantInt::get(SwitchI->getCondition()->getType(),
                         llvm::SharedCryptoUtils->scramble32(
                           SwitchI->getNumCases(), ScramblingKey)));
      SwitchI->addCase(NumCase, BB);
    }

    // Re-calculate the SwitchVar
    for (std::vector<BasicBlock *>::iterator Bi = OrigBBs.begin(),
                                             Be = OrigBBs.end();
         Bi != Be; ++Bi) {
      BasicBlock *BB = *Bi;
      ConstantInt *NumCase = NULL;

      // Return BB
      if (BB->getTerminator()->getNumSuccessors() == 0) {
        continue;
      }

      // If it is a non-conditional jump
      if (BB->getTerminator()->getNumSuccessors() == 1) {

        // Get successor and delete terminator
        BasicBlock *SuccBB = BB->getTerminator()->getSuccessor(0);
        BB->getTerminator()->eraseFromParent();

        // Get next case
        NumCase = SwitchI->findCaseDest(SuccBB);

        // If next case is the default case
        if (NumCase == NULL) {
          NumCase = cast<ConstantInt>(
            ConstantInt::get(SwitchI->getCondition()->getType(),
                             llvm::SharedCryptoUtils->scramble32(
                               SwitchI->getNumCases() - 1, ScramblingKey)));
        }

        // Update SwitchVar and jump to the end of the loop
        new StoreInst(NumCase, LoadI->getPointerOperand(), BB);
        BranchInst::Create(LoopEndBB, BB);
        continue;
      }

      // If it is a conditional jump
      if (BB->getTerminator()->getNumSuccessors() == 2) {

        // Get next cases
        ConstantInt *NumCaseTrue =
          SwitchI->findCaseDest(BB->getTerminator()->getSuccessor(0));
        ConstantInt *NumCaseFalse =
          SwitchI->findCaseDest(BB->getTerminator()->getSuccessor(1));

        // Check if next cases are the default cases
        if (NumCaseTrue == NULL) {
          NumCaseTrue = cast<ConstantInt>(
            ConstantInt::get(SwitchI->getCondition()->getType(),
                             llvm::SharedCryptoUtils->scramble32(
                               SwitchI->getNumCases() - 1, ScramblingKey)));
        }
        if (NumCaseFalse == NULL) {
          NumCaseFalse = cast<ConstantInt>(
            ConstantInt::get(SwitchI->getCondition()->getType(),
                             llvm::SharedCryptoUtils->scramble32(
                               SwitchI->getNumCases() - 1, ScramblingKey)));
        }

        // Create a select instruction
        BranchInst *SelBranchI = cast<BranchInst>(BB->getTerminator());
        SelectInst *SelI =
          SelectInst::Create(SelBranchI->getCondition(), NumCaseTrue,
                             NumCaseFalse, "", BB->getTerminator());

        // Erase the old terminator
        BB->getTerminator()->eraseFromParent();

        // Update SwitchVar and jump to the end of the loop
        new StoreInst(SelI, LoadI->getPointerOperand(), BB);
        BranchInst::Create(LoopEndBB, BB);
        continue;
      }
    }

    fixStack(F);
    return true;
  }

  // Shamefully borrowed from Scalar/Reg2Mem.cpp :(
  bool valueEscapes(const Instruction *Inst) {
    const BasicBlock *BB = Inst->getParent();
    for (const User *U : Inst->users()) {
      const Instruction *UI = cast<Instruction>(U);
      if (UI->getParent() != BB || isa<PHINode>(UI))
        return true;
    }
    return false;
  }

  // Remove PHI node and demote reg to stack
  void fixStack(Function *F) {
    std::vector<PHINode *> TmpPhi;
    std::vector<Instruction *> TmpReg;
    BasicBlock *EntryBB = &*F->begin();
    do {
      TmpPhi.clear();
      TmpReg.clear();
      for (Function::iterator Fi = F->begin(), Fe = F->end(); Fi != Fe; ++Fi) {
        for (BasicBlock::iterator Bj = Fi->begin(), Be = Fi->end(); Bj != Be;
             ++Bj) {
          if (isa<PHINode>(Bj)) {
            PHINode *Phi = cast<PHINode>(Bj);
            TmpPhi.push_back(Phi);
            continue;
          }
          if (!(isa<AllocaInst>(Bj) && Bj->getParent() == EntryBB) &&
              (valueEscapes(&*Bj) || Bj->isUsedOutsideOfBlock(&*Fi))) {
            TmpReg.push_back(&*Bj);
            continue;
          }
        }
      }
      for (unsigned Ti = 0; Ti != TmpReg.size(); ++Ti) {
        DemoteRegToStack(*TmpReg.at(Ti), F->begin()->getTerminator());
      }
      for (unsigned Ti = 0; Ti != TmpPhi.size(); ++Ti) {
        DemotePHIToStack(TmpPhi.at(Ti), F->begin()->getTerminator());
      }
    } while (TmpReg.size() != 0 || TmpPhi.size() != 0);
  }
};

} // namespace

char Flattening::ID = 0;
static RegisterPass<Flattening>
  X(DEBUG_TYPE,
    "Enable Control Flow Flattening (CFF/FLA) obfuscation");