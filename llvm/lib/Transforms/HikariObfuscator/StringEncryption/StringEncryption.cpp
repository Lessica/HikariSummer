#include "../CryptoUtils/CryptoUtils.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/NoFolder.h"
#include "llvm/Pass.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Transforms/Utils/ModuleUtils.h"
#include <map>
#include <set>
#include <string>
#include <vector>

using namespace llvm;

#define DEBUG_TYPE "obfs-strcry"

static cl::opt<unsigned> MaximumStringLength(
  DEBUG_TYPE "-max-strlen", cl::NotHidden,
  cl::desc("Maximum string length in bytes of the section which "
           "will be obfuscated by the obfs-strcry pass"),
  cl::init(1024), cl::Optional);

STATISTIC(EncryptedCStringCounter, "Number of encrypted C strings");
STATISTIC(EncryptedObjCStringCounter, "Number of encrypted ObjC strings");

namespace {

struct StringEncryption : public ModulePass {
  static char ID; // Pass identification, replacement for typeid
  std::map<Function * /* Function */, GlobalVariable * /* Decryption Status */>
    EncryptionStatus;
  StringEncryption() : ModulePass(ID) {}

  bool runOnModule(Module &M) override {
    bool ModuleModified = false;
    for (Function &F : M) {
      Constant *S = ConstantInt::get(Type::getInt32Ty(M.getContext()), 0);
      GlobalVariable *GV =
        new GlobalVariable(M, S->getType(), false,
                           GlobalValue::LinkageTypes::PrivateLinkage, S, "");
      EncryptionStatus[&F] = GV;
      ModuleModified = handleFunction(&F) || ModuleModified;
    }
    return ModuleModified;
  }

  bool handleFunction(Function *F) {

    fixFunctionConstantExpr(F);

    std::set<GlobalVariable *> Globals;
    std::set<User *> Users;

    for (BasicBlock &BB : *F) {
      for (Instruction &I : BB) {
        for (Value *Op : I.operands()) {
          if (GlobalVariable *G =
                dyn_cast<GlobalVariable>(Op->stripPointerCasts())) {
            if (User *U = dyn_cast<User>(Op)) {
              Users.insert(U);
            }
            Users.insert(&I);
            Globals.insert(G);
          }
        }
      }
    }

    std::set<GlobalVariable *> RawStrings;
    std::set<GlobalVariable *> ObjCStrings;
    std::map<GlobalVariable *, std::pair<Constant *, GlobalVariable *>> GV2Keys;
    std::map<GlobalVariable * /* old */,
             std::pair<GlobalVariable * /* encrypted */,
                       GlobalVariable * /* decrypt space */>>
      Old2New;

    for (GlobalVariable *GV : Globals) {
      if (GV->hasInitializer() &&
          GV->getSection() != StringRef("llvm.metadata") &&
          GV->getSection().find(StringRef("__objc")) == std::string::npos &&
          GV->getName().find("OBJC") == std::string::npos) {
        if (GV->getInitializer()->getType() ==
            StructType::getTypeByName(F->getContext(),
                                      "struct.__NSConstantString_tag")) {
          ObjCStrings.insert(GV);
          RawStrings.insert(
            cast<GlobalVariable>(cast<ConstantStruct>(GV->getInitializer())
                                   ->getOperand(2)
                                   ->stripPointerCasts()));

        } else if (isa<ConstantDataSequential>(GV->getInitializer())) {
          RawStrings.insert(GV);
        } else if (isa<ConstantArray>(GV->getInitializer())) {
          ConstantArray *CA = cast<ConstantArray>(GV->getInitializer());
          for (unsigned Oi = 0; Oi < CA->getNumOperands(); Oi++) {
            Value *Op = CA->getOperand(Oi)->stripPointerCasts();
            if (GlobalVariable *OpGV = dyn_cast<GlobalVariable>(Op)) {
              Globals.insert(OpGV);
            }
          }
        }
      }
    }

    for (GlobalVariable *GV : RawStrings) {

      if (GV->getInitializer()->isZeroValue() ||
          GV->getInitializer()->isNullValue()) {
        continue;
      }

      ConstantDataSequential *CDS =
        cast<ConstantDataSequential>(GV->getInitializer());
      Type *MemberTy = CDS->getElementType();

      // Ignore non-integer type
      if (!isa<IntegerType>(MemberTy)) {
        continue;
      }

      IntegerType *IntTy = cast<IntegerType>(MemberTy);
      Constant *KeyConst = NULL;
      Constant *EncryptedConst = NULL;
      Constant *DummyConst = NULL;

      if (IntTy == Type::getInt8Ty(GV->getParent()->getContext())) {

        if (CDS->getNumElements() * sizeof(uint8_t) > MaximumStringLength) {
          continue;
        }

        std::vector<uint8_t> Keys;
        std::vector<uint8_t> Encry;
        std::vector<uint8_t> Dummy;

        for (unsigned Ci = 0; Ci < CDS->getNumElements(); Ci++) {
          uint8_t K = llvm::SharedCryptoUtils->get_uint8_t();
          uint64_t V = CDS->getElementAsInteger(Ci);
          Keys.push_back(K);
          Encry.push_back(K ^ V);
          Dummy.push_back(rand());
        }

        KeyConst = ConstantDataArray::get(GV->getParent()->getContext(),
                                          ArrayRef<uint8_t>(Keys));
        EncryptedConst = ConstantDataArray::get(GV->getParent()->getContext(),
                                                ArrayRef<uint8_t>(Encry));
        DummyConst = ConstantDataArray::get(GV->getParent()->getContext(),
                                            ArrayRef<uint8_t>(Dummy));

      } else if (IntTy == Type::getInt16Ty(GV->getParent()->getContext())) {

        if (CDS->getNumElements() * sizeof(uint16_t) > MaximumStringLength) {
          continue;
        }

        std::vector<uint16_t> Keys;
        std::vector<uint16_t> Encry;
        std::vector<uint16_t> Dummy;

        for (unsigned Ci = 0; Ci < CDS->getNumElements(); Ci++) {
          uint16_t K = llvm::SharedCryptoUtils->get_uint16_t();
          uint64_t V = CDS->getElementAsInteger(Ci);
          Keys.push_back(K);
          Encry.push_back(K ^ V);
          Dummy.push_back(rand());
        }

        KeyConst = ConstantDataArray::get(GV->getParent()->getContext(),
                                          ArrayRef<uint16_t>(Keys));
        EncryptedConst = ConstantDataArray::get(GV->getParent()->getContext(),
                                                ArrayRef<uint16_t>(Encry));
        DummyConst = ConstantDataArray::get(GV->getParent()->getContext(),
                                            ArrayRef<uint16_t>(Dummy));

      } else if (IntTy == Type::getInt32Ty(GV->getParent()->getContext())) {

        if (CDS->getNumElements() * sizeof(uint32_t) > MaximumStringLength) {
          continue;
        }

        std::vector<uint32_t> Keys;
        std::vector<uint32_t> Encry;
        std::vector<uint32_t> Dummy;

        for (unsigned Ci = 0; Ci < CDS->getNumElements(); Ci++) {
          uint32_t K = llvm::SharedCryptoUtils->get_uint32_t();
          uint64_t V = CDS->getElementAsInteger(Ci);
          Keys.push_back(K);
          Encry.push_back(K ^ V);
          Dummy.push_back(rand());
        }

        KeyConst = ConstantDataArray::get(GV->getParent()->getContext(),
                                          ArrayRef<uint32_t>(Keys));
        EncryptedConst = ConstantDataArray::get(GV->getParent()->getContext(),
                                                ArrayRef<uint32_t>(Encry));
        DummyConst = ConstantDataArray::get(GV->getParent()->getContext(),
                                            ArrayRef<uint32_t>(Dummy));

      } else if (IntTy == Type::getInt64Ty(GV->getParent()->getContext())) {

        if (CDS->getNumElements() * sizeof(uint64_t) > MaximumStringLength) {
          continue;
        }

        std::vector<uint64_t> Keys;
        std::vector<uint64_t> Encry;
        std::vector<uint64_t> Dummy;

        for (unsigned Ci = 0; Ci < CDS->getNumElements(); Ci++) {
          uint64_t K = llvm::SharedCryptoUtils->get_uint64_t();
          uint64_t V = CDS->getElementAsInteger(Ci);
          Keys.push_back(K);
          Encry.push_back(K ^ V);
          Dummy.push_back(rand());
        }

        KeyConst = ConstantDataArray::get(GV->getParent()->getContext(),
                                          ArrayRef<uint64_t>(Keys));
        EncryptedConst = ConstantDataArray::get(GV->getParent()->getContext(),
                                                ArrayRef<uint64_t>(Encry));
        DummyConst = ConstantDataArray::get(GV->getParent()->getContext(),
                                            ArrayRef<uint64_t>(Dummy));
      } else {
        errs() << "Unsupported CDS Type\n";
        abort();
      }

      // Prepare new RawGV
      GlobalVariable *EncryptedRawGV = new GlobalVariable(
        *(GV->getParent()), EncryptedConst->getType(), false, GV->getLinkage(),
        EncryptedConst, "ObfsEncryptedString", nullptr,
        GV->getThreadLocalMode(), GV->getType()->getAddressSpace());
      GlobalVariable *DecryptSpaceGV = new GlobalVariable(
        *(GV->getParent()), DummyConst->getType(), false, GV->getLinkage(),
        DummyConst, "ObfsDecryptSpace", nullptr, GV->getThreadLocalMode(),
        GV->getType()->getAddressSpace());

      Old2New[GV] = std::make_pair(EncryptedRawGV, DecryptSpaceGV);
      GV2Keys[DecryptSpaceGV] = std::make_pair(KeyConst, EncryptedRawGV);
    }

    // Now prepare new ObjC GV
    for (GlobalVariable *GV : ObjCStrings) {
      ConstantStruct *CS = cast<ConstantStruct>(GV->getInitializer());
      GlobalVariable *OldRawString =
        cast<GlobalVariable>(CS->getOperand(2)->stripPointerCasts());

      // Filter out zero initializers
      if (Old2New.find(OldRawString) == Old2New.end()) {
        continue;
      }

      GlobalVariable *EncryptedOCGV = getObjCString(
        GV, "ObfsEncryptedStringObjC", Old2New[OldRawString].first, CS);
      GlobalVariable *DecryptSpaceOCGV = getObjCString(
        GV, "ObfsDecryptSpaceObjC", Old2New[OldRawString].second, CS);
      Old2New[GV] = std::make_pair(EncryptedOCGV, DecryptSpaceOCGV);
    }

    if (Old2New.empty() || GV2Keys.empty())
      return false;

    EncryptedObjCStringCounter += ObjCStrings.size();
    EncryptedCStringCounter += RawStrings.size();

    // Replace usages
    for (User *U : Users) {
      for (std::map<GlobalVariable *,
                    std::pair<GlobalVariable *, GlobalVariable *>>::iterator
             Mi = Old2New.begin(),
             Me = Old2New.end();
           Mi != Me; ++Mi) {
        U->replaceUsesOfWith(Mi->first, Mi->second.second);
        Mi->first->removeDeadConstantUsers();
      }
    }

    // Clean up Old ObjC GVs
    for (GlobalVariable *GV : ObjCStrings) {
      if (GV->getNumUses() == 0) {
        GV->dropAllReferences();
        Old2New.erase(GV);
        GV->eraseFromParent();
      }
    }

    // Clean up Old Raw GVs
    for (std::map<GlobalVariable *,
                  std::pair<GlobalVariable *, GlobalVariable *>>::iterator
           Mi = Old2New.begin(),
           Me = Old2New.end();
         Mi != Me; ++Mi) {
      GlobalVariable *GVToDelete = Mi->first;
      GVToDelete->removeDeadConstantUsers();
      if (GVToDelete->getNumUses() == 0) {
        GVToDelete->dropAllReferences();
        GVToDelete->eraseFromParent();
      }
    }

    GlobalVariable *StatusGV = EncryptionStatus[F];

    /*
      - Split Original EntryPoint BB into EntryBB and PrecedBB.
      - Create new BB as Decryption BB between EntryBB and PrecedBB. Adjust the
      terminators into: EntryBB (Alloca a new array containing all) -> DecryptBB
      (If not decrypted) -> PrecedBB
    */
    BasicBlock *EntryBB = &(F->getEntryBlock());
    BasicBlock *PrecedBB =
      EntryBB->splitBasicBlock(EntryBB->getFirstNonPHIOrDbgOrLifetime());
    PrecedBB->setName("ObfsPrecedingBlock");

    BasicBlock *DecryptBB = BasicBlock::Create(
      F->getContext(), "ObfsStringDecryptionBB", F, PrecedBB);

    // Change EntryBB's terminator to jump to DecryptBB
    // We'll add new terminator to jump PrecedBB later
    BranchInst *NewBrI = BranchInst::Create(DecryptBB);
    ReplaceInstWithInst(EntryBB->getTerminator(), NewBrI);
    IRBuilder<> IRB(EntryBB->getFirstNonPHIOrDbgOrLifetime());

    // Insert decryption block
    handleDecryptionBlock(DecryptBB, PrecedBB, GV2Keys);

    // Add atomic load checking status in EntryBB
    LoadInst *LoadI = IRB.CreateLoad(StatusGV, "ObfsLoadEncryptionStatus");

    // Will be released at the start of PrecedBB
    LoadI->setAtomic(AtomicOrdering::Acquire);
    LoadI->setAlignment(Align(4));
    Value *ConditionVal = IRB.CreateICmpEQ(
      LoadI, ConstantInt::get(Type::getInt32Ty(F->getContext()), 0));
    EntryBB->getTerminator()->eraseFromParent();
    BranchInst::Create(DecryptBB, PrecedBB, ConditionVal, EntryBB);

    // Add StoreInst atomically in PrecedBB start.
    // No matter control flow is coming from EntryBB or DecryptBB, the GVs must
    // be decrypted.
    IRBuilder<> IRBC(PrecedBB->getFirstNonPHIOrDbgOrLifetime());
    StoreInst *StoreI = IRBC.CreateStore(
      ConstantInt::get(Type::getInt32Ty(F->getContext()), 1), StatusGV);
    StoreI->setAlignment(Align(4));

    // Release the lock acquired in LoadI
    StoreI->setAtomic(AtomicOrdering::Release);

    return true;
  }

  // Replace ConstantExpr with equal instructions, otherwise replacing on
  // Constant will crash the compiler.
  void fixFunctionConstantExpr(Function *F) {
    for (BasicBlock &BB : *F) {
      fixBasicBlockConstantExpr(&BB);
    }
  }

  // PHIs must be placed at BB start so CEs must be placed prior to current BB.
  void fixBasicBlockConstantExpr(BasicBlock *BB) {

    assert(!BB->empty() && "BasicBlock is empty!");
    assert((BB->getParent() != NULL) &&
           "BasicBlock must be inside a Function!");

    Instruction *FunctionInsertPt =
      &*(BB->getParent()->getEntryBlock().getFirstInsertionPt());

    for (Instruction &I : *BB) {
      if (isa<LandingPadInst>(I) || isa<FuncletPadInst>(I)) {
        continue;
      }
      for (unsigned Oi = 0; Oi < I.getNumOperands(); Oi++) {
        if (ConstantExpr *C = dyn_cast<ConstantExpr>(I.getOperand(Oi))) {
          Instruction *InsertPt = &I;
          IRBuilder<NoFolder> IRB(InsertPt);
          if (isa<PHINode>(I)) {
            IRB.SetInsertPoint(FunctionInsertPt);
          }
          Instruction *Inst = IRB.Insert(C->getAsInstruction());
          I.setOperand(Oi, Inst);
        }
      }
    }
  }

  GlobalVariable *getObjCString(GlobalVariable *GV, std::string Name,
                                GlobalVariable *NewString, ConstantStruct *CS) {
    Value *ZeroVal = ConstantInt::get(Type::getInt32Ty(GV->getContext()), 0);
    std::vector<Constant *> Vals;
    Vals.push_back(CS->getOperand(0));
    Vals.push_back(CS->getOperand(1));

    Constant *GEPed = ConstantExpr::getInBoundsGetElementPtr(
      nullptr, NewString, {ZeroVal, ZeroVal});
    if (GEPed->getType() == CS->getOperand(2)->getType()) {
      Vals.push_back(GEPed);
    } else {
      Constant *BitCasted =
        ConstantExpr::getBitCast(NewString, CS->getOperand(2)->getType());
      Vals.push_back(BitCasted);
    }
    Vals.push_back(CS->getOperand(3));

    Constant *NewCs =
      ConstantStruct::get(CS->getType(), ArrayRef<Constant *>(Vals));
    return new GlobalVariable(*(GV->getParent()), NewCs->getType(), false,
                              GV->getLinkage(), NewCs, Name.c_str(), nullptr,
                              GV->getThreadLocalMode(),
                              GV->getType()->getAddressSpace());
  }

  void handleDecryptionBlock(
    BasicBlock *BB, BasicBlock *DestBB,
    std::map<GlobalVariable *, std::pair<Constant *, GlobalVariable *>>
      &GV2Keys) {

    IRBuilder<> IRB(BB);
    Value *ZeroVal = ConstantInt::get(Type::getInt32Ty(BB->getContext()), 0);

    for (std::map<GlobalVariable *,
                  std::pair<Constant *, GlobalVariable *>>::iterator
           GVIter = GV2Keys.begin(),
           GVIterEnd = GV2Keys.end();
         GVIter != GVIterEnd; ++GVIter) {
      ConstantDataArray *CastedCDA =
        cast<ConstantDataArray>(GVIter->second.first);

      // Prevent optimization of encrypted data
      appendToCompilerUsed(*GVIter->second.second->getParent(),
                           {GVIter->second.second});

      // Element-By-Element XOR so the fucking verifier won't complain.
      // Also, this hides keys.
      for (unsigned Ci = 0; Ci < CastedCDA->getType()->getNumElements(); Ci++) {
        Value *Offset =
          ConstantInt::get(Type::getInt32Ty(BB->getContext()), Ci);
        Value *EncryptedGEP =
          IRB.CreateGEP(GVIter->second.second, {ZeroVal, Offset});
        Value *DecryptedGEP = IRB.CreateGEP(GVIter->first, {ZeroVal, Offset});
        LoadInst *LI = IRB.CreateLoad(EncryptedGEP, "ObfsEncryptedChar");
        Value *XORed = IRB.CreateXor(LI, CastedCDA->getElementAsConstant(Ci));
        IRB.CreateStore(XORed, DecryptedGEP);
      }
    }

    IRB.CreateBr(DestBB);
  }

  bool doFinalization(Module &M) override {
    EncryptionStatus.clear();
    return false;
  }
};

} // namespace

char StringEncryption::ID = 0;

#define PASS_DESCRIPTION "Enable string encryption"

// Register to opt
static RegisterPass<StringEncryption> X(DEBUG_TYPE, PASS_DESCRIPTION);

// Register to clang
static cl::opt<bool> PassEnabled("enable-strcry", cl::NotHidden,
                                 cl::desc(PASS_DESCRIPTION), cl::init(false),
                                 cl::Optional);
static RegisterStandardPasses Y(PassManagerBuilder::EP_OptimizerLast,
                                [](const PassManagerBuilder &Builder,
                                   legacy::PassManagerBase &PM) {
                                  if (PassEnabled) {
                                    PM.add(new StringEncryption());
                                  }
                                });