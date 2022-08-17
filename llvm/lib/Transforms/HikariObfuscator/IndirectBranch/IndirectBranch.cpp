#include "IndirectBranch.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Transforms/Utils/ModuleUtils.h"

using namespace llvm;

#define DEBUG_TYPE "obfs-indbra"

STATISTIC(AddedIndirectBranchCounter,
          "Number of added indirect branch instructions");

namespace {

struct IndirectBranch : public FunctionPass {
  static char ID; // Pass identification, replacement for typeid
  std::map<BasicBlock *, unsigned long long> IndexMap;
  IndirectBranch() : FunctionPass(ID) {}

  bool doInitialization(Module &M) override {
    std::vector<Constant *> BBs;
    unsigned long long Idx = 0;
    for (Function &F : M) {
      for (BasicBlock &BB : F) {
        BasicBlock *BBPtr = &BB;
        if (BBPtr != &(BBPtr->getParent()->getEntryBlock())) {
          IndexMap[BBPtr] = Idx++;
          BBs.push_back(BlockAddress::get(BBPtr));
        }
      }
    }
    ArrayType *ArrTy =
      ArrayType::get(Type::getInt8PtrTy(M.getContext()), BBs.size());
    Constant *BlockAddrArr =
      ConstantArray::get(ArrTy, ArrayRef<Constant *>(BBs));
    GlobalVariable *BlockAddrArrGV = new GlobalVariable(
      M, ArrTy, false, GlobalVariable::LinkageTypes::InternalLinkage,
      BlockAddrArr, "ObfsIBGlobalTable");
    appendToCompilerUsed(M, {BlockAddrArrGV});
    return true;
  }

  bool runOnFunction(llvm::Function &F) override {

    // Collect branching conditions
    std::vector<BranchInst *> BIs;
    for (Instruction &I : instructions(F)) {
      if (BranchInst *BI = dyn_cast<BranchInst>(&I)) {
        BIs.push_back(BI);
      }
    }

    Value *ZeroVal =
      ConstantInt::get(Type::getInt32Ty(F.getParent()->getContext()), 0);
    for (BranchInst *BI : BIs) {
      IRBuilder<> IRB(BI);
      std::vector<BasicBlock *> BBs;

      // We use the condition's evaluation result to generate the GEP
      // instruction. False evaluates to 0 while true evaluates to 1. So here
      // we insert the false block first.
      if (BI->isConditional()) {
        BBs.push_back(BI->getSuccessor(1));
      }
      BBs.push_back(BI->getSuccessor(0));

      ArrayType *ArrTy = ArrayType::get(
        Type::getInt8PtrTy(F.getParent()->getContext()), BBs.size());
      std::vector<Constant *> BlockAddrs;
      for (unsigned Bi = 0; Bi < BBs.size(); Bi++) {
        BlockAddrs.push_back(BlockAddress::get(BBs[Bi]));
      }

      GlobalVariable *LoadFrom = NULL;
      if (BI->isConditional() ||
          IndexMap.find(BI->getSuccessor(0)) == IndexMap.end()) {
        Constant *BlockAddrArr =
          ConstantArray::get(ArrTy, ArrayRef<Constant *>(BlockAddrs));
        LoadFrom =
          new GlobalVariable(*F.getParent(), ArrTy, false,
                             GlobalVariable::LinkageTypes::PrivateLinkage,
                             BlockAddrArr, "ObfsIBConditionalTable");
        appendToCompilerUsed(*F.getParent(), {LoadFrom});
      } else {
        LoadFrom = F.getParent()->getGlobalVariable("ObfsIBGlobalTable", true);
      }

      Value *Idx = NULL;
      if (BI->isConditional()) {
        Value *Cond = BI->getCondition();
        Idx =
          IRB.CreateZExt(Cond, Type::getInt32Ty(F.getParent()->getContext()));
      } else {
        Idx = ConstantInt::get(Type::getInt32Ty(F.getParent()->getContext()),
                               IndexMap[BI->getSuccessor(0)]);
      }

      Value *GEP = IRB.CreateGEP(LoadFrom, {ZeroVal, Idx});
      LoadInst *LoadI = IRB.CreateLoad(GEP, "ObfsIBTargetAddress");
      IndirectBrInst *IndBrI = IndirectBrInst::Create(LoadI, BBs.size());
      for (BasicBlock *BB : BBs) {
        IndBrI->addDestination(BB);
      }

      ReplaceInstWithInst(BI, IndBrI);
      AddedIndirectBranchCounter++;
    }

    return true;
  }

  bool doFinalization(Module &M) override {
    IndexMap.clear();
    return false;
  }
};

} // namespace

char IndirectBranch::ID = 0;

// Register to opt
static RegisterPass<IndirectBranch> X(DEBUG_TYPE,
                                      INDIRECTBRANCH_PASS_DESCRIPTION);

// Register to loader
FunctionPass *llvm::createIndirectBranchPass() { return new IndirectBranch(); }
INITIALIZE_PASS(IndirectBranch, DEBUG_TYPE, INDIRECTBRANCH_PASS_DESCRIPTION,
                false, false);