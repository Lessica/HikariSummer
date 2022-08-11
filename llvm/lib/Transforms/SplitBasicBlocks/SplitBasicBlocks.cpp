#include "llvm/ADT/Statistic.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Pass.h"
#include "llvm/Support/CommandLine.h"

using namespace llvm;

#define DEBUG_TYPE "obfs-split"

static cl::opt<unsigned> MaxSplitTimes(
  "obfs-split-times", cl::NotHidden,
  cl::desc("Split each basic block into N parts"),
  cl::init(2),
  cl::Optional
);

STATISTIC(SplitInitialBasicBlocksCounter, "Initial number of basic blocks");
STATISTIC(SplitFinalBasicBlocksCounter, "Final number of basic blocks");

namespace {

struct SplitBasicBlocks : public FunctionPass {
  static char ID; // Pass identification, replacement for typeid
  SplitBasicBlocks() : FunctionPass(ID) {}

  bool runOnFunction(Function &F) override {
    if (MaxSplitTimes > 10) {
      errs() << "obfs-split-times should be less than 10\n";
      return false;
    }
    std::vector<BasicBlock *> OriginalBasicBlock;
    for (BasicBlock &BB : F) {
      SplitInitialBasicBlocksCounter++;
      OriginalBasicBlock.push_back(&BB);
    }
    for (BasicBlock *BB : OriginalBasicBlock) {
      SplitFinalBasicBlocksCounter++;
      if (basicBlockContainsPHINode(BB) || BB->size() < 2) {
        continue;
      }
      basicBlockSplit(BB);
    }
    return true;
  }

  void basicBlockSplit(BasicBlock *BB) {
    int SplitCnt = std::min((int)MaxSplitTimes, (int)BB->size());
    int SplitSize = (BB->size() + SplitCnt - 1) / SplitCnt;
    int InstIdx;
    BasicBlock *CurrBB = BB;
    while (--SplitCnt) {
      InstIdx = 0;
      for (Instruction &I : *CurrBB) {
        if (InstIdx++ == SplitSize) {
          CurrBB = CurrBB->splitBasicBlock(&I);
          SplitFinalBasicBlocksCounter++;
          break;
        }
      }
    }
  }

  bool basicBlockContainsPHINode(BasicBlock *BB) {
    for (Instruction &I : *BB) {
      if (isa<PHINode>(&I)) {
        return true;
      }
    }
    return false;
  }
};

} // namespace

char SplitBasicBlocks::ID = 0;
static RegisterPass<SplitBasicBlocks>
  X(DEBUG_TYPE, "Split each basic block into several parts");