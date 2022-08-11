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

STATISTIC(SplitCounter, "Counts number of the split operation");
STATISTIC(SplittedBasicBlocksCounter, "Counts number of basic blocks split");

namespace {

struct SplitBasicBlocks : public FunctionPass {
  static char ID; // Pass identification, replacement for typeid
  SplitBasicBlocks() : FunctionPass(ID) {}

  bool runOnFunction(Function &F) override {
    std::vector<BasicBlock *> OriginalBasicBlock;
    for (BasicBlock &BB : F) {
      OriginalBasicBlock.push_back(&BB);
    }
    for (BasicBlock *BB : OriginalBasicBlock) {
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
          SplitCounter++;
          break;
        }
      }
    }
    SplittedBasicBlocksCounter++;
  }

  // Why PHI Node cannot be split?
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