#include "llvm/ADT/Statistic.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Pass.h"
#include "llvm/Support/CommandLine.h"

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

