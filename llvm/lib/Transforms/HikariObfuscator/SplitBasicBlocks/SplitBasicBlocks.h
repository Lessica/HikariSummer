#ifndef _SPLITBASICBLOCKS_H_
#define _SPLITBASICBLOCKS_H_

#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Pass.h"

#define SPLITBASICBLOCKS_PASS_DESCRIPTION                                      \
  "Split each basic block into several parts"

using namespace llvm;

namespace llvm {

FunctionPass *createSplitBasicBlocksPass();
void initializeSplitBasicBlocksPass(PassRegistry &Registry);

} // namespace llvm

#endif