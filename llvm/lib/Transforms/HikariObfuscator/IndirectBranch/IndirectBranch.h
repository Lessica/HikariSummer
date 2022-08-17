#ifndef _INDIRECTBRANCH_H_
#define _INDIRECTBRANCH_H_

#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Pass.h"

#define INDIRECTBRANCH_PASS_DESCRIPTION "Enable Indirect Branch obfuscation"

using namespace llvm;

namespace llvm {

FunctionPass *createIndirectBranchPass();
void initializeIndirectBranchPass(PassRegistry &Registry);

} // namespace llvm

#endif