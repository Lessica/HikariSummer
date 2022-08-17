#ifndef _SUBSTITUTION_H_
#define _SUBSTITUTION_H_

#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Pass.h"

#define SUBSTITUTION_PASS_DESCRIPTION                                          \
  "Enable Instruction Substitution obfuscation"

using namespace llvm;

namespace llvm {

FunctionPass *createSubstitutionPass();
void initializeSubstitutionPass(PassRegistry &Registry);

} // namespace llvm

#endif