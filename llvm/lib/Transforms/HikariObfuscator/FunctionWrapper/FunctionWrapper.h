#ifndef _FUNCTIONWRAPPER_H_
#define _FUNCTIONWRAPPER_H_

#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Pass.h"

#define FUNCTIONWRAPPER_PASS_DESCRIPTION "Enable Function Wrapper obfuscation"

using namespace llvm;

namespace llvm {

ModulePass *createFunctionWrapperPass();
void initializeFunctionWrapperPass(PassRegistry &Registry);

} // namespace llvm

#endif