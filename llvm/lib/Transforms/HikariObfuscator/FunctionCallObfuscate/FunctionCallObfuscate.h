#ifndef _FUNCTIONCALLOBFUSCATE_H_
#define _FUNCTIONCALLOBFUSCATE_H_

#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Pass.h"

#define FUNCTIONCALLOBFUSCATE_PASS_DESCRIPTION                                 \
  "Enable function call obfuscation"

using namespace llvm;

namespace llvm {

FunctionPass *createFunctionCallObfuscatePass();
void initializeFunctionCallObfuscatePass(PassRegistry &Registry);

} // namespace llvm

#endif