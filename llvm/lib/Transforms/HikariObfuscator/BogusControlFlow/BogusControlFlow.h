#ifndef _BOGUSCONTROLFLOW_H_
#define _BOGUSCONTROLFLOW_H_

#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Pass.h"

#define BOGUSCONTROLFLOW_PASS_DESCRIPTION                                      \
  "Enable Bogus Control Flow (BCF) obfuscation"

using namespace llvm;

namespace llvm {

FunctionPass *createBogusControlFlowPass();
void initializeBogusControlFlowPass(PassRegistry &Registry);

} // namespace llvm

#endif