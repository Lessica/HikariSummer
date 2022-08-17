#ifndef _FLATTENING_H_
#define _FLATTENING_H_

#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Pass.h"

#define FLATTENING_PASS_DESCRIPTION                                            \
  "Enable Control Flow Flattening (CFF/FLA) obfuscation"

using namespace llvm;

namespace llvm {

FunctionPass *createFlatteningPass();
void initializeFlatteningPass(PassRegistry &Registry);

} // namespace llvm

#endif