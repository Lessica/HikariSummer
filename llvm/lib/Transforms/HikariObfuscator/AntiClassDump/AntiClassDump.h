#ifndef _ANTICLASSDUMP_H_
#define _ANTICLASSDUMP_H_

#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Pass.h"

#define ANTICLASSDUMP_PASS_DESCRIPTION                                         \
  "Enable protection against Objective-C class dump"

using namespace llvm;

namespace llvm {

ModulePass *createAntiClassDumpPass();
void initializeAntiClassDumpPass(PassRegistry &Registry);

} // namespace llvm

#endif