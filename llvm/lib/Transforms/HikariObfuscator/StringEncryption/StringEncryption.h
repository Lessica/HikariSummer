#ifndef _STRINGENCRYPTION_H_
#define _STRINGENCRYPTION_H_

#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Pass.h"

#define STRINGENCRYPTION_PASS_DESCRIPTION "Enable string encryption"

using namespace llvm;

namespace llvm {

ModulePass *createStringEncryptionPass();
void initializeStringEncryptionPass(PassRegistry &Registry);

} // namespace llvm

#endif