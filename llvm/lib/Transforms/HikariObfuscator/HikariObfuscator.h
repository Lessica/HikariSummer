#ifndef _HIKARIOBFUSCATOR_H_
#define _HIKARIOBFUSCATOR_H_

#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Pass.h"

#define HIKARIOBFUSCATOR_PASS_DESCRIPTION "Enable HikariObfuscator"

using namespace llvm;

namespace llvm {

ModulePass *createHikariObfuscatorPass();
void initializeHikariObfuscatorPass(PassRegistry &Registry);

} // namespace llvm

#endif