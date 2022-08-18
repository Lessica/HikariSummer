#include "../HikariObfuscator.h"
#include "substrate.h"
#include <llvm/Config/abi-breaking.h>
#include <llvm/Support/CommandLine.h>
#include <llvm/Support/raw_ostream.h>

#if LLVM_ENABLE_ABI_BREAKING_CHECKS
#error "Configure LLVM with -DLLVM_ABI_BREAKING_CHECKS=FORCE_OFF"
#endif

using namespace llvm;

Pass *(*old_get_LS)();

extern "C" Pass *_ZN4llvm21createLowerSwitchPassEv() { return old_get_LS(); }

void (*old_pmb)(void *dis, legacy::PassManagerBase &MPM);
static void new_pmb(void *dis, legacy::PassManagerBase &MPM) {
  MPM.add(createHikariObfuscatorPass());
  old_pmb(dis, MPM);
}

static __attribute__((__constructor__)) void Inj3c73d(int argc, char *argv[]) {
  char *ExecutablePath = argv[0];

  // Initialize our own LLVM Library
  MSImageRef ExecutableImage = MSGetImageByName(ExecutablePath);
  // errs() << "Applying Apple Clang Hooks...\n";

  MSHookFunction(
    (void *)MSFindSymbol(
      ExecutableImage,
      "__ZN4llvm18PassManagerBuilder25populateModulePassManagerERNS_6legacy15PassManagerBaseE"),
    (void *)new_pmb, (void **)&old_pmb);

  old_get_LS = (Pass * (*)()) MSFindSymbol(
    ExecutableImage,
    "__ZN4llvm15callDefaultCtorIN12_GLOBAL__N_111LowerSwitchEEEPNS_4PassEv");
}