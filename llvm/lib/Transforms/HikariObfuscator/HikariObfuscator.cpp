/*
 * "Pass Scheduler" of HikariObfuscator.
 * Because currently there is no way to add dependency to transform passes.
 * Ref: http://lists.llvm.org/pipermail/llvm-dev/2011-February/038109.html
 */

#include "HikariObfuscator.h"
#include "AntiClassDump/AntiClassDump.h"
#include "BogusControlFlow/BogusControlFlow.h"
#include "Flattening/Flattening.h"
#include "FunctionCallObfuscate/FunctionCallObfuscate.h"
#include "FunctionWrapper/FunctionWrapper.h"
#include "IndirectBranch/IndirectBranch.h"
#include "SplitBasicBlocks/SplitBasicBlocks.h"
#include "StringEncryption/StringEncryption.h"
#include "Substitution/Substitution.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/CommandLine.h"

using namespace llvm;

#define DEBUG_TYPE "obfs"

static cl::opt<bool>
  AntiClassDumpEnabled("enable-acdobf", cl::NotHidden,
                       cl::desc(ANTICLASSDUMP_PASS_DESCRIPTION),
                       cl::init(false), cl::Optional);

static cl::opt<bool>
  BogusControlFlowEnabled("enable-bcfobf", cl::NotHidden,
                          cl::desc(BOGUSCONTROLFLOW_PASS_DESCRIPTION),
                          cl::init(false), cl::Optional);

static cl::opt<bool> FlatteningEnabled("enable-cffobf", cl::NotHidden,
                                       cl::desc(FLATTENING_PASS_DESCRIPTION),
                                       cl::init(false), cl::Optional);

static cl::opt<bool>
  FunctionCallObfuscateEnabled("enable-fco", cl::NotHidden,
                               cl::desc(FUNCTIONCALLOBFUSCATE_PASS_DESCRIPTION),
                               cl::init(false), cl::Optional);

static cl::opt<bool>
  FunctionWrapperEnabled("enable-funwra", cl::NotHidden,
                         cl::desc(FUNCTIONWRAPPER_PASS_DESCRIPTION),
                         cl::init(false), cl::Optional);

static cl::opt<bool>
  IndirectBranchEnabled("enable-indbra", cl::NotHidden,
                        cl::desc(INDIRECTBRANCH_PASS_DESCRIPTION),
                        cl::init(false), cl::Optional);

static cl::opt<bool>
  SplitBasicBlocksEnabled("enable-split", cl::NotHidden,
                          cl::desc(SPLITBASICBLOCKS_PASS_DESCRIPTION),
                          cl::init(false), cl::Optional);

static cl::opt<bool>
  StringEncryptionEnabled("enable-strcry", cl::NotHidden,
                          cl::desc(STRINGENCRYPTION_PASS_DESCRIPTION),
                          cl::init(false), cl::Optional);

static cl::opt<bool>
  SubstitutionEnabled("enable-subobf", cl::NotHidden,
                      cl::desc(SUBSTITUTION_PASS_DESCRIPTION), cl::init(false),
                      cl::Optional);

namespace {

struct HikariObfuscator : public ModulePass {
  static char ID;
  HikariObfuscator() : ModulePass(ID) {}

  bool doInitialization(Module &M) override {
    errs() << "HikariObfuscator: ";
    if (getenv("ACDOBF")) {
      errs() << "  * AntiClassDump ";
      AntiClassDumpEnabled = true;
    }
    if (getenv("BCFOBF")) {
      errs() << "  * BogusControlFlow ";
      BogusControlFlowEnabled = true;
    }
    if (getenv("CFFOBF")) {
      errs() << "  * Flattening ";
      FlatteningEnabled = true;
    }
    if (getenv("FCO")) {
      errs() << "  * FunctionCallObfuscate ";
      FunctionCallObfuscateEnabled = true;
    }
    if (getenv("FUNWRA")) {
      errs() << "  * FunctionWrapper ";
      FunctionWrapperEnabled = true;
    }
    if (getenv("INDBRA")) {
      errs() << "  * IndirectBranch ";
      IndirectBranchEnabled = true;
    }
    if (getenv("SPLIT")) {
      errs() << "  * SplitBasicBlocks ";
      SplitBasicBlocksEnabled = true;
    }
    if (getenv("STRCRY")) {
      errs() << "  * StringEncryption ";
      StringEncryptionEnabled = true;
    }
    if (getenv("SUBOBF")) {
      errs() << "  * Substitution ";
      SubstitutionEnabled = true;
    }
    return false;
  }

  bool runOnModule(llvm::Module &M) override {

    errs() << "HikariObfuscator in.\n";

    if (AntiClassDumpEnabled) {
      ModulePass *P = createAntiClassDumpPass();
      P->doInitialization(M);
      P->runOnModule(M);
      P->doFinalization(M);
      delete P;
    }

    if (FunctionCallObfuscateEnabled) {
      FunctionPass *P = createFunctionCallObfuscatePass();
      P->doInitialization(M);
      for (Function &F : M) {
        if (!F.isDeclaration()) {
          P->runOnFunction(F);
        }
      }
      P->doFinalization(M);
      delete P;
    }

    if (StringEncryptionEnabled) {
      ModulePass *P = createStringEncryptionPass();
      P->doInitialization(M);
      P->runOnModule(M);
      P->doFinalization(M);
      delete P;
    }

    if (SplitBasicBlocksEnabled) {
      FunctionPass *P = createSplitBasicBlocksPass();
      P->doInitialization(M);
      for (Function &F : M) {
        if (F.isDeclaration()) {
          continue;
        }
        P->runOnFunction(F);
      }
      P->doFinalization(M);
      delete P;
    }

    if (BogusControlFlowEnabled) {
      FunctionPass *P = createBogusControlFlowPass();
      P->doInitialization(M);
      for (Function &F : M) {
        if (F.isDeclaration()) {
          continue;
        }
        P->runOnFunction(F);
      }
      P->doFinalization(M);
      delete P;
    }

    if (FlatteningEnabled) {
      FunctionPass *P = createFlatteningPass();
      P->doInitialization(M);
      for (Function &F : M) {
        if (F.isDeclaration()) {
          continue;
        }
        P->runOnFunction(F);
      }
      P->doFinalization(M);
      delete P;
    }

    if (SubstitutionEnabled) {
      FunctionPass *P = createSubstitutionPass();
      P->doInitialization(M);
      for (Function &F : M) {
        if (F.isDeclaration()) {
          continue;
        }
        P->runOnFunction(F);
      }
      P->doFinalization(M);
      delete P;
    }

    if (IndirectBranchEnabled) {
      FunctionPass *P = createIndirectBranchPass();
      P->doInitialization(M);
      for (Function &F : M) {
        P->runOnFunction(F);
      }
      P->doFinalization(M);
      delete P;
    }

    if (FunctionWrapperEnabled) {
      ModulePass *P = createFunctionWrapperPass();
      P->doInitialization(M);
      P->runOnModule(M);
      P->doFinalization(M);
      delete P;
    }

    errs() << "HikariObfuscator out.\n";
    return true;
  }

  bool doFinalization(Module &M) override {
    std::vector<Function *> FuncsToDelete;
    for (Function &F : M) {
      if (F.isDeclaration() && F.hasName() && F.getName().contains("hikari_")) {
        for (User *U : F.users()) {
          if (Instruction *Inst = dyn_cast<Instruction>(U)) {
            Inst->eraseFromParent();
          }
        }
        FuncsToDelete.push_back(&F);
      }
    }

    for (Function *F : FuncsToDelete) {
      F->eraseFromParent();
    }

    return true;
  }
};

} // namespace

char HikariObfuscator::ID = 0;

// Register to opt
static RegisterPass<HikariObfuscator> X(DEBUG_TYPE,
                                        HIKARIOBFUSCATOR_PASS_DESCRIPTION);

// Register to loader
ModulePass *llvm::createHikariObfuscatorPass() {
  return new HikariObfuscator();
}

INITIALIZE_PASS_BEGIN(HikariObfuscator, DEBUG_TYPE,
                      HIKARIOBFUSCATOR_PASS_DESCRIPTION, false, false)
INITIALIZE_PASS_DEPENDENCY(AntiClassDump);
INITIALIZE_PASS_DEPENDENCY(BogusControlFlow);
INITIALIZE_PASS_DEPENDENCY(Flattening);
INITIALIZE_PASS_DEPENDENCY(FunctionCallObfuscate);
INITIALIZE_PASS_DEPENDENCY(FunctionWrapper);
INITIALIZE_PASS_DEPENDENCY(IndirectBranch);
INITIALIZE_PASS_DEPENDENCY(SplitBasicBlocks);
INITIALIZE_PASS_DEPENDENCY(StringEncryption);
INITIALIZE_PASS_DEPENDENCY(Substitution);
INITIALIZE_PASS_END(HikariObfuscator, DEBUG_TYPE,
                    HIKARIOBFUSCATOR_PASS_DESCRIPTION, false, false)