#include "llvm/ADT/Statistic.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/Module.h"
#include "llvm/Pass.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Transforms/CryptoUtils.h"
#include "llvm/Transforms/Utils/ModuleUtils.h"

using namespace llvm;

#define DEBUG_TYPE "obfs-funwra"

static cl::opt<unsigned> ProbabilityRate(
  DEBUG_TYPE "-prob", cl::NotHidden,
  cl::desc(
    "The probability (%) each call site will be obfuscated by the " DEBUG_TYPE
    " pass"),
  cl::init(70),
  cl::Optional
);

static cl::opt<unsigned> LoopTimes(
  DEBUG_TYPE "-loop", cl::NotHidden,
  cl::desc("Number of times the " DEBUG_TYPE " pass will loop on a call site"),
  cl::init(1),
  cl::Optional
);

namespace {

struct FunctionWrapper : public ModulePass {
  static char ID; // Pass identification, replacement for typeid
  FunctionWrapper() : ModulePass(ID) {
    llvm::SharedCryptoUtils->prng_seed();
  }

  bool runOnModule(Module &M) override {
    if (LoopTimes == 0) {
      errs() << DEBUG_TYPE "-loop should be greater than 0\n";
      return false;
    }
    if (ProbabilityRate > 100) {
      errs() << DEBUG_TYPE "-prob should be less than 100\n";
      return false;
    }
    std::vector<CallBase *> CallBases;
    for (Function &F : M) {
      for (inst_iterator Fi = inst_begin(&F), Fe = inst_end(&F);
           Fi != Fe; Fi++)
      {
        Instruction *FuncInst = &*Fi;
        if (isa<CallInst>(FuncInst) || isa<InvokeInst>(FuncInst)) {
          if (llvm::SharedCryptoUtils->get_range(100) <= ProbabilityRate) {
            CallBases.push_back(cast<CallBase>(FuncInst));
          }
        }
      }
    }
    for (CallBase *CB : CallBases) {
      for (unsigned Ci = 0; Ci < LoopTimes && CB != nullptr; Ci++) {
        CB = handleCallBase(CB);
      }
    }
    return true;
  }

  CallBase *handleCallBase(CallBase *CB) {

    Value *CalledFunction = CB->getCalledFunction();
    if (CalledFunction == nullptr) {
      CalledFunction = CB->getCalledOperand()->stripPointerCasts();
    }

    // Filter out IndirectCalls that depends on the context.
    // Otherwise, It'll be blatantly troublesome since you can't reference an
    // Instruction outside its BB. Too much trouble for a hobby project.
    // To be precise, we only keep CB that refers to a non-intrinsic function
    // either directly or through casting.
    if (CalledFunction == nullptr ||
        (!isa<ConstantExpr>(CalledFunction) &&
         !isa<Function>(CalledFunction)) ||
        CB->getIntrinsicID() != Intrinsic::not_intrinsic)
    {
      return nullptr;
    }

    if (Function *FuncTmp = dyn_cast<Function>(CalledFunction)) {
      if (FuncTmp->getName().startswith("clang.")) {
        return nullptr; // Clang Intrinsic
      }
      for (Argument *ArgIter = FuncTmp->arg_begin(),
                    *ArgEnd = FuncTmp->arg_end();
           ArgIter != ArgEnd; ++ArgIter)
      {
        Argument &Arg = *(ArgIter);
        if (Arg.hasByValAttr()) {

          // Arguments with byval attribute yields issues without proper
          // handling. The "proper" method to handle this is to revisit and
          // patch attribute stealing code. Technically readonly attr probably
          // should also get filtered out here.

          // Nah, too much work. This would do for open-source version since
          // private already this pass with more advanced solutions.

          return nullptr;
        }
      }
    }

    // Create a new function which in turn calls the actual function
    std::vector<Type *> Tys;
    for (unsigned Ci = 0; Ci < CB->getNumArgOperands(); Ci++) {
      Tys.push_back(CB->getArgOperand(Ci)->getType());
    }

    FunctionType *FuncTy =
      FunctionType::get(CB->getType(), ArrayRef<Type *>(Tys), false);
    Function *Func =
      Function::Create(FuncTy, GlobalValue::LinkageTypes::InternalLinkage,
                       "ObfsFunctionWrapper", CB->getParent()->getModule());

    // Trolling was all fun and shit so old implementation forced this symbol to
    // exist in all objects.
    appendToCompilerUsed(*Func->getParent(), {Func});

    BasicBlock *BB = BasicBlock::Create(Func->getContext(), "", Func);
    IRBuilder<> IRB(BB);

    std::vector<Value *> Params;
    for (Argument *Arg = Func->arg_begin(), *ArgEnd = Func->arg_end();
         Arg != ArgEnd; Arg++)
    {
      Params.push_back(Arg);
    }

    Constant *Callee = ConstantExpr::getBitCast(
      cast<Function>(CalledFunction), CB->getCalledOperand()->getType());
    FunctionType *CalleeTy =
      cast<FunctionType>(Callee->getType()->getPointerElementType());
    Value *RetVal = IRB.CreateCall(CalleeTy, Callee, ArrayRef<Value *>(Params));
    if (FuncTy->getReturnType()->isVoidTy()) {
      IRB.CreateRetVoid();
    } else {
      IRB.CreateRet(RetVal);
    }

    CB->setCalledFunction(Func);
    CB->mutateFunctionType(FuncTy);

    return CB;
  }
};

} // namespace

char FunctionWrapper::ID = 0;
static RegisterPass<FunctionWrapper>
  X(DEBUG_TYPE,
    "Enable Function Wrapper obfuscation");