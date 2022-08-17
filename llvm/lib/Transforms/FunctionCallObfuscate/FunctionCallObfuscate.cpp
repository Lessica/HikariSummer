#include "json.hpp"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/Triple.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/NoFolder.h"
#include "llvm/Pass.h"
#include "llvm/Support/CommandLine.h"
#include <fstream>

using namespace llvm;
using json = nlohmann::json;

#define DEBUG_TYPE "obfs-fco"

#define DLOPEN_DARWIN_FLAG (0x2 | 0x8)
#define DLOPEN_ANDROID64_FLAG (0x00002 | 0x100)
#define DLOPEN_ANDROID32_FLAG (0x0000 | 0x2)

static cl::opt<uint64_t>
  RtldDefaultFlags(DEBUG_TYPE "-flags", cl::NotHidden,
                   cl::desc("The value of RTLD_DEFAULT on your platform"),
                   cl::init(-1), cl::Optional);

static cl::opt<StringRef>
  ConfigurationJSONPath(DEBUG_TYPE "-config", cl::NotHidden,
                        cl::desc("The path to the configuration JSON file"),
                        cl::init("+-x/"), cl::Optional);

STATISTIC(ObfuscatedFunctionCounter, "Number of functions obfuscated");

namespace {

struct FunctionCallObfuscate : public FunctionPass {
  static char ID; // Pass identification, replacement for typeid
  json Configuration;
  FunctionCallObfuscate() : FunctionPass(ID) {}

  bool doInitialization(Module &M) override {
    if (ConfigurationJSONPath == "+-x/") {
      errs() << "Configuration JSON path is not specified\n";
      SmallString<32> Path;
      if (sys::path::home_directory(Path)) {
        sys::path::append(Path, "HikariObfuscator", "SymbolConfig.json");
        ConfigurationJSONPath = Path.str();
      }
    }

    std::ifstream InputFile(ConfigurationJSONPath.str());
    if (InputFile.good()) {
      errs() << "Reading configuration from " << ConfigurationJSONPath << "\n";
      InputFile >> this->Configuration;
    } else {
      errs() << "Failed to read configuration from " << ConfigurationJSONPath
             << "\n";
    }

    Triple Tri(M.getTargetTriple());
    if (Tri.getVendor() != Triple::VendorType::Apple) {
      errs() << M.getTargetTriple()
             << " is not supported by " DEBUG_TYPE ".\nProbably GNU Step?\n";
      return false;
    }

    // Generic ObjC runtime declarations
    Type *Int64Ty = Type::getInt64Ty(M.getContext()),
         *Int32Ty = Type::getInt32Ty(M.getContext()),
         *Int8PtrTy = Type::getInt8PtrTy(M.getContext()),
         *Int8Ty = Type::getInt8Ty(M.getContext());

    // id (*IMP)(id, SEL, ...)
    FunctionType *IMPType =
      FunctionType::get(Int8PtrTy, {Int8PtrTy, Int8PtrTy}, true);
    PointerType *IMPPointerType = PointerType::get(IMPType, 0);

    // IMP class_replaceMethod(Class cls, SEL name, IMP imp, const char *types);
    std::vector<Type *> ClassReplaceMethodTyArgs;
    ClassReplaceMethodTyArgs.push_back(Int8PtrTy);      // Class cls
    ClassReplaceMethodTyArgs.push_back(Int8PtrTy);      // SEL name
    ClassReplaceMethodTyArgs.push_back(IMPPointerType); // IMP imp
    ClassReplaceMethodTyArgs.push_back(Int8PtrTy);      // const char *types
    FunctionType *ClassReplaceMethodTy =
      FunctionType::get(IMPPointerType, ClassReplaceMethodTyArgs, false);
    M.getOrInsertFunction("class_replaceMethod", ClassReplaceMethodTy);

    // SEL sel_registerName(const char *str);
    FunctionType *SELRegisterNameTy =
      FunctionType::get(Int8PtrTy, {Int8PtrTy}, false);
    M.getOrInsertFunction("sel_registerName", SELRegisterNameTy);

    // id objc_getClass(const char *name);
    FunctionType *ObjCGetClassTy =
      FunctionType::get(Int8PtrTy, {Int8PtrTy}, false);
    M.getOrInsertFunction("objc_getClass", ObjCGetClassTy);
    M.getOrInsertFunction("objc_getMetaClass", ObjCGetClassTy);

    // objc_property_attribute_t { const char *name; const char *value; };
    StructType *ObjCPropertyAttributeTTy = StructType::getTypeByName(
      M.getContext(), "struct.objc_property_attribute_t");
    if (ObjCPropertyAttributeTTy == NULL) {
      std::vector<Type *> Tys;
      Tys.push_back(Int8PtrTy); // const char *name
      Tys.push_back(Int8PtrTy); // const char *value
      ObjCPropertyAttributeTTy = StructType::create(
        ArrayRef<Type *>(Tys), "struct.objc_property_attribute_t");
      M.getOrInsertGlobal("struct.objc_property_attribute_t",
                          ObjCPropertyAttributeTTy);
    }

    // BOOL class_addIvar(Class cls, const char *name, size_t size, uint8_t
    // alignment, const char *types);
    std::vector<Type *> AddIvarTyArgs;
    AddIvarTyArgs.push_back(Int8PtrTy); // Class cls
    AddIvarTyArgs.push_back(Int8PtrTy); // const char *name
    if (Tri.isArch64Bit()) {            // size_t size
      AddIvarTyArgs.push_back(Int64Ty);
    } else {
      AddIvarTyArgs.push_back(Int32Ty);
    }
    AddIvarTyArgs.push_back(Int8Ty);    // uint8_t alignment
    AddIvarTyArgs.push_back(Int8PtrTy); // const char *types
    FunctionType *AddIvarTy = FunctionType::get(Int8Ty, AddIvarTyArgs, false);
    M.getOrInsertFunction("class_addIvar", AddIvarTy);

    // BOOL class_addProperty(Class cls, const char *name, const
    // objc_property_attribute_t *attributes, unsigned int attributeCount);
    std::vector<Type *> AddPropTyArgs;
    AddPropTyArgs.push_back(Int8PtrTy); // Class cls
    AddPropTyArgs.push_back(Int8PtrTy); // const char *name
    AddPropTyArgs.push_back(
      ObjCPropertyAttributeTTy
        ->getPointerTo()); // const objc_property_attribute_t *attributes
    AddPropTyArgs.push_back(Int32Ty); // unsigned int attributeCount
    FunctionType *AddPropTy = FunctionType::get(Int8Ty, AddPropTyArgs, false);
    M.getOrInsertFunction("class_addProperty", AddPropTy);

    // const char * class_getName(Class cls);
    FunctionType *ClassGetNameTy =
      FunctionType::get(Int8PtrTy, {Int8PtrTy}, false);
    M.getOrInsertFunction("class_getName", ClassGetNameTy);

    // id objc_getMetaClass(const char *name);
    FunctionType *ObjCGetMetaClassTy =
      FunctionType::get(Int8PtrTy, {Int8PtrTy}, false);
    M.getOrInsertFunction("objc_getMetaClass", ObjCGetMetaClassTy);

    // void *dlopen(const char* path, int mode);
    FunctionType *DLOpenTy =
      FunctionType::get(Int8PtrTy, {Int8PtrTy, Int32Ty}, false);
    M.getOrInsertFunction("dlopen", DLOpenTy);

    // void *dlsym(void *handle, const char *symbol);
    FunctionType *DLSymTy =
      FunctionType::get(Int8PtrTy, {Int8PtrTy, Int8PtrTy}, false);
    M.getOrInsertFunction("dlsym", DLSymTy);

    return true;
  }

  bool runOnFunction(llvm::Function &F) override {

    Triple Tri(F.getParent()->getTargetTriple());
    if (!Tri.isAndroid() && !Tri.isOSDarwin()) {
      errs() << "Only Android and Darwin are supported by " DEBUG_TYPE ".\n";
      return false;
    }

    if (Tri.isOSDarwin()) {
      RtldDefaultFlags = DLOPEN_DARWIN_FLAG;
    } else if (Tri.isAndroid()) {
      RtldDefaultFlags =
        Tri.isArch64Bit() ? DLOPEN_ANDROID64_FLAG : DLOPEN_ANDROID32_FLAG;
    }

    DEBUG_WITH_TYPE(DEBUG_TYPE,
                    errs() << "Running on function: " << F.getName() << "\n");

    fixFunctionConstantExpr(&F);

    Module *M = F.getParent();
    handleObjC(*M);

    Type *Int8PtrTy = Type::getInt8PtrTy(M->getContext()),
         *Int32Ty = Type::getInt32Ty(M->getContext());

    Function *DLOpenFunc = M->getFunction("dlopen"),
             *DLSymFunc = M->getFunction("dlsym");

    for (BasicBlock &BB : F) {
      for (BasicBlock::iterator I = BB.getFirstInsertionPt(), IE = BB.end();
           I != IE; ++I) {

        Instruction &Inst = *I;
        CallBase *CB = dyn_cast<CallBase>(&Inst);
        if (!CB) {
          continue;
        }

        Function *Callee = CB->getCalledFunction();

        if (!Callee) {
          Callee =
            dyn_cast<Function>(CB->getCalledOperand()->stripPointerCasts());
        }

        if (!Callee) {
          DEBUG_WITH_TYPE(DEBUG_TYPE, errs()
                                        << "Failed to get callee for "
                                        << *CB->getCalledOperand() << "\n");
          continue;
        }

        // It's only safe to restrict our modification to external symbols.
        // Otherwise, stripped binary will crash.
        if (!Callee->empty() || Callee->getName().equals("dlsym") ||
            Callee->getName().equals("dlopen") || Callee->isIntrinsic()) {
          continue;
        }

        if (this->Configuration.find(Callee->getName()) !=
            this->Configuration.end()) {

          std::string NewName =
            this->Configuration[Callee->getName().str()].get<std::string>();
          StringRef NewNameRef(NewName);

          BasicBlock *EntryBB = CB->getParent();
          IRBuilder<> IRB(EntryBB, EntryBB->getFirstInsertionPt());

          std::vector<Value *> DLOpenArgs;
          DLOpenArgs.push_back(Constant::getNullValue(Int8PtrTy));
          DLOpenArgs.push_back(ConstantInt::get(Int32Ty, RtldDefaultFlags));

          Value *FuncHandle =
            IRB.CreateCall(DLOpenFunc, ArrayRef<Value *>(DLOpenArgs));

          std::vector<Value *> DLSymArgs;
          DLSymArgs.push_back(FuncHandle);
          DLSymArgs.push_back(IRB.CreateGlobalStringPtr(NewNameRef));

          Value *NewFunc =
            IRB.CreateCall(DLSymFunc, ArrayRef<Value *>(DLSymArgs));
          Value *BitCastedNewFunc =
            IRB.CreateBitCast(NewFunc, CB->getCalledOperand()->getType());

          CB->setCalledOperand(BitCastedNewFunc);
        }
      }
    }

    return true;
  }

  // Replace ConstantExpr with equal instructions, otherwise replacing on
  // Constant will crash the compiler.
  void fixFunctionConstantExpr(Function *F) {
    for (BasicBlock &BB : *F) {
      fixBasicBlockConstantExpr(&BB);
    }
  }

  // PHIs must be placed at BB start so CEs must be placed prior to current BB.
  void fixBasicBlockConstantExpr(BasicBlock *BB) {

    assert(!BB->empty() && "BasicBlock is empty!");
    assert((BB->getParent() != NULL) &&
           "BasicBlock must be inside a Function!");

    Instruction *FunctionInsertPt =
      &*(BB->getParent()->getEntryBlock().getFirstInsertionPt());

    for (Instruction &I : *BB) {
      if (isa<LandingPadInst>(I) || isa<FuncletPadInst>(I)) {
        continue;
      }
      for (unsigned Oi = 0; Oi < I.getNumOperands(); Oi++) {
        if (ConstantExpr *C = dyn_cast<ConstantExpr>(I.getOperand(Oi))) {
          Instruction *InsertPt = &I;
          IRBuilder<NoFolder> IRB(InsertPt);
          if (isa<PHINode>(I)) {
            IRB.SetInsertPoint(FunctionInsertPt);
          }
          Instruction *Inst = IRB.Insert(C->getAsInstruction());
          I.setOperand(Oi, Inst);
        }
      }
    }
  }

  void handleObjC(Module &M) {
    Function *ObjCGetClassFunc = cast<Function>(M.getFunction("objc_getClass"));
    Function *SelRegisterNameFunc =
      cast<Function>(M.getFunction("sel_registerName"));

    // Iterate over all class-ref uses and replace with objc_getClass() call.
    // Strings are encrypted in other passes.
    for (GlobalVariable &GV : M.globals()) {
      if (!GV.hasInitializer()) {
        continue;
      }

      std::string GVName = GV.getName().str();
      if (GVName.find("OBJC_CLASSLIST_REFERENCES") == 0) {
        std::string ClsName = GV.getInitializer()->getName().str();
        ClsName.replace(ClsName.find("OBJC_CLASS_$_"),
                        sizeof("OBJC_CLASS_$_") - 1, "");
        for (User *U : GV.users()) {
          if (Instruction *I = dyn_cast<Instruction>(U)) {
            IRBuilder<> IRB(I);
            Value *NewClsName = IRB.CreateGlobalStringPtr(ClsName);
            CallInst *CallI = IRB.CreateCall(ObjCGetClassFunc, {NewClsName});
            // We need to bit-cast it back to bypass IRVerifier
            Value *BitCastedI = IRB.CreateBitCast(CallI, I->getType());
            I->replaceAllUsesWith(BitCastedI);
            I->eraseFromParent();
          }
        }
      } else if (GVName.find("OBJC_SELECTOR_REFERENCES") == 0) {
        ConstantExpr *CE = cast<ConstantExpr>(GV.getInitializer());
        Constant *C = CE->getOperand(0);
        GlobalVariable *SelNameGV = dyn_cast<GlobalVariable>(C);
        ConstantDataArray *SelNameArr =
          dyn_cast<ConstantDataArray>(SelNameGV->getInitializer());
        StringRef SelName = SelNameArr->getAsString();
        for (User *U : GV.users()) {
          if (Instruction *I = dyn_cast<Instruction>(U)) {
            IRBuilder<> IRB(I);
            Value *NewSelName = IRB.CreateGlobalStringPtr(SelName);
            CallInst *CallI = IRB.CreateCall(SelRegisterNameFunc, {NewSelName});
            // We need to bit-cast it back to bypass IRVerifier
            Value *BitCastedI = IRB.CreateBitCast(CallI, I->getType());
            I->replaceAllUsesWith(BitCastedI);
            I->eraseFromParent();
          }
        }
      }

      GV.removeDeadConstantUsers();
      if (GV.use_empty()) {
        GV.dropAllReferences();
        GV.eraseFromParent();
      }
    }
  }
};

} // namespace

char FunctionCallObfuscate::ID = 0;
static RegisterPass<FunctionCallObfuscate>
  X(DEBUG_TYPE, "Enable function call obfuscation");