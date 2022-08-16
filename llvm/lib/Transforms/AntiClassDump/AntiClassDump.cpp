#include "llvm/ADT/Statistic.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"
#include "llvm/Pass.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/Transforms/Utils/ModuleUtils.h"
#include <algorithm>
#include <cassert>

using namespace llvm;

#define DEBUG_TYPE "obfs-acd"

static cl::opt<bool>
  UseInitialize(DEBUG_TYPE "-use-initialize", cl::NotHidden,
                cl::desc("Inject codes to +initialize instead of +load"),
                cl::init(false), cl::Optional);

STATISTIC(ObfuscatedClassCounter, "Number of hidden classes");
STATISTIC(ObfuscatedInstanceMethodCounter, "Number of hidden instance methods");
STATISTIC(ObfuscatedClassMethodCounter, "Number of hidden class methods");

namespace {

struct AntiClassDump : public ModulePass {
  static char ID; // Pass identification, replacement for typeid
  AntiClassDump() : ModulePass(ID) {}

  bool doInitialization(Module &M) override {

    Triple Tri(M.getTargetTriple());
    if (Tri.getVendor() != Triple::VendorType::Apple) {
      errs() << M.getTargetTriple()
             << " is not supported by obfs-acd.\nProbably GNU Step?\n";
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

    return true;
  }

  bool runOnModule(Module &M) override {

    GlobalVariable *ObjCLabelClsGV =
      M.getGlobalVariable("OBJC_LABEL_CLASS_$", true);
    if (ObjCLabelClsGV == NULL) {
      errs() << "No ObjC class found in: " << M.getSourceFileName() << "\n";
      return false;
    }

    assert(ObjCLabelClsGV->hasInitializer() &&
           "OBJC_LABEL_CLASS_$ does not have an initializer.");
    ConstantArray *ObjCLabelClsCDS =
      dyn_cast<ConstantArray>(ObjCLabelClsGV->getInitializer());
    assert(ObjCLabelClsCDS &&
           "OBJC_LABEL_CLASS_$ is not a ConstantArray. Is the target using "
           "unsupported legacy runtime?");

    // This is for storing classes that can be used in handleClass().
    std::vector<std::string> RdyClsNames;

    // This is temporary storage for classes.
    std::deque<std::string> NotRdyClsNames;

    std::map<std::string /* Class */, std::string /* Super Class */> Cls2SupCls;

    // Map class name to corresponding GV
    std::map<std::string /* Class */, GlobalVariable *> ClsName2GV;

    for (unsigned Ci = 0; Ci < ObjCLabelClsCDS->getNumOperands(); Ci++) {

      ConstantExpr *ClsCE =
        dyn_cast<ConstantExpr>(ObjCLabelClsCDS->getOperand(Ci));
      GlobalVariable *ClsCEGV = dyn_cast<GlobalVariable>(ClsCE->getOperand(0));
      ConstantStruct *ClsCS =
        dyn_cast<ConstantStruct>(ClsCEGV->getInitializer());

      GlobalVariable *SupClsGV =
        (ClsCS->getOperand(1) == NULL)
          ? NULL
          : dyn_cast<GlobalVariable>(ClsCS->getOperand(1));

      std::string ClsName = ClsCEGV->getName().str();
      ClsName.replace(ClsName.find("OBJC_CLASS_$_"),
                      sizeof("OBJC_CLASS_$_") - 1,
                      ""); // remove OBJC_CLASS_$_ prefix

      // We need to handle classes that does not have a base.
      std::string SupClsName = "";
      if (SupClsGV != NULL) {
        SupClsName = SupClsGV->getName().str();
        SupClsName.replace(SupClsName.find("OBJC_CLASS_$_"),
                           sizeof("OBJC_CLASS_$_") - 1,
                           ""); // remove OBJC_CLASS_$_ prefix
      }

      Cls2SupCls[ClsName] = SupClsName;
      ClsName2GV[ClsName] = ClsCEGV;

      if (SupClsName.empty() /* NULL Super Class */ ||
          (SupClsGV != NULL &&
           !SupClsGV->hasInitializer() /* External Super Class */)) {
        RdyClsNames.push_back(ClsName);
      } else {
        NotRdyClsNames.push_back(ClsName);
      }
    }

    // Sort initialized sequence based on dependencies
    while (NotRdyClsNames.size() > 0) {

      std::string NotRdyClsName = NotRdyClsNames.front();
      NotRdyClsNames.pop_front();

      std::string NotRdySupClsName = Cls2SupCls[NotRdyClsName];
      if (!NotRdySupClsName.empty() &&
          std::find(RdyClsNames.begin(), RdyClsNames.end(), NotRdySupClsName) ==
            RdyClsNames.end()) {
        // Super class is an uninitialized non-null class.
        // Push back and waiting until base class is allocated.
        NotRdyClsNames.push_back(NotRdyClsName);
      } else {
        // Base class is ready.
        RdyClsNames.push_back(NotRdyClsName);
      }
    }

    // Now run handleClass() for each class.
    for (std::string ReadyClsName : RdyClsNames) {
      handleClass(ClsName2GV[ReadyClsName], &M);
    }

    return true;
  }

  void handleClass(GlobalVariable *ClsGV, Module *M) {

    ObfuscatedClassCounter++;

    assert(ClsGV->hasInitializer() &&
           "The initializer of ObjC class structure is missing.");

    // struct _class_t {
    //   struct _class_t *isa;
    //   struct _class_t *const superclass;
    //   void *cache;
    //   IMP *vtable;
    //   struct class_ro_t *ro;
    // }

    StringRef ClsName = ClsGV->getName();
    ClsName = ClsName.substr(sizeof("OBJC_CLASS_$_") -
                             1); // remove OBJC_CLASS_$_ prefix

    ConstantStruct *ClsCS = dyn_cast<ConstantStruct>(ClsGV->getInitializer());
    StringRef SupClsName = ClsCS->getOperand(1)->getName();
    SupClsName = SupClsName.substr(sizeof("OBJC_CLASS_$_") -
                                   1); // remove OBJC_CLASS_$_ prefix

    DEBUG_WITH_TYPE(DEBUG_TYPE, errs() << "Handling class " << ClsName
                                       << " with its super class " << SupClsName
                                       << "\n");

    GlobalVariable *MetaClassGV =
      cast<GlobalVariable>(ClsCS->getOperand(0)); // struct _class_t *isa
    assert(MetaClassGV->hasInitializer() &&
           "The initializer of meta class is missing.");

    GlobalVariable *MetaClassRoGV =
      cast<GlobalVariable>(MetaClassGV->getInitializer()->getOperand(
        MetaClassGV->getInitializer()->getNumOperands() - 1));

    GlobalVariable *ClassRoGV =
      cast<GlobalVariable>(ClsCS->getOperand(4)); // struct class_ro_t *ro

    // {METHODS, IVARS, PROPS}
    std::map<std::string, Value *> ClassRoInfo = extractClassRoTStruct(
      cast<ConstantStruct>(MetaClassRoGV->getInitializer()), M);

    // Find entry basic block
    BasicBlock *EntryBB = NULL;
    Function *AnyFunc = NULL;
    if (ClassRoInfo.find("METHODS") != ClassRoInfo.end()) {
      ConstantArray *MethodList = cast<ConstantArray>(ClassRoInfo["METHODS"]);
      for (unsigned Mi = 0; Mi < MethodList->getNumOperands(); Mi++) {

        // MethodStruct has type %struct._objc_method = type { i8*, i8*, i8* }
        // {GEP(NAME), GEP(TYPE), BitCast(IMP)}
        ConstantStruct *MethodStruct =
          cast<ConstantStruct>(MethodList->getOperand(Mi));

        // MethodStruct->getOperand(0)->getOperand(0) is SEL name.
        GlobalVariable *SelNameGV =
          cast<GlobalVariable>(MethodStruct->getOperand(0)->getOperand(0));
        ConstantDataSequential *SelNameCDS =
          cast<ConstantDataSequential>(SelNameGV->getInitializer());
        StringRef SelName = SelNameCDS->getAsCString();
        if ((SelName == StringRef("initialize") && UseInitialize) ||
            (SelName == StringRef("load") && !UseInitialize)) {
          Function *IMPFunc =
            cast<Function>(MethodStruct->getOperand(2)->getOperand(0));
          DEBUG_WITH_TYPE(DEBUG_TYPE, errs() << "Found existing initializer\n");
          EntryBB = &(IMPFunc->getEntryBlock());
        }

        if (!AnyFunc) {
          AnyFunc = cast<Function>(MethodStruct->getOperand(2)->getOperand(0));
        }
      }
    } else {
      errs() << "Did not find class method list of class: " << ClsName << "\n";
    }

    bool RequiresTerminator = false;
    if (EntryBB == NULL) {
      RequiresTerminator = true;

      // Failed to find existing +initializer or +load, create a new one.
      DEBUG_WITH_TYPE(DEBUG_TYPE, errs() << "Creating initializer\n");

      Type *Int8PtrTy = Type::getInt8PtrTy(M->getContext());
      std::vector<Type *> InitializerParamTyVec;
      InitializerParamTyVec.push_back(Int8PtrTy);
      InitializerParamTyVec.push_back(Int8PtrTy);

      FunctionType *InitializerTy =
        FunctionType::get(Type::getVoidTy(M->getContext()),
                          ArrayRef<Type *>(InitializerParamTyVec), false);
      Function *Initializer = Function::Create(
        InitializerTy, GlobalValue::LinkageTypes::InternalLinkage,
        "ObfsACDInitializer", M);
      Initializer->copyAttributesFrom(AnyFunc);

      EntryBB = BasicBlock::Create(M->getContext(), "ObfsACDInitializerBB",
                                   Initializer);
    }

    if (RequiresTerminator) {
      IRBuilder<> Foo(EntryBB);
      Foo.CreateRetVoid();
    }

    // Begin IRBuilder
    IRBuilder<> *IRB = new IRBuilder<>(EntryBB, EntryBB->getFirstInsertionPt());

    // Prepare ObjC API definitions
    Function *ObjCGetClassFunc = M->getFunction("objc_getClass");
    Constant *ClsNameGV = IRB->CreateGlobalStringPtr(ClsName);

    CallInst *ObjCGetClassCallI =
      IRB->CreateCall(ObjCGetClassFunc, {ClsNameGV});
    ConstantStruct *MetaClassRoCS =
      cast<ConstantStruct>(MetaClassRoGV->getInitializer());
    ConstantStruct *ClassRoCS =
      cast<ConstantStruct>(ClassRoGV->getInitializer());

    // TODO:
    //  Now scan for props and ivars in OBJC_CLASS_RO and OBJC_METACLASS_RO.
    //  Note that class_ro_t's structure is different between 32 and 64 bit
    //  runtime.

    // struct class_ro_t {
    //     uint32_t flags;
    //     uint32_t instanceStart;
    //     uint32_t instanceSize;
    // #ifdef __LP64__
    //     uint32_t reserved;
    // #endif
    //
    //     const uint8_t * ivarLayout;
    //
    //     const char * name;
    //     method_list_t * baseMethodList;
    //     protocol_list_t * baseProtocols;
    //     const ivar_list_t * ivars;
    //
    //     const uint8_t * weakIvarLayout;
    //     property_list_t *baseProperties;
    // }

    // struct method_t {
    //     SEL name;
    //     const char *types;
    //     IMP imp;
    // }

    Type *Int32Ty = Type::getInt32Ty(M->getContext());
    StructType *ObjCMethodTy =
      StructType::getTypeByName(M->getContext(), "struct._objc_method");

    Triple Tri = Triple(M->getTargetTriple());
    Constant *ClsMethodList = ClassRoCS->getAggregateElement(5);
    if (!ClsMethodList->isNullValue()) {

      errs() << "Handling instance methods of class: " << ClsName << "\n";
      handleMethods(ClassRoCS, IRB, M, ObjCGetClassCallI, false);

      errs() << "Updating instance method map of class: " << ClsName << "\n";

      // typedef struct method_list_t {
      //     uint32_t entsize_NEVER_USE;  // low 2 bits used for fixup markers
      //     uint32_t count;
      //     struct method_t first;
      // } method_list_t;
      GlobalVariable *MethodListGV =
        cast<GlobalVariable>(ClsMethodList->getOperand(0));
      StructType *MethodListGVTy =
        cast<StructType>(MethodListGV->getInitializer()->getType());

      // New method array
      ArrayType *NewMethodArrTy = ArrayType::get(ObjCMethodTy, 0);
      Constant *NewMethodArr =
        ConstantArray::get(NewMethodArrTy, ArrayRef<Constant *>());

      // New method list types
      std::vector<Type *> NewMethodListTyVec;
      NewMethodListTyVec.push_back(MethodListGVTy->getElementType(0));
      NewMethodListTyVec.push_back(MethodListGVTy->getElementType(1));
      NewMethodListTyVec.push_back(NewMethodArrTy);

      // New method list values
      std::vector<Constant *> NewMethodListValVec;
      NewMethodListValVec.push_back(
        MethodListGV->getInitializer()->getAggregateElement(0u));
      NewMethodListValVec.push_back(
        ConstantInt::get(MethodListGVTy->getElementType(1), 0)); // count = 0
      NewMethodListValVec.push_back(NewMethodArr);

      StructType *NewMethodListTy =
        StructType::get(M->getContext(), ArrayRef<Type *>(NewMethodListTyVec));
      Constant *NewMethodList = ConstantStruct::get(
        NewMethodListTy, ArrayRef<Constant *>(NewMethodListValVec));
      GlobalVariable *NewMethodListGV = new GlobalVariable(
        *M, NewMethodListTy, true, GlobalValue::LinkageTypes::PrivateLinkage,
        NewMethodList, "ObfsACDNewInstanceMethodMap");
      appendToCompilerUsed(*M, {NewMethodListGV});

      NewMethodListGV->copyAttributesFrom(MethodListGV);

      Constant *BitCastExpr = ConstantExpr::getBitCast(
        NewMethodListGV,
        StructType::getTypeByName(M->getContext(), "struct.__method_list_t")
          ->getPointerTo());

      ClassRoCS->handleOperandChange(ClsMethodList, BitCastExpr);

      MethodListGV->replaceAllUsesWith(
        ConstantExpr::getBitCast(NewMethodListGV, MethodListGV->getType()));
      MethodListGV->dropAllReferences();
      MethodListGV->eraseFromParent();

      errs() << "Updated instance method map of class: " << ClassRoGV->getName()
             << "\n";
    }

    Constant *MetaClsMethodList = MetaClassRoCS->getAggregateElement(5);
    if (!MetaClsMethodList->isNullValue()) {

      errs() << "Handling class methods of class: " << ClsName << "\n";
      handleMethods(MetaClassRoCS, IRB, M, ObjCGetClassCallI, true);

      errs() << "Updating class method map of class: " << ClsName << "\n";

      GlobalVariable *MethodListGV =
        cast<GlobalVariable>(MetaClsMethodList->getOperand(0));

      Constant *LoadMetaMethodName = NULL;
      if (UseInitialize) {
        LoadMetaMethodName =
          cast<Constant>(IRB->CreateGlobalStringPtr("initialize"));
      } else {
        LoadMetaMethodName = cast<Constant>(IRB->CreateGlobalStringPtr("load"));
      }

      // This method signature is generated by clang
      // ASTContext::getObjCEncodingForMethodDecl
      Constant *LoadMetaMethodType = NULL;
      if (Tri.isOSDarwin() && Tri.isArch64Bit()) {
        LoadMetaMethodType =
          cast<Constant>(IRB->CreateGlobalStringPtr("v16@0:8"));
      } else if (Tri.isOSDarwin() && Tri.isArch32Bit()) {
        LoadMetaMethodType =
          cast<Constant>(IRB->CreateGlobalStringPtr("v8@0:4"));
      } else {
        errs() << "Unknown platform. Blindly applying method signature for "
                  "macOS 64 bit\n";
        LoadMetaMethodType =
          cast<Constant>(IRB->CreateGlobalStringPtr("v16@0:8"));
      }

      Constant *BitCastedIMP = cast<Constant>(IRB->CreateBitCast(
        IRB->GetInsertBlock()->getParent(),
        ObjCGetClassFunc->getFunctionType()->getParamType(0)));

      std::vector<Constant *> LoadMetaMethodStructValVec;
      LoadMetaMethodStructValVec.push_back(LoadMetaMethodName);
      LoadMetaMethodStructValVec.push_back(LoadMetaMethodType);
      LoadMetaMethodStructValVec.push_back(BitCastedIMP);

      // New load method
      Constant *NewLoadMetaMethod =
        ConstantStruct::get(cast<StructType>(ObjCMethodTy),
                            ArrayRef<Constant *>(LoadMetaMethodStructValVec));

      // New method array
      ArrayType *NewMethodArrTy = ArrayType::get(ObjCMethodTy, 1);
      Constant *NewMethodArr = ConstantArray::get(
        NewMethodArrTy, ArrayRef<Constant *>(NewLoadMetaMethod));

      std::vector<Type *> NewMethodListTyVec;
      NewMethodListTyVec.push_back(Int32Ty); // uint32_t entsize_NEVER_USE
      NewMethodListTyVec.push_back(Int32Ty); // uint32_t count
      NewMethodListTyVec.push_back(NewMethodArrTy);

      std::vector<Constant *> NewMethodListValVec;
      NewMethodListValVec.push_back(
        ConstantInt::get(Int32Ty, 0x18)); // uint32_t entsize_NEVER_USE
      NewMethodListValVec.push_back(ConstantInt::get(Int32Ty, 1)); // count = 1
      NewMethodListValVec.push_back(NewMethodArr);

      StructType *NewMethodListTy =
        StructType::get(M->getContext(), ArrayRef<Type *>(NewMethodListTyVec));
      Constant *NewMethodList = ConstantStruct::get(
        NewMethodListTy, ArrayRef<Constant *>(NewMethodListValVec));
      GlobalVariable *NewMethodListGV = new GlobalVariable(
        *M, NewMethodListTy, true, GlobalValue::LinkageTypes::PrivateLinkage,
        NewMethodList, "ObfsACDNewClassMethodMap");
      appendToCompilerUsed(*M, {NewMethodListGV});

      NewMethodListGV->copyAttributesFrom(MethodListGV);

      Constant *BitCastExpr = ConstantExpr::getBitCast(
        NewMethodListGV,
        StructType::getTypeByName(M->getContext(), "struct.__method_list_t")
          ->getPointerTo());

      MetaClassRoCS->handleOperandChange(MetaClsMethodList, BitCastExpr);

      MethodListGV->replaceAllUsesWith(
        ConstantExpr::getBitCast(NewMethodListGV, MethodListGV->getType()));
      MethodListGV->dropAllReferences();
      MethodListGV->eraseFromParent();

      errs() << "Updated class method map of class: " << ClassRoGV->getName()
             << "\n";
    }
  }

  // Split a class_ro_t structure into a map of values.
  std::map<std::string, Value *>
  extractClassRoTStruct(ConstantStruct *ClassRoStruct, Module *M) {
    std::map<std::string, Value *> ClsInfoMap;
    PointerType *MethodListPtrTy =
      StructType::getTypeByName(M->getContext(), "struct.__method_list_t")
        ->getPointerTo();
    PointerType *IvarListPtrTy =
      StructType::getTypeByName(M->getContext(), "struct._ivar_list_t")
        ->getPointerTo();
    PointerType *PropListPtrTy =
      StructType::getTypeByName(M->getContext(), "struct._prop_list_t")
        ->getPointerTo();

    for (unsigned Ci = 0; Ci < ClassRoStruct->getType()->getNumElements();
         Ci++) {

      Constant *TmpC =
        dyn_cast<Constant>(ClassRoStruct->getAggregateElement(Ci));
      if (!TmpC || TmpC->isNullValue()) {
        continue;
      }

      Type *TmpTy = TmpC->getType();
      if (TmpTy == IvarListPtrTy) {
        // TODO: not implemented
        ClsInfoMap["IVARS"] = cast<ConstantExpr>(TmpC);
      } else if (TmpTy == PropListPtrTy) {
        // TODO: not implemented
        ClsInfoMap["PROPS"] = cast<ConstantExpr>(TmpC);
      } else if (TmpTy == MethodListPtrTy) {

        // Insert Methods
        ConstantExpr *MethodListCE = cast<ConstantExpr>(TmpC);

        // Note: MethodListCE is also a BitCastConstantExpr
        GlobalVariable *MethodListGV =
          cast<GlobalVariable>(MethodListCE->getOperand(0));

        // Now BitCast is stripped out.
        assert(MethodListGV->hasInitializer() &&
               "MethodListGV does not have an initializer.");
        ConstantStruct *MethodListCS =
          cast<ConstantStruct>(MethodListGV->getInitializer());

        // Extracting %struct._objc_method array from %struct.__method_list_t =
        // type { i32, i32, [0 x %struct._objc_method] }
        ClsInfoMap["METHODS"] =
          cast<ConstantArray>(MethodListCS->getOperand(2));
      }
    }

    return ClsInfoMap;
  }

  void handleMethods(ConstantStruct *ClassRoStruct, IRBuilder<> *IRB, Module *M,
                     CallInst *ObjCGetClassCallI, bool IsMetaClass) {

    Function *SelRegisterNameFunc = M->getFunction("sel_registerName");
    Function *ClassReplaceMethodFunc = M->getFunction("class_replaceMethod");
    Function *ClassGetNameFunc = M->getFunction("class_getName");
    Function *ObjCGetMetaClassFunc = M->getFunction("objc_getMetaClass");

    PointerType *MethodListPtrTy =
      StructType::getTypeByName(M->getContext(), "struct.__method_list_t")
        ->getPointerTo();

    for (unsigned Ci = 0; Ci < ClassRoStruct->getType()->getNumElements();
         Ci++) {

      Constant *TmpC =
        dyn_cast<Constant>(ClassRoStruct->getAggregateElement(Ci));
      if (!TmpC || TmpC->isNullValue()) {
        continue;
      }

      Type *TmpTy = TmpC->getType();
      if (TmpTy == MethodListPtrTy) {

        // Insert Methods
        ConstantExpr *MethodListCE = cast<ConstantExpr>(TmpC);

        // Note: MethodListCE is also a BitCastConstantExpr.
        GlobalVariable *MethodListGV =
          dyn_cast<GlobalVariable>(MethodListCE->getOperand(0));

        // Now BitCast is stripped out.
        assert(MethodListGV->hasInitializer() &&
               "MethodListGV does not have an initializer.");
        ConstantStruct *MethodListStruct =
          cast<ConstantStruct>(MethodListGV->getInitializer());

        // Extracting %struct._objc_method array from %struct.__method_list_t =
        // type { i32, i32, [0 x %struct._objc_method] }
        if (MethodListStruct->getOperand(2)->isZeroValue()) {
          return;
        }

        ConstantArray *MethodList =
          cast<ConstantArray>(MethodListStruct->getOperand(2));
        for (unsigned Mi = 0; Mi < MethodList->getNumOperands(); Mi++) {

          // MethodStruct has type %struct._objc_method = type { i8*, i8*, i8*
          // } which contains {GEP(NAME), GEP(TYPE), IMP}.
          ConstantStruct *MethodStruct =
            cast<ConstantStruct>(MethodList->getOperand(Mi));

          // Class cls
          std::vector<Value *> ReplaceMethodArgs;
          if (IsMetaClass) {

            // const char * class_getName(Class cls);
            CallInst *ClassGetNameCallI =
              IRB->CreateCall(ClassGetNameFunc, {ObjCGetClassCallI});

            // Class objc_getMetaClass(const char *name);
            CallInst *ObjCGetMetaClassCallI =
              IRB->CreateCall(ObjCGetMetaClassFunc, {ClassGetNameCallI});

            ReplaceMethodArgs.push_back(ObjCGetMetaClassCallI);
            ObfuscatedClassMethodCounter++;
          } else {
            ReplaceMethodArgs.push_back(ObjCGetClassCallI);
            ObfuscatedInstanceMethodCounter++;
          }

          // SEL name
          // SEL sel_registerName(const char *str);
          CallInst *SelCallI =
            IRB->CreateCall(SelRegisterNameFunc, {MethodStruct->getOperand(0)});
          ReplaceMethodArgs.push_back(SelCallI);

          // IMP imp
          Type *IMPType =
            ClassReplaceMethodFunc->getFunctionType()->getParamType(2);
          Value *BitCastedIMP =
            IRB->CreateBitCast(MethodStruct->getOperand(2), IMPType);
          ReplaceMethodArgs.push_back(BitCastedIMP);

          // const char *types
          ReplaceMethodArgs.push_back(MethodStruct->getOperand(1));

          // IMP class_replaceMethod(Class cls, SEL name, IMP imp, const char
          // *types);
          IRB->CreateCall(ClassReplaceMethodFunc,
                          ArrayRef<Value *>(ReplaceMethodArgs),
                          "ObfsACDReplaceCall");
        }
      }
    }
  }
};

} // namespace

char AntiClassDump::ID = 0;

// Register to opt
static RegisterPass<AntiClassDump> X(DEBUG_TYPE,
                                     "Enable protection against class dump");

// Register to clang
static RegisterStandardPasses Y(PassManagerBuilder::EP_OptimizerLast,
                                [](const PassManagerBuilder &Builder,
                                   legacy::PassManagerBase &PM) {
                                  PM.add(new AntiClassDump());
                                });
static RegisterStandardPasses Z(PassManagerBuilder::EP_EnabledOnOptLevel0,
                                [](const PassManagerBuilder &Builder,
                                   legacy::PassManagerBase &PM) {
                                  PM.add(new AntiClassDump());
                                });