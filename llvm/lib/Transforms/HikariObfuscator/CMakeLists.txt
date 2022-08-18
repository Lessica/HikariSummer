add_subdirectory(CryptoUtils)
add_subdirectory(AntiClassDump)
add_subdirectory(BogusControlFlow)
add_subdirectory(Flattening)
add_subdirectory(FunctionCallObfuscate)
add_subdirectory(FunctionWrapper)
add_subdirectory(IndirectBranch)
add_subdirectory(SplitBasicBlocks)
add_subdirectory(StringEncryption)
add_subdirectory(Substitution)

if (WIN32 OR CYGWIN)
    set(LLVM_LINK_COMPONENTS Core Support)
endif ()

add_llvm_library(LLVMHikariObfuscator MODULE BUILDTREE_ONLY
        HikariObfuscator.cpp
        CryptoUtils/CryptoUtils.cpp
        AntiClassDump/AntiClassDump.cpp
        BogusControlFlow/BogusControlFlow.cpp
        Flattening/Flattening.cpp
        FunctionCallObfuscate/FunctionCallObfuscate.cpp
        FunctionWrapper/FunctionWrapper.cpp
        IndirectBranch/IndirectBranch.cpp
        SplitBasicBlocks/SplitBasicBlocks.cpp
        StringEncryption/StringEncryption.cpp
        Substitution/Substitution.cpp

        DEPENDS
        intrinsics_gen

        PLUGIN_TOOL
        opt
        )

target_compile_options(LLVMHikariObfuscator PRIVATE -Wno-covered-switch-default)

add_library(LLVMHanabiSupport SHARED
        ${CMAKE_CURRENT_LIST_DIR}/HanabiLoader/Dummy.cpp
        )

add_dependencies(LLVMHanabiSupport
        LLVMCore
        LLVMSupport
        )

add_llvm_library(LLVMHanabiLoader SHARED BUILDTREE_ONLY
        HanabiLoader/HanabiLoader.cpp
        HikariObfuscator.cpp
        CryptoUtils/CryptoUtils.cpp
        AntiClassDump/AntiClassDump.cpp
        BogusControlFlow/BogusControlFlow.cpp
        Flattening/Flattening.cpp
        FunctionCallObfuscate/FunctionCallObfuscate.cpp
        FunctionWrapper/FunctionWrapper.cpp
        IndirectBranch/IndirectBranch.cpp
        SplitBasicBlocks/SplitBasicBlocks.cpp
        StringEncryption/StringEncryption.cpp
        Substitution/Substitution.cpp

        DEPENDS
        LLVMHanabiSupport
        intrinsics_gen
        )

target_compile_options(LLVMHanabiLoader
        PRIVATE -Wno-dollar-in-identifier-extension
        PRIVATE -Wno-variadic-macros
        PRIVATE -Wno-covered-switch-default
        )

target_link_libraries(LLVMHanabiSupport
        PRIVATE
        LLVMCore
        LLVMSupport
        )

target_link_libraries(LLVMHanabiLoader
        PRIVATE
        ${CMAKE_CURRENT_LIST_DIR}/HanabiLoader/libsubstitute.dylib
        )

target_link_options(LLVMHanabiSupport
        PRIVATE -undefined
        PRIVATE dynamic_lookup
        PRIVATE -all_load
        )

target_link_options(LLVMHanabiLoader
        PRIVATE -undefined
        PRIVATE dynamic_lookup
        PRIVATE -flat_namespace
        )