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
