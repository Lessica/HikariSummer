if (WIN32 OR CYGWIN)
    set(LLVM_LINK_COMPONENTS Core Support)
endif ()

add_llvm_library(LLVMFunctionWrapper MODULE BUILDTREE_ONLY
        FunctionWrapper.cpp

        LINK_COMPONENTS
            CryptoUtils

        DEPENDS
            LLVMCryptoUtils
            intrinsics_gen

        PLUGIN_TOOL
            opt
        )
