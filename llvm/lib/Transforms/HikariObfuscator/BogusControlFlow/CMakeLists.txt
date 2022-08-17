if (WIN32 OR CYGWIN)
    set(LLVM_LINK_COMPONENTS Core Support)
endif ()

add_llvm_library(LLVMBogusControlFlow MODULE BUILDTREE_ONLY
        BogusControlFlow.cpp

        LINK_COMPONENTS
            CryptoUtils

        DEPENDS
            LLVMCryptoUtils
            intrinsics_gen

        PLUGIN_TOOL
            opt
        )
