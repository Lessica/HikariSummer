if (WIN32 OR CYGWIN)
    set(LLVM_LINK_COMPONENTS Core Support)
endif ()

add_llvm_library(LLVMCryptoUtils SHARED BUILDTREE_ONLY
        CryptoUtils.cpp

        DEPENDS
            intrinsics_gen
        )
