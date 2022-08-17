if (WIN32 OR CYGWIN)
    set(LLVM_LINK_COMPONENTS Core Support)
endif ()

add_llvm_library(LLVMFunctionCallObfuscate MODULE BUILDTREE_ONLY
        FunctionCallObfuscate.cpp

        DEPENDS
            intrinsics_gen

        PLUGIN_TOOL
            opt
        )
