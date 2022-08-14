if(WIN32 OR CYGWIN)
    set(LLVM_LINK_COMPONENTS Core Support)
endif()

add_llvm_library( LLVMStringEncryption MODULE BUILDTREE_ONLY
        StringEncryption.cpp

        DEPENDS
        intrinsics_gen
        PLUGIN_TOOL
        opt
        )
