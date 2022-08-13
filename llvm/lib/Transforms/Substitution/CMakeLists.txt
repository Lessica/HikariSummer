if(WIN32 OR CYGWIN)
    set(LLVM_LINK_COMPONENTS Core Support)
endif()

add_llvm_library( LLVMSubstitution MODULE BUILDTREE_ONLY
        Substitution.cpp

        DEPENDS
        intrinsics_gen
        PLUGIN_TOOL
        opt
        )
