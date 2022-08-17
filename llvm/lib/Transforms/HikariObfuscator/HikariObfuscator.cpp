/*
 * Hikari's own "Pass Scheduler".
 * Because currently there is no way to add dependency to transform passes.
 * Ref: http://lists.llvm.org/pipermail/llvm-dev/2011-February/038109.html
 */

#include "HikariObfuscator.h"
#include "AntiClassDump/AntiClassDump.h"
#include "BogusControlFlow/BogusControlFlow.h"
#include "Flattening/Flattening.h"
#include "FunctionCallObfuscate/FunctionCallObfuscate.h"
#include "FunctionWrapper/FunctionWrapper.h"
#include "IndirectBranch/IndirectBranch.h"
#include "SplitBasicBlocks/SplitBasicBlocks.h"
#include "StringEncryption/StringEncryption.h"
#include "Substitution/Substitution.h"
#include "llvm/Support/CommandLine.h"

using namespace llvm;

static cl::opt<bool>
  AntiClassDumpEnabled("enable-acdobf", cl::NotHidden,
                       cl::desc(ANTICLASSDUMP_PASS_DESCRIPTION),
                       cl::init(false), cl::Optional);

static cl::opt<bool>
  BogusControlFlowEnabled("enable-bcfobf", cl::NotHidden,
                          cl::desc(BOGUSCONTROLFLOW_PASS_DESCRIPTION),
                          cl::init(false), cl::Optional);

static cl::opt<bool> FlatteningEnabled("enable-cffobf", cl::NotHidden,
                                       cl::desc(FLATTENING_PASS_DESCRIPTION),
                                       cl::init(false), cl::Optional);

static cl::opt<bool>
  FunctionCallObfuscateEnabled("enable-fco", cl::NotHidden,
                               cl::desc(FUNCTIONCALLOBFUSCATE_PASS_DESCRIPTION),
                               cl::init(false), cl::Optional);

static cl::opt<bool>
  FunctionWrapperEnabled("enable-funwra", cl::NotHidden,
                         cl::desc(FUNCTIONWRAPPER_PASS_DESCRIPTION),
                         cl::init(false), cl::Optional);

static cl::opt<bool>
  IndirectBranchEnabled("enable-indbra", cl::NotHidden,
                        cl::desc(INDIRECTBRANCH_PASS_DESCRIPTION),
                        cl::init(false), cl::Optional);

static cl::opt<bool>
  SplitBasicBlocksEnabled("enable-split", cl::NotHidden,
                          cl::desc(SPLITBASICBLOCKS_PASS_DESCRIPTION),
                          cl::init(false), cl::Optional);

static cl::opt<bool>
  StringEncryptionEnabled("enable-strcry", cl::NotHidden,
                          cl::desc(STRINGENCRYPTION_PASS_DESCRIPTION),
                          cl::init(false), cl::Optional);

static cl::opt<bool>
  SubstitutionEnabled("enable-subobf", cl::NotHidden,
                      cl::desc(SUBSTITUTION_PASS_DESCRIPTION), cl::init(false),
                      cl::Optional);
