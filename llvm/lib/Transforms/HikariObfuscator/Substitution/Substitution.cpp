#include "Substitution.h"
#include "../CryptoUtils/CryptoUtils.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/Support/CommandLine.h"

using namespace llvm;

#define DEBUG_TYPE "obfs-sub"

#define NUMBER_ADD_SUBST 4
#define NUMBER_SUB_SUBST 3
#define NUMBER_AND_SUBST 2
#define NUMBER_OR_SUBST 2
#define NUMBER_XOR_SUBST 2

static cl::opt<unsigned> ProbabilityRate(
  DEBUG_TYPE "-prob", cl::NotHidden,
  cl::desc(
    "The probability (%) each function will be obfuscated by the " DEBUG_TYPE
    " pass"),
  cl::init(50), cl::Optional);

static cl::opt<unsigned> LoopTimes(DEBUG_TYPE "-loop", cl::NotHidden,
                                   cl::desc("Number of times the " DEBUG_TYPE
                                            " pass will loop on a function"),
                                   cl::init(1), cl::Optional);

STATISTIC(AddCounter, "Add instruction substituted");
STATISTIC(SubCounter, "Sub instruction substituted");
// STATISTIC(MulCounter, "Mul instruction substituted");
// STATISTIC(DivCounter, "Div instruction substituted");
// STATISTIC(RemCounter, "Rem instruction substituted");
// STATISTIC(ShiCounter, "Shift instruction substituted");
STATISTIC(AndCounter, "And instruction substituted");
STATISTIC(OrCounter, "Or instruction substituted");
STATISTIC(XorCounter, "Xor instruction substituted");

namespace {

struct Substitution : public FunctionPass {
  static char ID; // Pass identification, replacement for typeid

  void (Substitution::*FuncAdd[NUMBER_ADD_SUBST])(BinaryOperator *BinOp);
  void (Substitution::*FuncSub[NUMBER_SUB_SUBST])(BinaryOperator *BinOp);
  void (Substitution::*FuncAnd[NUMBER_AND_SUBST])(BinaryOperator *BinOp);
  void (Substitution::*FuncOr[NUMBER_OR_SUBST])(BinaryOperator *BinOp);
  void (Substitution::*FuncXor[NUMBER_XOR_SUBST])(BinaryOperator *BinOp);

  Substitution() : FunctionPass(ID) {
    llvm::SharedCryptoUtils->prng_seed();

    FuncAdd[0] = &Substitution::addNegSubstitution;
    FuncAdd[1] = &Substitution::addDoubleNegSubstitution;
    FuncAdd[2] = &Substitution::addRandomSubstitution;
    FuncAdd[3] = &Substitution::addRandomSubstitution2;

    FuncSub[0] = &Substitution::subNegSubstitution;
    FuncSub[1] = &Substitution::subRandomSubstitution;
    FuncSub[2] = &Substitution::subRandomSubstitution2;

    FuncAnd[0] = &Substitution::andSubstitution;
    FuncAnd[1] = &Substitution::andRandomSubstitution;

    FuncOr[0] = &Substitution::orSubstitution;
    FuncOr[1] = &Substitution::orRandomSubstitution;

    FuncXor[0] = &Substitution::xorSubstitution;
    FuncXor[1] = &Substitution::xorRandomSubstitution;
  }

  bool runOnFunction(Function &F) override {
    if (LoopTimes == 0) {
      errs() << DEBUG_TYPE "-loop should be greater than 0\n";
      return false;
    }
    if (ProbabilityRate > 100) {
      errs() << DEBUG_TYPE "-prob should be less than 100\n";
      return false;
    }
    return doSubstitution(&F);
  }

  bool doSubstitution(Function *F) {
    Function *FuncTmp = F;

    // Loop for the number of time we run the pass on the function
    bool FuncChanged = false;
    int LoopT = LoopTimes;
    do {
      for (Function::iterator Bb = FuncTmp->begin(), Be = FuncTmp->end();
           Bb != Be; ++Bb) {
        for (BasicBlock::iterator BBi = Bb->begin(), BBe = Bb->end();
             BBi != BBe; ++BBi) {
          if ((BBi->isBinaryOp() || BBi->isUnaryOp()) &&
              llvm::SharedCryptoUtils->get_range(100) <= ProbabilityRate) {
            switch (BBi->getOpcode()) {
            case BinaryOperator::Add:
              // case BinaryOperator::FAdd:
              (this->*FuncAdd[llvm::SharedCryptoUtils->get_range(
                        NUMBER_ADD_SUBST)])(cast<BinaryOperator>(BBi));
              ++AddCounter;
              FuncChanged = true;
              break;
            case BinaryOperator::Sub:
              // case BinaryOperator::FSub:
              (this->*FuncSub[llvm::SharedCryptoUtils->get_range(
                        NUMBER_SUB_SUBST)])(cast<BinaryOperator>(BBi));
              ++SubCounter;
              FuncChanged = true;
              break;
            case BinaryOperator::Mul:
            case BinaryOperator::FMul:
              break;
            case BinaryOperator::UDiv:
            case BinaryOperator::SDiv:
            case BinaryOperator::FDiv:
              break;
            case BinaryOperator::URem:
            case BinaryOperator::SRem:
            case BinaryOperator::FRem:
              break;
            case Instruction::Shl:
              break;
            case Instruction::LShr:
              break;
            case Instruction::AShr:
              break;
            case Instruction::And:
              (this->*FuncAnd[llvm::SharedCryptoUtils->get_range(
                        NUMBER_AND_SUBST)])(cast<BinaryOperator>(BBi));
              ++AndCounter;
              FuncChanged = true;
              break;
            case Instruction::Or:
              (this->*FuncOr[llvm::SharedCryptoUtils->get_range(
                        NUMBER_OR_SUBST)])(cast<BinaryOperator>(BBi));
              ++OrCounter;
              FuncChanged = true;
              break;
            case Instruction::Xor:
              (this->*FuncXor[llvm::SharedCryptoUtils->get_range(
                        NUMBER_XOR_SUBST)])(cast<BinaryOperator>(BBi));
              ++XorCounter;
              FuncChanged = true;
              break;
            default:
              break;
            }              // End switch
          }                // End isBinaryOp
        }                  // End for BasicBlock
      }                    // End for Function
    } while (--LoopT > 0); // End for LoopT
    return FuncChanged;
  }

  // a = b - (-c)
  void addNegSubstitution(BinaryOperator *BinOp) {
    Instruction *Op = NULL;
    if (BinOp->getOpcode() == Instruction::Add) {
      Op = BinaryOperator::CreateNeg(BinOp->getOperand(1), "", BinOp);
      Op = BinaryOperator::Create(Instruction::Sub, BinOp->getOperand(0), Op,
                                  "", BinOp);
    } else {
      Op = UnaryOperator::CreateFNeg(BinOp->getOperand(1), "", BinOp);
      Op = BinaryOperator::Create(Instruction::FSub, BinOp->getOperand(0), Op,
                                  "", BinOp);
    }
    BinOp->replaceAllUsesWith(Op);
  }

  // a = -(-b + (-c))
  void addDoubleNegSubstitution(BinaryOperator *BinOp) {
    Instruction *Op, *Op2 = NULL;
    if (BinOp->getOpcode() == Instruction::Add) {
      Op = BinaryOperator::CreateNeg(BinOp->getOperand(0), "", BinOp);
      Op2 = BinaryOperator::CreateNeg(BinOp->getOperand(1), "", BinOp);
      Op = BinaryOperator::Create(Instruction::Add, Op, Op2, "", BinOp);
      Op = BinaryOperator::CreateNeg(Op, "", BinOp);
    } else { // Instruction::FAdd
      Op = UnaryOperator::CreateFNeg(BinOp->getOperand(0), "", BinOp);
      Op2 = UnaryOperator::CreateFNeg(BinOp->getOperand(1), "", BinOp);
      Op = BinaryOperator::Create(Instruction::FAdd, Op, Op2, "", BinOp);
      Op = UnaryOperator::CreateFNeg(Op, "", BinOp);
    }
    BinOp->replaceAllUsesWith(Op);
  }

  // r = rand (); a = b + r; a = a + c; a = a - r
  void addRandomSubstitution(BinaryOperator *BinOp) {
    Instruction *Op = NULL;
    Type *Ty = BinOp->getType();
    if (BinOp->getOpcode() == Instruction::Add) {
      ConstantInt *Co = (ConstantInt *)ConstantInt::get(
        Ty, llvm::SharedCryptoUtils->get_uint64_t());
      Op = BinaryOperator::Create(Instruction::Add, BinOp->getOperand(0), Co,
                                  "", BinOp);
      Op = BinaryOperator::Create(Instruction::Add, Op, BinOp->getOperand(1),
                                  "", BinOp);
      Op = BinaryOperator::Create(Instruction::Sub, Op, Co, "", BinOp);
    } else { // Instruction::FAdd
      ConstantFP *Co = (ConstantFP *)ConstantFP::get(
        Ty, (double)llvm::SharedCryptoUtils->get_uint64_t());
      Op = BinaryOperator::Create(Instruction::FAdd, BinOp->getOperand(0), Co,
                                  "", BinOp);
      Op = BinaryOperator::Create(Instruction::FAdd, Op, BinOp->getOperand(1),
                                  "", BinOp);
      Op = BinaryOperator::Create(Instruction::FSub, Op, Co, "", BinOp);
    }
    BinOp->replaceAllUsesWith(Op);
  }

  // r = rand (); a = b - r; a = a + b; a = a + r
  void addRandomSubstitution2(BinaryOperator *BinOp) {
    Instruction *Op = NULL;
    Type *Ty = BinOp->getType();
    if (BinOp->getOpcode() == Instruction::Add) {
      ConstantInt *Co = (ConstantInt *)ConstantInt::get(
        Ty, llvm::SharedCryptoUtils->get_uint64_t());
      Op = BinaryOperator::Create(Instruction::Sub, BinOp->getOperand(0), Co,
                                  "", BinOp);
      Op = BinaryOperator::Create(Instruction::Add, Op, BinOp->getOperand(1),
                                  "", BinOp);
      Op = BinaryOperator::Create(Instruction::Add, Op, Co, "", BinOp);
    } else {
      ConstantFP *Co = (ConstantFP *)ConstantFP::get(
        Ty, (double)llvm::SharedCryptoUtils->get_uint64_t());
      Op = BinaryOperator::Create(Instruction::FAdd, BinOp->getOperand(0), Co,
                                  "", BinOp);
      Op = BinaryOperator::Create(Instruction::FAdd, Op, BinOp->getOperand(1),
                                  "", BinOp);
      Op = BinaryOperator::Create(Instruction::FSub, Op, Co, "", BinOp);
    }
    BinOp->replaceAllUsesWith(Op);
  }

  // a = b + (-c)
  void subNegSubstitution(BinaryOperator *BinOp) {
    Instruction *Op = NULL;
    if (BinOp->getOpcode() == Instruction::Sub) {
      Op = BinaryOperator::CreateNeg(BinOp->getOperand(1), "", BinOp);
      Op = BinaryOperator::Create(Instruction::Add, BinOp->getOperand(0), Op,
                                  "", BinOp);
    } else {
      Op = UnaryOperator::CreateFNeg(BinOp->getOperand(1), "", BinOp);
      Op = BinaryOperator::Create(Instruction::FAdd, BinOp->getOperand(0), Op,
                                  "", BinOp);
    }
    BinOp->replaceAllUsesWith(Op);
  }

  // r = rand (); a = b + r; a = a - c; a = a - r
  void subRandomSubstitution(BinaryOperator *BinOp) {
    Instruction *Op = NULL;
    Type *Ty = BinOp->getType();
    if (BinOp->getOpcode() == Instruction::Sub) {
      ConstantInt *Co = (ConstantInt *)ConstantInt::get(
        Ty, llvm::SharedCryptoUtils->get_uint64_t());
      Op = BinaryOperator::Create(Instruction::Add, BinOp->getOperand(0), Co,
                                  "", BinOp);
      Op = BinaryOperator::Create(Instruction::Sub, Op, BinOp->getOperand(1),
                                  "", BinOp);
      Op = BinaryOperator::Create(Instruction::Sub, Op, Co, "", BinOp);
    } else {
      ConstantFP *Co = (ConstantFP *)ConstantFP::get(
        Ty, (double)llvm::SharedCryptoUtils->get_uint64_t());
      Op = BinaryOperator::Create(Instruction::FAdd, BinOp->getOperand(0), Co,
                                  "", BinOp);
      Op = BinaryOperator::Create(Instruction::FSub, Op, BinOp->getOperand(1),
                                  "", BinOp);
      Op = BinaryOperator::Create(Instruction::FSub, Op, Co, "", BinOp);
    }
    BinOp->replaceAllUsesWith(Op);
  }

  // r = rand (); a = b - r; a = a - c; a = a + r
  void subRandomSubstitution2(BinaryOperator *BinOp) {
    Instruction *Op = NULL;
    Type *Ty = BinOp->getType();
    if (BinOp->getOpcode() == Instruction::Sub) {
      ConstantInt *Co = (ConstantInt *)ConstantInt::get(
        Ty, llvm::SharedCryptoUtils->get_uint64_t());
      Op = BinaryOperator::Create(Instruction::Sub, BinOp->getOperand(0), Co,
                                  "", BinOp);
      Op = BinaryOperator::Create(Instruction::Sub, Op, BinOp->getOperand(1),
                                  "", BinOp);
      Op = BinaryOperator::Create(Instruction::Add, Op, Co, "", BinOp);
    } else {
      ConstantFP *Co = (ConstantFP *)ConstantFP::get(
        Ty, (double)llvm::SharedCryptoUtils->get_uint64_t());
      Op = BinaryOperator::Create(Instruction::FSub, BinOp->getOperand(0), Co,
                                  "", BinOp);
      Op = BinaryOperator::Create(Instruction::FSub, Op, BinOp->getOperand(1),
                                  "", BinOp);
      Op = BinaryOperator::Create(Instruction::FAdd, Op, Co, "", BinOp);
    }
    BinOp->replaceAllUsesWith(Op);
  }

  // a = b & c => a = (b^~c)& b
  void andSubstitution(BinaryOperator *BinOp) {
    BinaryOperator *Op = NULL;

    // Create NOT on second operand => ~c
    Op = BinaryOperator::CreateNot(BinOp->getOperand(1), "", BinOp);

    // Create XOR => (b^~c)
    BinaryOperator *Op1 = BinaryOperator::Create(
      Instruction::Xor, BinOp->getOperand(0), Op, "", BinOp);

    // Create AND => (b^~c) & b
    Op = BinaryOperator::Create(Instruction::And, Op1, BinOp->getOperand(0), "",
                                BinOp);
    BinOp->replaceAllUsesWith(Op);
  }

  // a = a && b <=> !(!a | !b) && (r | !r)
  void andRandomSubstitution(BinaryOperator *BinOp) {

    // Copy of the BinaryOperator type to create the random number with the
    // same type of the operands
    Type *Ty = BinOp->getType();

    // r (Random number)
    ConstantInt *Co = (ConstantInt *)ConstantInt::get(
      Ty, llvm::SharedCryptoUtils->get_uint64_t());

    // !a
    BinaryOperator *Op =
      BinaryOperator::CreateNot(BinOp->getOperand(0), "", BinOp);

    // !b
    BinaryOperator *Op1 =
      BinaryOperator::CreateNot(BinOp->getOperand(1), "", BinOp);

    // !r
    BinaryOperator *Opr = BinaryOperator::CreateNot(Co, "", BinOp);

    // (!a | !b)
    BinaryOperator *Opa =
      BinaryOperator::Create(Instruction::Or, Op, Op1, "", BinOp);

    // (r | !r)
    Opr = BinaryOperator::Create(Instruction::Or, Co, Opr, "", BinOp);

    // !(!a | !b)
    Op = BinaryOperator::CreateNot(Opa, "", BinOp);

    // !(!a | !b) && (r | !r)
    Op = BinaryOperator::Create(Instruction::And, Op, Opr, "", BinOp);

    // Replace all the old AND operators with the new one transformed
    BinOp->replaceAllUsesWith(Op);
  }

  // a = (b & c) | (b ^ c)
  void orSubstitution(BinaryOperator *BinOp) {
    BinaryOperator *Op = NULL;

    // (b & c)
    Op = BinaryOperator::Create(Instruction::And, BinOp->getOperand(0),
                                BinOp->getOperand(1), "", BinOp);

    // (b ^ c)
    BinaryOperator *Op1 = BinaryOperator::Create(
      Instruction::Xor, BinOp->getOperand(0), BinOp->getOperand(1), "", BinOp);

    // a = (b & c) | (b ^ c)
    Op = BinaryOperator::Create(Instruction::Or, Op, Op1, "", BinOp);
    BinOp->replaceAllUsesWith(Op);
  }

  // [(!a && r) || (a && !r) ^ (!b && r) ||(b && !r) ] || [!(!a || !b) && (r ||
  // !r)]
  void orRandomSubstitution(BinaryOperator *BinOp) {

    Type *Ty = BinOp->getType();
    ConstantInt *Co = (ConstantInt *)ConstantInt::get(
      Ty, llvm::SharedCryptoUtils->get_uint64_t());

    // !a
    BinaryOperator *Op =
      BinaryOperator::CreateNot(BinOp->getOperand(0), "", BinOp);

    // !b
    BinaryOperator *Op1 =
      BinaryOperator::CreateNot(BinOp->getOperand(1), "", BinOp);

    // !r
    BinaryOperator *Op2 = BinaryOperator::CreateNot(Co, "", BinOp);

    // !a && r
    BinaryOperator *Op3 =
      BinaryOperator::Create(Instruction::And, Op, Co, "", BinOp);

    // a && !r
    BinaryOperator *Op4 = BinaryOperator::Create(
      Instruction::And, BinOp->getOperand(0), Op2, "", BinOp);

    // !b && r
    BinaryOperator *Op5 =
      BinaryOperator::Create(Instruction::And, Op1, Co, "", BinOp);

    // b && !r
    BinaryOperator *Op6 = BinaryOperator::Create(
      Instruction::And, BinOp->getOperand(1), Op2, "", BinOp);

    // (!a && r) || (a && !r)
    Op3 = BinaryOperator::Create(Instruction::Or, Op3, Op4, "", BinOp);

    // (!b && r) ||(b && !r)
    Op4 = BinaryOperator::Create(Instruction::Or, Op5, Op6, "", BinOp);

    // (!a && r) || (a && !r) ^ (!b && r) ||(b && !r)
    Op5 = BinaryOperator::Create(Instruction::Xor, Op3, Op4, "", BinOp);

    // !a || !b
    Op3 = BinaryOperator::Create(Instruction::Or, Op, Op1, "", BinOp);

    // !(!a || !b)
    Op3 = BinaryOperator::CreateNot(Op3, "", BinOp);

    // r || !r
    Op4 = BinaryOperator::Create(Instruction::Or, Co, Op2, "", BinOp);

    // !(!a || !b) && (r || !r)
    Op4 = BinaryOperator::Create(Instruction::And, Op3, Op4, "", BinOp);

    // [(!a && r) || (a && !r) ^ (!b && r) ||(b && !r) ] || [!(!a || !b) && (r
    // || !r)]
    Op = BinaryOperator::Create(Instruction::Or, Op5, Op4, "", BinOp);
    BinOp->replaceAllUsesWith(Op);
  }

  // a = a ~ b => a = (!a && b) || (a && !b)
  void xorSubstitution(BinaryOperator *BinOp) {
    BinaryOperator *Op = NULL;

    // !a
    Op = BinaryOperator::CreateNot(BinOp->getOperand(0), "", BinOp);

    // !a && b
    Op = BinaryOperator::Create(Instruction::And, BinOp->getOperand(1), Op, "",
                                BinOp);

    // !b
    BinaryOperator *Op1 =
      BinaryOperator::CreateNot(BinOp->getOperand(1), "", BinOp);

    // a && !b
    Op1 = BinaryOperator::Create(Instruction::And, BinOp->getOperand(0), Op1,
                                 "", BinOp);

    // (!a && b) || (a && !b)
    Op = BinaryOperator::Create(Instruction::Or, Op, Op1, "", BinOp);
    BinOp->replaceAllUsesWith(Op);
  }

  // (a ^ r) ^ (b ^ r) <=> (!a && r || a && !r) ^ (!b && r || b && !r)
  void xorRandomSubstitution(BinaryOperator *BinOp) {
    BinaryOperator *Op = NULL;

    Type *Ty = BinOp->getType();
    ConstantInt *Co = (ConstantInt *)ConstantInt::get(
      Ty, llvm::SharedCryptoUtils->get_uint64_t());

    // !a
    Op = BinaryOperator::CreateNot(BinOp->getOperand(0), "", BinOp);

    // !a && r
    Op = BinaryOperator::Create(Instruction::And, Co, Op, "", BinOp);

    // !r
    BinaryOperator *Opr = BinaryOperator::CreateNot(Co, "", BinOp);

    // a && !r
    BinaryOperator *Op1 = BinaryOperator::Create(
      Instruction::And, BinOp->getOperand(0), Opr, "", BinOp);

    // !b
    BinaryOperator *Op2 =
      BinaryOperator::CreateNot(BinOp->getOperand(1), "", BinOp);

    // !b && r
    Op2 = BinaryOperator::Create(Instruction::And, Op2, Co, "", BinOp);

    // b && !r
    BinaryOperator *Op3 = BinaryOperator::Create(
      Instruction::And, BinOp->getOperand(1), Opr, "", BinOp);

    // (!a && r) || (a && !r)
    Op = BinaryOperator::Create(Instruction::Or, Op, Op1, "", BinOp);

    // (!b && r) || (b && !r)
    Op1 = BinaryOperator::Create(Instruction::Or, Op2, Op3, "", BinOp);

    // (!a && r) || (a && !r) ^ (!b && r) || (b && !r)
    Op = BinaryOperator::Create(Instruction::Xor, Op, Op1, "", BinOp);
    BinOp->replaceAllUsesWith(Op);
  }
};

} // namespace

char Substitution::ID = 0;

// Register to opt
static RegisterPass<Substitution> X(DEBUG_TYPE, SUBSTITUTION_PASS_DESCRIPTION);

// Register to loader
FunctionPass *llvm::createSubstitutionPass() { return new Substitution(); }
INITIALIZE_PASS(Substitution, DEBUG_TYPE, SUBSTITUTION_PASS_DESCRIPTION, false,
                false);