#include "llvm/Transforms/CryptoUtils.h"

using namespace llvm;

namespace llvm {
ManagedStatic<CryptoUtils> SharedCryptoUtils;
} // namespace llvm

CryptoUtils::CryptoUtils() {}

uint32_t CryptoUtils::scramble32(
  uint32_t in, std::map<uint32_t /* IDX */, uint32_t /* VAL */> &VMap) {
  if (VMap.find(in) == VMap.end()) {
    uint32_t V = get_uint32_t();
    VMap[in] = V;
    return V;
  }
  return VMap[in];
}

CryptoUtils::~CryptoUtils() {
  if (eng != nullptr) {
    delete eng;
  }
}

void CryptoUtils::prng_seed() {
  using namespace std::chrono;
  std::uint_fast64_t ms =
    duration_cast<milliseconds>(system_clock::now().time_since_epoch()).count();
  eng = new std::mt19937_64(ms);
}

void CryptoUtils::prng_seed(std::uint_fast64_t seed) {
  eng = new std::mt19937_64(seed);
}

std::uint_fast64_t CryptoUtils::get_raw() {
  if (eng == nullptr) {
    prng_seed();
  }
  return (*eng)();
}

uint32_t CryptoUtils::get_range(uint32_t min, uint32_t max) {
  if (eng == nullptr) {
    prng_seed();
  }
  if (max == 0) {
    return 0;
  }
  std::uniform_int_distribution<uint32_t> dis(min, max - 1);
  return dis(*eng);
}
