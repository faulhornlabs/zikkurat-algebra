
// === platform-specific code ===

#include <stdint.h>

#ifdef ARCH_X86_64

// ------ intel/amd x86_64 with intrinsics ------

#include <x86intrin.h>

inline uint8_t addcarry_u64( uint8_t carry, uint64_t arg1, uint64_t arg2, uint64_t *tgt ) {
  return _addcarry_u64( carry, arg1, arg2, *tgt );
}

inline uint8_t subborrow_u64( uint8_t carry, uint64_t arg1, uint64_t arg2, uint64_t *tgt ) {
  return _subborrow_u64( carry, arg1, arg2, *tgt );
}

inline uint8_t addcarry_u128_inplace(  uint64_t *tgt_lo, uint64_t *tgt_hi, uint64_t arg_lo, uint64_t arg_hi) {
  uint8_t c;
  c = _addcarry_u64( 0, *tgt_lo, arg_lo, tgt_lo );
  c = _addcarry_u64( c, *tgt_hi, arg_hi, tgt_hi );
  return c;
}

#else

// ------ portable implementation for generic 64-bit architectures ------

inline uint8_t addcarry_u64( uint8_t carry, uint64_t arg1, uint64_t arg2, uint64_t *tgt ) {
  uint64_t u;
  u = arg1 + arg2 + carry;
  *tgt = u;
  return ( (carry) ? (u <= arg1) : (u < arg1) );
}

inline uint8_t subborrow_u64( uint8_t carry, uint64_t arg1, uint64_t arg2, uint64_t *tgt ) {
  uint64_t u;
  u = arg1 - arg2 - carry;
  *tgt = u;
  return ( (carry) ? (u >= arg1) : (u > arg1) );
}

inline uint8_t addcarry_u128_inplace( uint64_t *tgt_lo, uint64_t *tgt_hi, uint64_t arg_lo, uint64_t arg_hi) {
  uint8_t  c;
  uint64_t u,v;
  u = tgt_lo[0] + arg_lo;
  c = (u < arg_lo) ? 1 : 0; 
  c = addcarry_u64( c, tgt_hi[0], arg_hi, tgt_hi );
  *tgt_lo = u;
  *tgt_hi = v;
  return c;
}

#endif
