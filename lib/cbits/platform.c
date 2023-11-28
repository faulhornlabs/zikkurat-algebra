
#include "platform.h"

uint8_t wrap_addcarry_u64( uint8_t carry, uint64_t arg1, uint64_t arg2, uint64_t *tgt ) {
  return addcarry_u64( carry, arg1, arg2, tgt );
}

uint8_t wrap_subborrow_u64( uint8_t carry, uint64_t arg1, uint64_t arg2, uint64_t *tgt ) {
  return subborrow_u64( carry, arg1, arg2, tgt );
}

uint8_t wrap_addcarry_u128_inplace( uint64_t *tgt_lo, uint64_t *tgt_hi, uint64_t arg_lo, uint64_t arg_hi) {
  return addcarry_u128_inplace( tgt_lo, tgt_hi, arg_lo, arg_hi); 
}

