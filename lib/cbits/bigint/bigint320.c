
// unsigned big integers composed of 5 64-bit words
//
// NOTE: generated code, do not edit!

#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <x86intrin.h>
#include <assert.h>
#include "bigint320.h"

#define NLIMBS 5

#define MIN(a,b) ( ((a)<=(b)) ? (a) : (b) )
#define MAX(a,b) ( ((a)>=(b)) ? (a) : (b) )

inline uint8_t addcarry_u128_inplace(  uint64_t *tgt_lo, uint64_t *tgt_hi, uint64_t arg_lo, uint64_t arg_hi) {
  uint8_t c;
  c = _addcarry_u64( 0, *tgt_lo, arg_lo, tgt_lo );
  c = _addcarry_u64( c, *tgt_hi, arg_hi, tgt_hi );
  return c;
}

//------------------------------------------------------------------------------

uint8_t bigint320_is_zero(const uint64_t *src) {
  return ( (src[0] == 0) && (src[1] == 0) && (src[2] == 0) && (src[3] == 0) && (src[4] == 0)) ;
}

uint8_t bigint320_is_one(const uint64_t *src) {
  return ( (src[0] == 1) && (src[1] == 0) && (src[2] == 0) && (src[3] == 0) && (src[4] == 0)) ;
}

uint8_t bigint320_is_equal(const uint64_t *src1, const uint64_t *src2 ) {
  return ( (src1[0] == src2[0]) && (src1[1] == src2[1]) && (src1[2] == src2[2]) && (src1[3] == src2[3]) && (src1[4] == src2[4])) ;
}

void bigint320_set_zero(uint64_t *tgt) {
  memset( tgt, 0, 40 );
}

void bigint320_set_one(uint64_t *tgt) {
  memset( tgt, 0, 40 );
  tgt[0] = 1;
}

void bigint320_set_small(uint64_t *tgt, uint64_t s) {
  memset( tgt, 0, 40 );
  tgt[0] = s;
}

void bigint320_copy(const uint64_t *src, uint64_t *tgt) {
  memcpy( tgt, src, 40 );
}

void bigint320_print(const uint64_t *what, int underscore_separators) {
  if (underscore_separators) {
    for(int i=0; i<5; i++) {
      printf("%016llx_", what[4-i]);
    }
  }
  else {
    for(int i=0; i<5; i++) {
      printf("%016llx", what[4-i]);
    }
  }
}

void bigint320_debug_print(const char *txt, const uint64_t *what) {
  printf("%s = 0x",txt);
  bigint320_print( what, 1 );
  printf("\n");
}

// increments bigint by 1, inplace
uint8_t bigint320_inc_inplace( uint64_t *tgt ) {
  uint8_t c = 0;
  c = _addcarry_u64( c, tgt[0], 1, tgt+0 );
  c = _addcarry_u64( c, tgt[1], 0, tgt+1 );
  c = _addcarry_u64( c, tgt[2], 0, tgt+2 );
  c = _addcarry_u64( c, tgt[3], 0, tgt+3 );
  c = _addcarry_u64( c, tgt[4], 0, tgt+4 );
  return c;
}

// decrements bigint by 1, inplace
uint8_t bigint320_dec_inplace( uint64_t *tgt ) {
  uint8_t b = 0;
  b = _subborrow_u64( b, tgt[0], 1, tgt+0 );
  b = _subborrow_u64( b, tgt[1], 0, tgt+1 );
  b = _subborrow_u64( b, tgt[2], 0, tgt+2 );
  b = _subborrow_u64( b, tgt[3], 0, tgt+3 );
  b = _subborrow_u64( b, tgt[4], 0, tgt+4 );
  return b;
}

// negates a bigint
void bigint320_neg( const uint64_t *src, uint64_t *tgt ) {
  uint8_t b = 0;
  b = _subborrow_u64( b, 0, src[0], tgt+0 );
  b = _subborrow_u64( b, 0, src[1], tgt+1 );
  b = _subborrow_u64( b, 0, src[2], tgt+2 );
  b = _subborrow_u64( b, 0, src[3], tgt+3 );
  b = _subborrow_u64( b, 0, src[4], tgt+4 );
}

// negates a bigint inplace
void bigint320_neg_inplace( uint64_t *tgt ) {
  uint8_t b = 0;
  b = _subborrow_u64( b, 0, tgt[0], tgt+0 );
  b = _subborrow_u64( b, 0, tgt[1], tgt+1 );
  b = _subborrow_u64( b, 0, tgt[2], tgt+2 );
  b = _subborrow_u64( b, 0, tgt[3], tgt+3 );
  b = _subborrow_u64( b, 0, tgt[4], tgt+4 );
}

// adds two (unsigned) big integers made up from 5 limbs (64-bit words)
uint8_t bigint320_add( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  uint8_t c = 0;
  c = _addcarry_u64( c, src1[0], src2[0],  tgt+0 );
  c = _addcarry_u64( c, src1[1], src2[1],  tgt+1 );
  c = _addcarry_u64( c, src1[2], src2[2],  tgt+2 );
  c = _addcarry_u64( c, src1[3], src2[3],  tgt+3 );
  c = _addcarry_u64( c, src1[4], src2[4],  tgt+4 );
  return c;
}

// adds two big integers made up from 5 limbs (64-bit words)
uint8_t bigint320_add_inplace( uint64_t *tgt, const uint64_t *src2 ) {
  uint8_t c = 0;
  c = _addcarry_u64( c, tgt[0], src2[0],  tgt+0 );
  c = _addcarry_u64( c, tgt[1], src2[1],  tgt+1 );
  c = _addcarry_u64( c, tgt[2], src2[2],  tgt+2 );
  c = _addcarry_u64( c, tgt[3], src2[3],  tgt+3 );
  c = _addcarry_u64( c, tgt[4], src2[4],  tgt+4 );
  return c;
}

// subtracts two (unsigned) big integers made up from 5 limbs (64-bit words)
uint8_t bigint320_sub( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  uint8_t b = 0;
  b = _subborrow_u64( b, src1[0], src2[0],  tgt+0 );
  b = _subborrow_u64( b, src1[1], src2[1],  tgt+1 );
  b = _subborrow_u64( b, src1[2], src2[2],  tgt+2 );
  b = _subborrow_u64( b, src1[3], src2[3],  tgt+3 );
  b = _subborrow_u64( b, src1[4], src2[4],  tgt+4 );
  return b;
}

// subtracts two big integers made up from 5 limbs (64-bit words)
uint8_t bigint320_sub_inplace( uint64_t *tgt, const uint64_t *src2 ) {
  uint8_t b = 0;
  b = _subborrow_u64( b, tgt[0], src2[0],  tgt+0 );
  b = _subborrow_u64( b, tgt[1], src2[1],  tgt+1 );
  b = _subborrow_u64( b, tgt[2], src2[2],  tgt+2 );
  b = _subborrow_u64( b, tgt[3], src2[3],  tgt+3 );
  b = _subborrow_u64( b, tgt[4], src2[4],  tgt+4 );
  return b;
}

// tgt := src - tgt
uint8_t bigint320_sub_inplace_reverse( uint64_t *tgt, const uint64_t *src1 ) {
  uint8_t b = 0;
  b = _subborrow_u64( b, src1[0], tgt[0],  tgt+0 );
  b = _subborrow_u64( b, src1[1], tgt[1],  tgt+1 );
  b = _subborrow_u64( b, src1[2], tgt[2],  tgt+2 );
  b = _subborrow_u64( b, src1[3], tgt[3],  tgt+3 );
  b = _subborrow_u64( b, src1[4], tgt[4],  tgt+4 );
  return b;
}

// multiplies an (unsigned) big integers of 5 limbs with a 64-bit word
// note: `tgt` must have space for 6 limbs!
void bigint320_scale( uint64_t z, const uint64_t *src, uint64_t *tgt) {
  uint8_t c;
  __uint128_t x;
  uint64_t hi,lo;
  tgt[0] = 0;
  // limb # 0
  x = ((__uint128_t) src[0]) * z;
  lo = (uint64_t) x;
  hi = (uint64_t)(x >> 64);
  c = _addcarry_u64( 0, tgt[0], lo, tgt+0 );
  tgt[1] = hi + c;    // note: cannot overflow because `hi <= 2^64-2`
  // limb # 1
  x = ((__uint128_t) src[1]) * z;
  lo = (uint64_t) x;
  hi = (uint64_t)(x >> 64);
  c = _addcarry_u64( 0, tgt[1], lo, tgt+1 );
  tgt[2] = hi + c;    // note: cannot overflow because `hi <= 2^64-2`
  // limb # 2
  x = ((__uint128_t) src[2]) * z;
  lo = (uint64_t) x;
  hi = (uint64_t)(x >> 64);
  c = _addcarry_u64( 0, tgt[2], lo, tgt+2 );
  tgt[3] = hi + c;    // note: cannot overflow because `hi <= 2^64-2`
  // limb # 3
  x = ((__uint128_t) src[3]) * z;
  lo = (uint64_t) x;
  hi = (uint64_t)(x >> 64);
  c = _addcarry_u64( 0, tgt[3], lo, tgt+3 );
  tgt[4] = hi + c;    // note: cannot overflow because `hi <= 2^64-2`
  // limb # 4
  x = ((__uint128_t) src[4]) * z;
  lo = (uint64_t) x;
  hi = (uint64_t)(x >> 64);
  c = _addcarry_u64( 0, tgt[4], lo, tgt+4 );
  tgt[5] = hi + c;    // note: cannot overflow because `hi <= 2^64-2`
}

// squares an (unsigned) big integer of 5 limbs
// note: `tgt` must have space for 10 limbs!
void bigint320_sqr( const uint64_t *src, uint64_t *tgt ) {
  __uint128_t prod;
  uint64_t prod_hi, prod_lo;
  uint64_t carry;
  for(int m=0; m<10; m++) { tgt[m] = 0; }
  // *** m = 0 ***
  carry = 0;
  prod = ((__uint128_t) src[0]) * src[0];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+0, tgt+1, prod_lo, prod_hi );
  tgt[2] = carry;
  // *** m = 1 ***
  carry = 0;
  prod = ((__uint128_t) src[0]) * src[1];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+1, tgt+2, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+1, tgt+2, prod_lo, prod_hi );
  tgt[3] = carry;
  // *** m = 2 ***
  carry = 0;
  prod = ((__uint128_t) src[0]) * src[2];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+2, tgt+3, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+2, tgt+3, prod_lo, prod_hi );
  prod = ((__uint128_t) src[1]) * src[1];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+2, tgt+3, prod_lo, prod_hi );
  tgt[4] = carry;
  // *** m = 3 ***
  carry = 0;
  prod = ((__uint128_t) src[0]) * src[3];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+3, tgt+4, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+3, tgt+4, prod_lo, prod_hi );
  prod = ((__uint128_t) src[1]) * src[2];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+3, tgt+4, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+3, tgt+4, prod_lo, prod_hi );
  tgt[5] = carry;
  // *** m = 4 ***
  carry = 0;
  prod = ((__uint128_t) src[0]) * src[4];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+4, tgt+5, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+4, tgt+5, prod_lo, prod_hi );
  prod = ((__uint128_t) src[1]) * src[3];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+4, tgt+5, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+4, tgt+5, prod_lo, prod_hi );
  prod = ((__uint128_t) src[2]) * src[2];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+4, tgt+5, prod_lo, prod_hi );
  tgt[6] = carry;
  // *** m = 5 ***
  carry = 0;
  prod = ((__uint128_t) src[1]) * src[4];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+5, tgt+6, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+5, tgt+6, prod_lo, prod_hi );
  prod = ((__uint128_t) src[2]) * src[3];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+5, tgt+6, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+5, tgt+6, prod_lo, prod_hi );
  tgt[7] = carry;
  // *** m = 6 ***
  carry = 0;
  prod = ((__uint128_t) src[2]) * src[4];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+6, tgt+7, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+6, tgt+7, prod_lo, prod_hi );
  prod = ((__uint128_t) src[3]) * src[3];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+6, tgt+7, prod_lo, prod_hi );
  tgt[8] = carry;
  // *** m = 7 ***
  carry = 0;
  prod = ((__uint128_t) src[3]) * src[4];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+7, tgt+8, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+7, tgt+8, prod_lo, prod_hi );
  tgt[9] = carry;
  // *** m = 8 ***
  carry = 0;
  prod = ((__uint128_t) src[4]) * src[4];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+8, tgt+9, prod_lo, prod_hi );
}

// multiplies two (unsigned) big integers of 5 limbs
// note: `tgt` must have space for 10 limbs!
void bigint320_mul( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  __uint128_t prod;
  uint64_t prod_hi, prod_lo;
  uint64_t carry;
  for(int m=0; m<10; m++) { tgt[m] = 0; }
  // *** m = 0 ***
  carry  = 0;
  prod = ((__uint128_t) src1[0]) * src2[0];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+0, tgt+1, prod_lo, prod_hi );
  tgt[2] = carry;
  // *** m = 1 ***
  carry  = 0;
  prod = ((__uint128_t) src1[0]) * src2[1];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+1, tgt+2, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[1]) * src2[0];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+1, tgt+2, prod_lo, prod_hi );
  tgt[3] = carry;
  // *** m = 2 ***
  carry  = 0;
  prod = ((__uint128_t) src1[0]) * src2[2];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+2, tgt+3, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[1]) * src2[1];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+2, tgt+3, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[2]) * src2[0];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+2, tgt+3, prod_lo, prod_hi );
  tgt[4] = carry;
  // *** m = 3 ***
  carry  = 0;
  prod = ((__uint128_t) src1[0]) * src2[3];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+3, tgt+4, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[1]) * src2[2];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+3, tgt+4, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[2]) * src2[1];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+3, tgt+4, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[3]) * src2[0];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+3, tgt+4, prod_lo, prod_hi );
  tgt[5] = carry;
  // *** m = 4 ***
  carry  = 0;
  prod = ((__uint128_t) src1[0]) * src2[4];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+4, tgt+5, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[1]) * src2[3];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+4, tgt+5, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[2]) * src2[2];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+4, tgt+5, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[3]) * src2[1];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+4, tgt+5, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[4]) * src2[0];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+4, tgt+5, prod_lo, prod_hi );
  tgt[6] = carry;
  // *** m = 5 ***
  carry  = 0;
  prod = ((__uint128_t) src1[1]) * src2[4];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+5, tgt+6, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[2]) * src2[3];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+5, tgt+6, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[3]) * src2[2];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+5, tgt+6, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[4]) * src2[1];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+5, tgt+6, prod_lo, prod_hi );
  tgt[7] = carry;
  // *** m = 6 ***
  carry  = 0;
  prod = ((__uint128_t) src1[2]) * src2[4];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+6, tgt+7, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[3]) * src2[3];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+6, tgt+7, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[4]) * src2[2];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+6, tgt+7, prod_lo, prod_hi );
  tgt[8] = carry;
  // *** m = 7 ***
  carry  = 0;
  prod = ((__uint128_t) src1[3]) * src2[4];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+7, tgt+8, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[4]) * src2[3];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+7, tgt+8, prod_lo, prod_hi );
  tgt[9] = carry;
  // *** m = 8 ***
  carry  = 0;
  prod = ((__uint128_t) src1[4]) * src2[4];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+8, tgt+9, prod_lo, prod_hi );
}

// multiplies two (unsigned) big integers of 5 limbs,
// and *truncates* the result 5 limbs
// (so this gives the ring of integers modulo 2^320)
void bigint320_mul_truncated( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  __uint128_t prod;
  uint64_t prod_hi, prod_lo;
  uint64_t carry;
  for(int m=0; m<5; m++) { tgt[m] = 0; }
  // *** m = 0 ***
  carry  = 0;
  prod = ((__uint128_t) src1[0]) * src2[0];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+0, tgt+1, prod_lo, prod_hi );
  tgt[2] = carry;
  // *** m = 1 ***
  carry  = 0;
  prod = ((__uint128_t) src1[0]) * src2[1];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+1, tgt+2, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[1]) * src2[0];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+1, tgt+2, prod_lo, prod_hi );
  tgt[3] = carry;
  // *** m = 2 ***
  carry  = 0;
  prod = ((__uint128_t) src1[0]) * src2[2];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+2, tgt+3, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[1]) * src2[1];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+2, tgt+3, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[2]) * src2[0];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+2, tgt+3, prod_lo, prod_hi );
  tgt[4] = carry;
  // *** m = 3 ***
  carry  = 0;
  prod = ((__uint128_t) src1[0]) * src2[3];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+3, tgt+4, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[1]) * src2[2];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+3, tgt+4, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[2]) * src2[1];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+3, tgt+4, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[3]) * src2[0];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+3, tgt+4, prod_lo, prod_hi );
  // *** m = 4 ***
  carry  = 0;
  prod = ((__uint128_t) src1[0]) * src2[4];
  prod_lo = (uint64_t)(prod      );
  tgt[4] += prod_lo;
  prod = ((__uint128_t) src1[1]) * src2[3];
  prod_lo = (uint64_t)(prod      );
  tgt[4] += prod_lo;
  prod = ((__uint128_t) src1[2]) * src2[2];
  prod_lo = (uint64_t)(prod      );
  tgt[4] += prod_lo;
  prod = ((__uint128_t) src1[3]) * src2[1];
  prod_lo = (uint64_t)(prod      );
  tgt[4] += prod_lo;
  prod = ((__uint128_t) src1[4]) * src2[0];
  prod_lo = (uint64_t)(prod      );
  tgt[4] += prod_lo;
}

// shift left by 1 bit
uint8_t bigint320_shift_left_by_1( const uint64_t *src, uint64_t *tgt) {
  uint64_t tmp[5];
  tmp[0] = (src[0] << 1) ;
  tmp[1] = (src[1] << 1) | (src[0] >> 63) ;
  tmp[2] = (src[2] << 1) | (src[1] >> 63) ;
  tmp[3] = (src[3] << 1) | (src[2] >> 63) ;
  tmp[4] = (src[4] << 1) | (src[3] >> 63) ;
  uint8_t c = src[4] >> 63;
  memcpy( tgt, tmp, 40 );
  return c;
}

// shift left by k bits, for 0 <= k < 64
uint64_t bigint320_shift_left_by_small( const uint64_t *src, uint64_t *tgt, int by ) {
  assert( (by >= 0) && (by < 64) );
  uint64_t tmp[5];
  tmp[0] = (src[0] << by) ;
  tmp[1] = (src[1] << by) | (src[0] >> (64-by)) ;
  tmp[2] = (src[2] << by) | (src[1] >> (64-by)) ;
  tmp[3] = (src[3] << by) | (src[2] >> (64-by)) ;
  tmp[4] = (src[4] << by) | (src[3] >> (64-by)) ;
  uint64_t c = src[4] >> (64-by);
  memcpy( tgt, tmp, 40 );
  return c;
}

// shift left by k bits, for k >= 0
void bigint320_shift_left_by_k( const uint64_t *src, uint64_t *tgt, int by ) {
  assert(by >= 0);
  int small = by % 64;
  int move  = by / 64;
  if (by >= 320) {
    bigint320_set_zero(tgt);
  } 
  else {
    for(int i=0    ; i<move; i++) { tgt[i] = 0; }
    for(int i=move ; i<5     ; i++) { tgt[i] = src[i-move]; }
    bigint320_shift_left_by_small(tgt, tgt, small);
  }
}

// shift right by 1 bit
uint8_t bigint320_shift_right_by_1( const uint64_t *src, uint64_t *tgt) {
  uint64_t tmp[5];
  tmp[4] = (src[4] >> 1) ;
  tmp[3] = (src[3] >> 1) | (src[4] << 63) ;
  tmp[2] = (src[2] >> 1) | (src[3] << 63) ;
  tmp[1] = (src[1] >> 1) | (src[2] << 63) ;
  tmp[0] = (src[0] >> 1) | (src[1] << 63) ;
  uint8_t c = src[0] & 1;
  memcpy( tgt, tmp, 40 );
  return c;
}

// shift right by k bits, for 0 <= k < 64
uint64_t bigint320_shift_right_by_small( const uint64_t *src, uint64_t *tgt, int by ) {
  assert( (by >= 0) && (by < 64) );
  uint64_t tmp[5];
  tmp[4] = (src[4] >> by) ;
  tmp[3] = (src[3] >> by) | (src[4] << (64-by)) ;
  tmp[2] = (src[2] >> by) | (src[3] << (64-by)) ;
  tmp[1] = (src[1] >> by) | (src[2] << (64-by)) ;
  tmp[0] = (src[0] >> by) | (src[1] << (64-by)) ;
  uint64_t c = src[0] & ((1<<by) - 1);
  memcpy( tgt, tmp, 40 );
  return c;
}

// shift right by k bits, for k >= 0
void bigint320_shift_right_by_k( const uint64_t *src, uint64_t *tgt, int by ) {
  assert(by >= 0);
  int small = by % 64;
  int move  = by / 64;
  if (by >= 320) {
    bigint320_set_zero(tgt);
  } 
  else {
    for(int i=0     ; i<5-move; i++) { tgt[i] = src[i+move]; }
    for(int i=5-move; i<5     ; i++) { tgt[i] = 0;           }
    bigint320_shift_right_by_small(tgt, tgt, small);
  }
}
