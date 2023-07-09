
// unsigned big integers composed of 3 64-bit words
//
// NOTE: generated code, do not edit!

#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <x86intrin.h>
#include <assert.h>
#include "bigint192.h"

#define NLIMBS 3

#define MIN(a,b) ( ((a)<=(b)) ? (a) : (b) )
#define MAX(a,b) ( ((a)>=(b)) ? (a) : (b) )

inline uint8_t addcarry_u128_inplace(  uint64_t *tgt_lo, uint64_t *tgt_hi, uint64_t arg_lo, uint64_t arg_hi) {
  uint8_t c;
  c = _addcarry_u64( 0, *tgt_lo, arg_lo, tgt_lo );
  c = _addcarry_u64( c, *tgt_hi, arg_hi, tgt_hi );
  return c;
}

//------------------------------------------------------------------------------

uint8_t bigint192_is_zero(const uint64_t *src) {
  return ( (src[0] == 0) && (src[1] == 0) && (src[2] == 0)) ;
}

uint8_t bigint192_is_one(const uint64_t *src) {
  return ( (src[0] == 1) && (src[1] == 0) && (src[2] == 0)) ;
}

uint8_t bigint192_is_equal(const uint64_t *src1, const uint64_t *src2 ) {
  return ( (src1[0] == src2[0]) && (src1[1] == src2[1]) && (src1[2] == src2[2])) ;
}

void bigint192_set_zero(uint64_t *tgt) {
  memset( tgt, 0, 24 );
}

void bigint192_set_one(uint64_t *tgt) {
  memset( tgt, 0, 24 );
  tgt[0] = 1;
}

void bigint192_set_small(uint64_t *tgt, uint64_t s) {
  memset( tgt, 0, 24 );
  tgt[0] = s;
}

void bigint192_copy(const uint64_t *src, uint64_t *tgt) {
  if (src != tgt) { memcpy( tgt, src, 24 ); }
}

void bigint192_print(const uint64_t *what, int underscore_separators) {
  if (underscore_separators) {
    for(int i=0; i<3; i++) {
      printf("%016llx_", what[2-i]);
    }
  }
  else {
    for(int i=0; i<3; i++) {
      printf("%016llx", what[2-i]);
    }
  }
}

void bigint192_debug_print(const char *txt, const uint64_t *what) {
  printf("%s = 0x",txt);
  bigint192_print( what, 1 );
  printf("\n");
}

// increments bigint by 1, inplace
uint8_t bigint192_inc_inplace( uint64_t *tgt ) {
  uint8_t c = 0;
  c = _addcarry_u64( c, tgt[0], 1, tgt+0 );
  c = _addcarry_u64( c, tgt[1], 0, tgt+1 );
  c = _addcarry_u64( c, tgt[2], 0, tgt+2 );
  return c;
}

// decrements bigint by 1, inplace
uint8_t bigint192_dec_inplace( uint64_t *tgt ) {
  uint8_t b = 0;
  b = _subborrow_u64( b, tgt[0], 1, tgt+0 );
  b = _subborrow_u64( b, tgt[1], 0, tgt+1 );
  b = _subborrow_u64( b, tgt[2], 0, tgt+2 );
  return b;
}

// negates a bigint
void bigint192_neg( const uint64_t *src, uint64_t *tgt ) {
  uint8_t b = 0;
  b = _subborrow_u64( b, 0, src[0], tgt+0 );
  b = _subborrow_u64( b, 0, src[1], tgt+1 );
  b = _subborrow_u64( b, 0, src[2], tgt+2 );
}

// negates a bigint inplace
void bigint192_neg_inplace( uint64_t *tgt ) {
  uint8_t b = 0;
  b = _subborrow_u64( b, 0, tgt[0], tgt+0 );
  b = _subborrow_u64( b, 0, tgt[1], tgt+1 );
  b = _subborrow_u64( b, 0, tgt[2], tgt+2 );
}

// adds two (unsigned) big integers made up from 3 limbs (64-bit words)
uint8_t bigint192_add( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  uint8_t c = 0;
  c = _addcarry_u64( c, src1[0], src2[0],  tgt+0 );
  c = _addcarry_u64( c, src1[1], src2[1],  tgt+1 );
  c = _addcarry_u64( c, src1[2], src2[2],  tgt+2 );
  return c;
}

// adds two big integers made up from 3 limbs (64-bit words)
uint8_t bigint192_add_inplace( uint64_t *tgt, const uint64_t *src2 ) {
  uint8_t c = 0;
  c = _addcarry_u64( c, tgt[0], src2[0],  tgt+0 );
  c = _addcarry_u64( c, tgt[1], src2[1],  tgt+1 );
  c = _addcarry_u64( c, tgt[2], src2[2],  tgt+2 );
  return c;
}

// subtracts two (unsigned) big integers made up from 3 limbs (64-bit words)
uint8_t bigint192_sub( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  uint8_t b = 0;
  b = _subborrow_u64( b, src1[0], src2[0],  tgt+0 );
  b = _subborrow_u64( b, src1[1], src2[1],  tgt+1 );
  b = _subborrow_u64( b, src1[2], src2[2],  tgt+2 );
  return b;
}

// subtracts two big integers made up from 3 limbs (64-bit words)
uint8_t bigint192_sub_inplace( uint64_t *tgt, const uint64_t *src2 ) {
  uint8_t b = 0;
  b = _subborrow_u64( b, tgt[0], src2[0],  tgt+0 );
  b = _subborrow_u64( b, tgt[1], src2[1],  tgt+1 );
  b = _subborrow_u64( b, tgt[2], src2[2],  tgt+2 );
  return b;
}

// tgt := src - tgt
uint8_t bigint192_sub_inplace_reverse( uint64_t *tgt, const uint64_t *src1 ) {
  uint8_t b = 0;
  b = _subborrow_u64( b, src1[0], tgt[0],  tgt+0 );
  b = _subborrow_u64( b, src1[1], tgt[1],  tgt+1 );
  b = _subborrow_u64( b, src1[2], tgt[2],  tgt+2 );
  return b;
}

// multiplies an (unsigned) big integers of 3 limbs with a 64-bit word
// note: `tgt` must have space for 4 limbs!
void bigint192_scale( uint64_t z, const uint64_t *src, uint64_t *tgt) {
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
}

// squares an (unsigned) big integer of 3 limbs
// note: `tgt` must have space for 6 limbs!
void bigint192_sqr( const uint64_t *src, uint64_t *tgt ) {
  __uint128_t prod;
  uint64_t prod_hi, prod_lo;
  uint64_t carry;
  for(int m=0; m<6; m++) { tgt[m] = 0; }
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
  prod = ((__uint128_t) src[1]) * src[2];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+3, tgt+4, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+3, tgt+4, prod_lo, prod_hi );
  tgt[5] = carry;
  // *** m = 4 ***
  carry = 0;
  prod = ((__uint128_t) src[2]) * src[2];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+4, tgt+5, prod_lo, prod_hi );
}

// multiplies two (unsigned) big integers of 3 limbs
// note: `tgt` must have space for 6 limbs!
void bigint192_mul( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  __uint128_t prod;
  uint64_t prod_hi, prod_lo;
  uint64_t carry;
  for(int m=0; m<6; m++) { tgt[m] = 0; }
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
  prod = ((__uint128_t) src1[1]) * src2[2];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+3, tgt+4, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[2]) * src2[1];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+3, tgt+4, prod_lo, prod_hi );
  tgt[5] = carry;
  // *** m = 4 ***
  carry  = 0;
  prod = ((__uint128_t) src1[2]) * src2[2];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+4, tgt+5, prod_lo, prod_hi );
}

// multiplies two (unsigned) big integers of 3 limbs,
// and *truncates* the result 3 limbs
// (so this gives the ring of integers modulo 2^192)
void bigint192_mul_truncated( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  __uint128_t prod;
  uint64_t prod_hi, prod_lo;
  uint64_t carry;
  for(int m=0; m<3; m++) { tgt[m] = 0; }
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
  // *** m = 2 ***
  carry  = 0;
  prod = ((__uint128_t) src1[0]) * src2[2];
  prod_lo = (uint64_t)(prod      );
  tgt[2] += prod_lo;
  prod = ((__uint128_t) src1[1]) * src2[1];
  prod_lo = (uint64_t)(prod      );
  tgt[2] += prod_lo;
  prod = ((__uint128_t) src1[2]) * src2[0];
  prod_lo = (uint64_t)(prod      );
  tgt[2] += prod_lo;
}

// shift left by 1 bit
uint8_t bigint192_shift_left_by_1( const uint64_t *src, uint64_t *tgt) {
  uint64_t tmp[3];
  tmp[0] = (src[0] << 1) ;
  tmp[1] = (src[1] << 1) | (src[0] >> 63) ;
  tmp[2] = (src[2] << 1) | (src[1] >> 63) ;
  uint8_t c = src[2] >> 63;
  memcpy( tgt, tmp, 24 );
  return c;
}

// shift left by k bits, for 0 <= k < 64
uint64_t bigint192_shift_left_by_small( const uint64_t *src, uint64_t *tgt, int by ) {
  assert( (by >= 0) && (by < 64) );
  uint64_t tmp[3];
  tmp[0] = (src[0] << by) ;
  tmp[1] = (src[1] << by) | (src[0] >> (64-by)) ;
  tmp[2] = (src[2] << by) | (src[1] >> (64-by)) ;
  uint64_t c = src[2] >> (64-by);
  memcpy( tgt, tmp, 24 );
  return c;
}

// shift left by k bits, for k >= 0
void bigint192_shift_left_by_k( const uint64_t *src, uint64_t *tgt, int by ) {
  assert(by >= 0);
  int small = by % 64;
  int move  = by / 64;
  if (by >= 192) {
    bigint192_set_zero(tgt);
  } 
  else {
    for(int i=0    ; i<move; i++) { tgt[i] = 0; }
    for(int i=move ; i<3     ; i++) { tgt[i] = src[i-move]; }
    bigint192_shift_left_by_small(tgt, tgt, small);
  }
}

// shift right by 1 bit
uint8_t bigint192_shift_right_by_1( const uint64_t *src, uint64_t *tgt) {
  uint64_t tmp[3];
  tmp[2] = (src[2] >> 1) ;
  tmp[1] = (src[1] >> 1) | (src[2] << 63) ;
  tmp[0] = (src[0] >> 1) | (src[1] << 63) ;
  uint8_t c = src[0] & 1;
  memcpy( tgt, tmp, 24 );
  return c;
}

// shift right by k bits, for 0 <= k < 64
uint64_t bigint192_shift_right_by_small( const uint64_t *src, uint64_t *tgt, int by ) {
  assert( (by >= 0) && (by < 64) );
  uint64_t tmp[3];
  tmp[2] = (src[2] >> by) ;
  tmp[1] = (src[1] >> by) | (src[2] << (64-by)) ;
  tmp[0] = (src[0] >> by) | (src[1] << (64-by)) ;
  uint64_t c = src[0] & ((1<<by) - 1);
  memcpy( tgt, tmp, 24 );
  return c;
}

// shift right by k bits, for k >= 0
void bigint192_shift_right_by_k( const uint64_t *src, uint64_t *tgt, int by ) {
  assert(by >= 0);
  int small = by % 64;
  int move  = by / 64;
  if (by >= 192) {
    bigint192_set_zero(tgt);
  } 
  else {
    for(int i=0     ; i<3-move; i++) { tgt[i] = src[i+move]; }
    for(int i=3-move; i<3     ; i++) { tgt[i] = 0;           }
    bigint192_shift_right_by_small(tgt, tgt, small);
  }
}
