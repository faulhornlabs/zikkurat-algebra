
// unsigned big integers composed of 2 64-bit words
//
// NOTE: generated code, do not edit!

#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <x86intrin.h>
#include <assert.h>
#include "bigint128.h"

#define NLIMBS 2

#define MIN(a,b) ( ((a)<=(b)) ? (a) : (b) )
#define MAX(a,b) ( ((a)>=(b)) ? (a) : (b) )

inline uint8_t addcarry_u128_inplace(  uint64_t *tgt_lo, uint64_t *tgt_hi, uint64_t arg_lo, uint64_t arg_hi) {
  uint8_t c;
  c = _addcarry_u64( 0, *tgt_lo, arg_lo, tgt_lo );
  c = _addcarry_u64( c, *tgt_hi, arg_hi, tgt_hi );
  return c;
}

//------------------------------------------------------------------------------

uint8_t bigint128_is_zero(const uint64_t *src) {
  return ( (src[0] == 0) && (src[1] == 0)) ;
}

uint8_t bigint128_is_one(const uint64_t *src) {
  return ( (src[0] == 1) && (src[1] == 0)) ;
}

uint8_t bigint128_is_equal(const uint64_t *src1, const uint64_t *src2 ) {
  return ( (src1[0] == src2[0]) && (src1[1] == src2[1])) ;
}

void bigint128_set_zero(uint64_t *tgt) {
  memset( tgt, 0, 16 );
}

void bigint128_set_one(uint64_t *tgt) {
  memset( tgt, 0, 16 );
  tgt[0] = 1;
}

void bigint128_set_small(uint64_t *tgt, uint64_t s) {
  memset( tgt, 0, 16 );
  tgt[0] = s;
}

void bigint128_copy(const uint64_t *src, uint64_t *tgt) {
  memcpy( tgt, src, 16 );
}

void bigint128_print(const uint64_t *what, int underscore_separators) {
  if (underscore_separators) {
    for(int i=0; i<2; i++) {
      printf("%016llx_", what[1-i]);
    }
  }
  else {
    for(int i=0; i<2; i++) {
      printf("%016llx", what[1-i]);
    }
  }
}

void bigint128_debug_print(const char *txt, const uint64_t *what) {
  printf("%s = 0x",txt);
  bigint128_print( what, 1 );
  printf("\n");
}

// increments bigint by 1, inplace
uint8_t bigint128_inc_inplace( uint64_t *tgt ) {
  uint8_t c = 0;
  c = _addcarry_u64( c, tgt[0], 1, tgt+0 );
  c = _addcarry_u64( c, tgt[1], 0, tgt+1 );
  return c;
}

// decrements bigint by 1, inplace
uint8_t bigint128_dec_inplace( uint64_t *tgt ) {
  uint8_t b = 0;
  b = _subborrow_u64( b, tgt[0], 1, tgt+0 );
  b = _subborrow_u64( b, tgt[1], 0, tgt+1 );
  return b;
}

// negates a bigint
void bigint128_neg( const uint64_t *src, uint64_t *tgt ) {
  uint8_t b = 0;
  b = _subborrow_u64( b, 0, src[0], tgt+0 );
  b = _subborrow_u64( b, 0, src[1], tgt+1 );
}

// negates a bigint inplace
void bigint128_neg_inplace( uint64_t *tgt ) {
  uint8_t b = 0;
  b = _subborrow_u64( b, 0, tgt[0], tgt+0 );
  b = _subborrow_u64( b, 0, tgt[1], tgt+1 );
}

// adds two (unsigned) big integers made up from 2 limbs (64-bit words)
uint8_t bigint128_add( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  uint8_t c = 0;
  c = _addcarry_u64( c, src1[0], src2[0],  tgt+0 );
  c = _addcarry_u64( c, src1[1], src2[1],  tgt+1 );
  return c;
}

// adds two big integers made up from 2 limbs (64-bit words)
uint8_t bigint128_add_inplace( uint64_t *tgt, const uint64_t *src2 ) {
  uint8_t c = 0;
  c = _addcarry_u64( c, tgt[0], src2[0],  tgt+0 );
  c = _addcarry_u64( c, tgt[1], src2[1],  tgt+1 );
  return c;
}

// subtracts two (unsigned) big integers made up from 2 limbs (64-bit words)
uint8_t bigint128_sub( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  uint8_t b = 0;
  b = _subborrow_u64( b, src1[0], src2[0],  tgt+0 );
  b = _subborrow_u64( b, src1[1], src2[1],  tgt+1 );
  return b;
}

// subtracts two big integers made up from 2 limbs (64-bit words)
uint8_t bigint128_sub_inplace( uint64_t *tgt, const uint64_t *src2 ) {
  uint8_t b = 0;
  b = _subborrow_u64( b, tgt[0], src2[0],  tgt+0 );
  b = _subborrow_u64( b, tgt[1], src2[1],  tgt+1 );
  return b;
}

// tgt := src - tgt
uint8_t bigint128_sub_inplace_reverse( uint64_t *tgt, const uint64_t *src1 ) {
  uint8_t b = 0;
  b = _subborrow_u64( b, src1[0], tgt[0],  tgt+0 );
  b = _subborrow_u64( b, src1[1], tgt[1],  tgt+1 );
  return b;
}

// multiplies an (unsigned) big integers of 2 limbs with a 64-bit word
// note: `tgt` must have space for 3 limbs!
void bigint128_scale( uint64_t z, const uint64_t *src, uint64_t *tgt) {
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
}

// squares an (unsigned) big integer of 2 limbs
// note: `tgt` must have space for 4 limbs!
void bigint128_sqr( const uint64_t *src, uint64_t *tgt ) {
  __uint128_t prod;
  uint64_t prod_hi, prod_lo;
  uint64_t carry;
  for(int m=0; m<4; m++) { tgt[m] = 0; }
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
  prod = ((__uint128_t) src[1]) * src[1];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+2, tgt+3, prod_lo, prod_hi );
}

// multiplies two (unsigned) big integers of 2 limbs
// note: `tgt` must have space for 4 limbs!
void bigint128_mul( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  __uint128_t prod;
  uint64_t prod_hi, prod_lo;
  uint64_t carry;
  for(int m=0; m<4; m++) { tgt[m] = 0; }
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
  prod = ((__uint128_t) src1[1]) * src2[1];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+2, tgt+3, prod_lo, prod_hi );
}

// multiplies two (unsigned) big integers of 2 limbs,
// and *truncates* the result 2 limbs
// (so this gives the ring of integers modulo 2^128)
void bigint128_mul_truncated( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  __uint128_t prod;
  uint64_t prod_hi, prod_lo;
  uint64_t carry;
  for(int m=0; m<2; m++) { tgt[m] = 0; }
  // *** m = 0 ***
  carry  = 0;
  prod = ((__uint128_t) src1[0]) * src2[0];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+0, tgt+1, prod_lo, prod_hi );
  // *** m = 1 ***
  carry  = 0;
  prod = ((__uint128_t) src1[0]) * src2[1];
  prod_lo = (uint64_t)(prod      );
  tgt[1] += prod_lo;
  prod = ((__uint128_t) src1[1]) * src2[0];
  prod_lo = (uint64_t)(prod      );
  tgt[1] += prod_lo;
}

// shift left by 1 bit
uint8_t bigint128_shift_left_by_1( const uint64_t *src, uint64_t *tgt) {
  uint64_t tmp[2];
  tmp[0] = (src[0] << 1) ;
  tmp[1] = (src[1] << 1) | (src[0] >> 63) ;
  uint8_t c = src[1] >> 63;
  memcpy( tgt, tmp, 16 );
  return c;
}

// shift left by k bits, for 0 <= k < 64
uint64_t bigint128_shift_left_by_small( const uint64_t *src, uint64_t *tgt, int by ) {
  assert( (by >= 0) && (by < 64) );
  uint64_t tmp[2];
  tmp[0] = (src[0] << by) ;
  tmp[1] = (src[1] << by) | (src[0] >> (64-by)) ;
  uint64_t c = src[1] >> (64-by);
  memcpy( tgt, tmp, 16 );
  return c;
}

// shift left by k bits, for k >= 0
void bigint128_shift_left_by_k( const uint64_t *src, uint64_t *tgt, int by ) {
  assert(by >= 0);
  int small = by % 64;
  int move  = by / 64;
  if (by >= 128) {
    bigint128_set_zero(tgt);
  } 
  else {
    for(int i=0    ; i<move; i++) { tgt[i] = 0; }
    for(int i=move ; i<2     ; i++) { tgt[i] = src[i-move]; }
    bigint128_shift_left_by_small(tgt, tgt, small);
  }
}

// shift right by 1 bit
uint8_t bigint128_shift_right_by_1( const uint64_t *src, uint64_t *tgt) {
  uint64_t tmp[2];
  tmp[1] = (src[1] >> 1) ;
  tmp[0] = (src[0] >> 1) | (src[1] << 63) ;
  uint8_t c = src[0] & 1;
  memcpy( tgt, tmp, 16 );
  return c;
}

// shift right by k bits, for 0 <= k < 64
uint64_t bigint128_shift_right_by_small( const uint64_t *src, uint64_t *tgt, int by ) {
  assert( (by >= 0) && (by < 64) );
  uint64_t tmp[2];
  tmp[1] = (src[1] >> by) ;
  tmp[0] = (src[0] >> by) | (src[1] << (64-by)) ;
  uint64_t c = src[0] & ((1<<by) - 1);
  memcpy( tgt, tmp, 16 );
  return c;
}

// shift right by k bits, for k >= 0
void bigint128_shift_right_by_k( const uint64_t *src, uint64_t *tgt, int by ) {
  assert(by >= 0);
  int small = by % 64;
  int move  = by / 64;
  if (by >= 128) {
    bigint128_set_zero(tgt);
  } 
  else {
    for(int i=0     ; i<2-move; i++) { tgt[i] = src[i+move]; }
    for(int i=2-move; i<2     ; i++) { tgt[i] = 0;           }
    bigint128_shift_right_by_small(tgt, tgt, small);
  }
}
