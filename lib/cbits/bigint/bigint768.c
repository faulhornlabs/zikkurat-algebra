
// unsigned big integers composed of 12 64-bit words
//
// NOTE: generated code, do not edit!

#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>
#include "bigint768.h"
#include "platform.h"

#define NLIMBS 12

#define MIN(a,b) ( ((a)<=(b)) ? (a) : (b) )
#define MAX(a,b) ( ((a)>=(b)) ? (a) : (b) )

//------------------------------------------------------------------------------

uint8_t bigint768_is_zero(const uint64_t *src) {
  return ( (src[0] == 0) && (src[1] == 0) && (src[2] == 0) && (src[3] == 0) && (src[4] == 0) && (src[5] == 0) && (src[6] == 0) && (src[7] == 0) && (src[8] == 0) && (src[9] == 0) && (src[10] == 0) && (src[11] == 0)) ;
}

uint8_t bigint768_is_one(const uint64_t *src) {
  return ( (src[0] == 1) && (src[1] == 0) && (src[2] == 0) && (src[3] == 0) && (src[4] == 0) && (src[5] == 0) && (src[6] == 0) && (src[7] == 0) && (src[8] == 0) && (src[9] == 0) && (src[10] == 0) && (src[11] == 0)) ;
}

uint8_t bigint768_is_equal(const uint64_t *src1, const uint64_t *src2 ) {
  return ( (src1[0] == src2[0]) && (src1[1] == src2[1]) && (src1[2] == src2[2]) && (src1[3] == src2[3]) && (src1[4] == src2[4]) && (src1[5] == src2[5]) && (src1[6] == src2[6]) && (src1[7] == src2[7]) && (src1[8] == src2[8]) && (src1[9] == src2[9]) && (src1[10] == src2[10]) && (src1[11] == src2[11])) ;
}

void bigint768_set_zero(uint64_t *tgt) {
  memset( tgt, 0, 96 );
}

void bigint768_set_one(uint64_t *tgt) {
  memset( tgt, 0, 96 );
  tgt[0] = 1;
}

void bigint768_set_small(uint64_t *tgt, uint64_t s) {
  memset( tgt, 0, 96 );
  tgt[0] = s;
}

void bigint768_copy(const uint64_t *src, uint64_t *tgt) {
  if (src != tgt) { memcpy( tgt, src, 96 ); }
}

void bigint768_print(const uint64_t *what, int underscore_separators) {
  if (underscore_separators) {
    for(int i=0; i<12; i++) {
      printf("%016llx_", what[11-i]);
    }
  }
  else {
    for(int i=0; i<12; i++) {
      printf("%016llx", what[11-i]);
    }
  }
}

void bigint768_debug_print(const char *txt, const uint64_t *what) {
  printf("%s = 0x",txt);
  bigint768_print( what, 1 );
  printf("\n");
}

// increments bigint by 1, inplace
uint8_t bigint768_inc_inplace( uint64_t *tgt ) {
  uint8_t c = 0;
  c = addcarry_u64( c, tgt[0], 1, tgt+0 );
  c = addcarry_u64( c, tgt[1], 0, tgt+1 );
  c = addcarry_u64( c, tgt[2], 0, tgt+2 );
  c = addcarry_u64( c, tgt[3], 0, tgt+3 );
  c = addcarry_u64( c, tgt[4], 0, tgt+4 );
  c = addcarry_u64( c, tgt[5], 0, tgt+5 );
  c = addcarry_u64( c, tgt[6], 0, tgt+6 );
  c = addcarry_u64( c, tgt[7], 0, tgt+7 );
  c = addcarry_u64( c, tgt[8], 0, tgt+8 );
  c = addcarry_u64( c, tgt[9], 0, tgt+9 );
  c = addcarry_u64( c, tgt[10], 0, tgt+10 );
  c = addcarry_u64( c, tgt[11], 0, tgt+11 );
  return c;
}

// decrements bigint by 1, inplace
uint8_t bigint768_dec_inplace( uint64_t *tgt ) {
  uint8_t b = 0;
  b = subborrow_u64( b, tgt[0], 1, tgt+0 );
  b = subborrow_u64( b, tgt[1], 0, tgt+1 );
  b = subborrow_u64( b, tgt[2], 0, tgt+2 );
  b = subborrow_u64( b, tgt[3], 0, tgt+3 );
  b = subborrow_u64( b, tgt[4], 0, tgt+4 );
  b = subborrow_u64( b, tgt[5], 0, tgt+5 );
  b = subborrow_u64( b, tgt[6], 0, tgt+6 );
  b = subborrow_u64( b, tgt[7], 0, tgt+7 );
  b = subborrow_u64( b, tgt[8], 0, tgt+8 );
  b = subborrow_u64( b, tgt[9], 0, tgt+9 );
  b = subborrow_u64( b, tgt[10], 0, tgt+10 );
  b = subborrow_u64( b, tgt[11], 0, tgt+11 );
  return b;
}

// negates a bigint
void bigint768_neg( const uint64_t *src, uint64_t *tgt ) {
  uint8_t b = 0;
  b = subborrow_u64( b, 0, src[0], tgt+0 );
  b = subborrow_u64( b, 0, src[1], tgt+1 );
  b = subborrow_u64( b, 0, src[2], tgt+2 );
  b = subborrow_u64( b, 0, src[3], tgt+3 );
  b = subborrow_u64( b, 0, src[4], tgt+4 );
  b = subborrow_u64( b, 0, src[5], tgt+5 );
  b = subborrow_u64( b, 0, src[6], tgt+6 );
  b = subborrow_u64( b, 0, src[7], tgt+7 );
  b = subborrow_u64( b, 0, src[8], tgt+8 );
  b = subborrow_u64( b, 0, src[9], tgt+9 );
  b = subborrow_u64( b, 0, src[10], tgt+10 );
  b = subborrow_u64( b, 0, src[11], tgt+11 );
}

// negates a bigint inplace
void bigint768_neg_inplace( uint64_t *tgt ) {
  uint8_t b = 0;
  b = subborrow_u64( b, 0, tgt[0], tgt+0 );
  b = subborrow_u64( b, 0, tgt[1], tgt+1 );
  b = subborrow_u64( b, 0, tgt[2], tgt+2 );
  b = subborrow_u64( b, 0, tgt[3], tgt+3 );
  b = subborrow_u64( b, 0, tgt[4], tgt+4 );
  b = subborrow_u64( b, 0, tgt[5], tgt+5 );
  b = subborrow_u64( b, 0, tgt[6], tgt+6 );
  b = subborrow_u64( b, 0, tgt[7], tgt+7 );
  b = subborrow_u64( b, 0, tgt[8], tgt+8 );
  b = subborrow_u64( b, 0, tgt[9], tgt+9 );
  b = subborrow_u64( b, 0, tgt[10], tgt+10 );
  b = subborrow_u64( b, 0, tgt[11], tgt+11 );
}

// adds two (unsigned) big integers made up from 12 limbs (64-bit words)
uint8_t bigint768_add( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  uint8_t c = 0;
  c = addcarry_u64( c, src1[0], src2[0],  tgt+0 );
  c = addcarry_u64( c, src1[1], src2[1],  tgt+1 );
  c = addcarry_u64( c, src1[2], src2[2],  tgt+2 );
  c = addcarry_u64( c, src1[3], src2[3],  tgt+3 );
  c = addcarry_u64( c, src1[4], src2[4],  tgt+4 );
  c = addcarry_u64( c, src1[5], src2[5],  tgt+5 );
  c = addcarry_u64( c, src1[6], src2[6],  tgt+6 );
  c = addcarry_u64( c, src1[7], src2[7],  tgt+7 );
  c = addcarry_u64( c, src1[8], src2[8],  tgt+8 );
  c = addcarry_u64( c, src1[9], src2[9],  tgt+9 );
  c = addcarry_u64( c, src1[10], src2[10],  tgt+10 );
  c = addcarry_u64( c, src1[11], src2[11],  tgt+11 );
  return c;
}

// adds two big integers made up from 12 limbs (64-bit words)
uint8_t bigint768_add_inplace( uint64_t *tgt, const uint64_t *src2 ) {
  uint8_t c = 0;
  c = addcarry_u64( c, tgt[0], src2[0],  tgt+0 );
  c = addcarry_u64( c, tgt[1], src2[1],  tgt+1 );
  c = addcarry_u64( c, tgt[2], src2[2],  tgt+2 );
  c = addcarry_u64( c, tgt[3], src2[3],  tgt+3 );
  c = addcarry_u64( c, tgt[4], src2[4],  tgt+4 );
  c = addcarry_u64( c, tgt[5], src2[5],  tgt+5 );
  c = addcarry_u64( c, tgt[6], src2[6],  tgt+6 );
  c = addcarry_u64( c, tgt[7], src2[7],  tgt+7 );
  c = addcarry_u64( c, tgt[8], src2[8],  tgt+8 );
  c = addcarry_u64( c, tgt[9], src2[9],  tgt+9 );
  c = addcarry_u64( c, tgt[10], src2[10],  tgt+10 );
  c = addcarry_u64( c, tgt[11], src2[11],  tgt+11 );
  return c;
}

// subtracts two (unsigned) big integers made up from 12 limbs (64-bit words)
uint8_t bigint768_sub( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  uint8_t b = 0;
  b = subborrow_u64( b, src1[0], src2[0],  tgt+0 );
  b = subborrow_u64( b, src1[1], src2[1],  tgt+1 );
  b = subborrow_u64( b, src1[2], src2[2],  tgt+2 );
  b = subborrow_u64( b, src1[3], src2[3],  tgt+3 );
  b = subborrow_u64( b, src1[4], src2[4],  tgt+4 );
  b = subborrow_u64( b, src1[5], src2[5],  tgt+5 );
  b = subborrow_u64( b, src1[6], src2[6],  tgt+6 );
  b = subborrow_u64( b, src1[7], src2[7],  tgt+7 );
  b = subborrow_u64( b, src1[8], src2[8],  tgt+8 );
  b = subborrow_u64( b, src1[9], src2[9],  tgt+9 );
  b = subborrow_u64( b, src1[10], src2[10],  tgt+10 );
  b = subborrow_u64( b, src1[11], src2[11],  tgt+11 );
  return b;
}

// subtracts two big integers made up from 12 limbs (64-bit words)
uint8_t bigint768_sub_inplace( uint64_t *tgt, const uint64_t *src2 ) {
  uint8_t b = 0;
  b = subborrow_u64( b, tgt[0], src2[0],  tgt+0 );
  b = subborrow_u64( b, tgt[1], src2[1],  tgt+1 );
  b = subborrow_u64( b, tgt[2], src2[2],  tgt+2 );
  b = subborrow_u64( b, tgt[3], src2[3],  tgt+3 );
  b = subborrow_u64( b, tgt[4], src2[4],  tgt+4 );
  b = subborrow_u64( b, tgt[5], src2[5],  tgt+5 );
  b = subborrow_u64( b, tgt[6], src2[6],  tgt+6 );
  b = subborrow_u64( b, tgt[7], src2[7],  tgt+7 );
  b = subborrow_u64( b, tgt[8], src2[8],  tgt+8 );
  b = subborrow_u64( b, tgt[9], src2[9],  tgt+9 );
  b = subborrow_u64( b, tgt[10], src2[10],  tgt+10 );
  b = subborrow_u64( b, tgt[11], src2[11],  tgt+11 );
  return b;
}

// tgt := src - tgt
uint8_t bigint768_sub_inplace_reverse( uint64_t *tgt, const uint64_t *src1 ) {
  uint8_t b = 0;
  b = subborrow_u64( b, src1[0], tgt[0],  tgt+0 );
  b = subborrow_u64( b, src1[1], tgt[1],  tgt+1 );
  b = subborrow_u64( b, src1[2], tgt[2],  tgt+2 );
  b = subborrow_u64( b, src1[3], tgt[3],  tgt+3 );
  b = subborrow_u64( b, src1[4], tgt[4],  tgt+4 );
  b = subborrow_u64( b, src1[5], tgt[5],  tgt+5 );
  b = subborrow_u64( b, src1[6], tgt[6],  tgt+6 );
  b = subborrow_u64( b, src1[7], tgt[7],  tgt+7 );
  b = subborrow_u64( b, src1[8], tgt[8],  tgt+8 );
  b = subborrow_u64( b, src1[9], tgt[9],  tgt+9 );
  b = subborrow_u64( b, src1[10], tgt[10],  tgt+10 );
  b = subborrow_u64( b, src1[11], tgt[11],  tgt+11 );
  return b;
}

// multiplies an (unsigned) big integers of 12 limbs with a 64-bit word
// note: `tgt` must have space for 13 limbs!
void bigint768_scale( uint64_t z, const uint64_t *src, uint64_t *tgt) {
  uint8_t c;
  __uint128_t x;
  uint64_t hi,lo;
  tgt[0] = 0;
  // limb # 0
  x = ((__uint128_t) src[0]) * z;
  lo = (uint64_t) x;
  hi = (uint64_t)(x >> 64);
  c = addcarry_u64( 0, tgt[0], lo, tgt+0 );
  tgt[1] = hi + c;    // note: cannot overflow because `hi <= 2^64-2`
  // limb # 1
  x = ((__uint128_t) src[1]) * z;
  lo = (uint64_t) x;
  hi = (uint64_t)(x >> 64);
  c = addcarry_u64( 0, tgt[1], lo, tgt+1 );
  tgt[2] = hi + c;    // note: cannot overflow because `hi <= 2^64-2`
  // limb # 2
  x = ((__uint128_t) src[2]) * z;
  lo = (uint64_t) x;
  hi = (uint64_t)(x >> 64);
  c = addcarry_u64( 0, tgt[2], lo, tgt+2 );
  tgt[3] = hi + c;    // note: cannot overflow because `hi <= 2^64-2`
  // limb # 3
  x = ((__uint128_t) src[3]) * z;
  lo = (uint64_t) x;
  hi = (uint64_t)(x >> 64);
  c = addcarry_u64( 0, tgt[3], lo, tgt+3 );
  tgt[4] = hi + c;    // note: cannot overflow because `hi <= 2^64-2`
  // limb # 4
  x = ((__uint128_t) src[4]) * z;
  lo = (uint64_t) x;
  hi = (uint64_t)(x >> 64);
  c = addcarry_u64( 0, tgt[4], lo, tgt+4 );
  tgt[5] = hi + c;    // note: cannot overflow because `hi <= 2^64-2`
  // limb # 5
  x = ((__uint128_t) src[5]) * z;
  lo = (uint64_t) x;
  hi = (uint64_t)(x >> 64);
  c = addcarry_u64( 0, tgt[5], lo, tgt+5 );
  tgt[6] = hi + c;    // note: cannot overflow because `hi <= 2^64-2`
  // limb # 6
  x = ((__uint128_t) src[6]) * z;
  lo = (uint64_t) x;
  hi = (uint64_t)(x >> 64);
  c = addcarry_u64( 0, tgt[6], lo, tgt+6 );
  tgt[7] = hi + c;    // note: cannot overflow because `hi <= 2^64-2`
  // limb # 7
  x = ((__uint128_t) src[7]) * z;
  lo = (uint64_t) x;
  hi = (uint64_t)(x >> 64);
  c = addcarry_u64( 0, tgt[7], lo, tgt+7 );
  tgt[8] = hi + c;    // note: cannot overflow because `hi <= 2^64-2`
  // limb # 8
  x = ((__uint128_t) src[8]) * z;
  lo = (uint64_t) x;
  hi = (uint64_t)(x >> 64);
  c = addcarry_u64( 0, tgt[8], lo, tgt+8 );
  tgt[9] = hi + c;    // note: cannot overflow because `hi <= 2^64-2`
  // limb # 9
  x = ((__uint128_t) src[9]) * z;
  lo = (uint64_t) x;
  hi = (uint64_t)(x >> 64);
  c = addcarry_u64( 0, tgt[9], lo, tgt+9 );
  tgt[10] = hi + c;    // note: cannot overflow because `hi <= 2^64-2`
  // limb # 10
  x = ((__uint128_t) src[10]) * z;
  lo = (uint64_t) x;
  hi = (uint64_t)(x >> 64);
  c = addcarry_u64( 0, tgt[10], lo, tgt+10 );
  tgt[11] = hi + c;    // note: cannot overflow because `hi <= 2^64-2`
  // limb # 11
  x = ((__uint128_t) src[11]) * z;
  lo = (uint64_t) x;
  hi = (uint64_t)(x >> 64);
  c = addcarry_u64( 0, tgt[11], lo, tgt+11 );
  tgt[12] = hi + c;    // note: cannot overflow because `hi <= 2^64-2`
}

// squares an (unsigned) big integer of 12 limbs
// note: `tgt` must have space for 24 limbs!
void bigint768_sqr( const uint64_t *src, uint64_t *tgt ) {
  __uint128_t prod;
  uint64_t prod_hi, prod_lo;
  uint64_t carry;
  for(int m=0; m<24; m++) { tgt[m] = 0; }
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
  prod = ((__uint128_t) src[0]) * src[5];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+5, tgt+6, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+5, tgt+6, prod_lo, prod_hi );
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
  prod = ((__uint128_t) src[0]) * src[6];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+6, tgt+7, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+6, tgt+7, prod_lo, prod_hi );
  prod = ((__uint128_t) src[1]) * src[5];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+6, tgt+7, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+6, tgt+7, prod_lo, prod_hi );
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
  prod = ((__uint128_t) src[0]) * src[7];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+7, tgt+8, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+7, tgt+8, prod_lo, prod_hi );
  prod = ((__uint128_t) src[1]) * src[6];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+7, tgt+8, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+7, tgt+8, prod_lo, prod_hi );
  prod = ((__uint128_t) src[2]) * src[5];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+7, tgt+8, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+7, tgt+8, prod_lo, prod_hi );
  prod = ((__uint128_t) src[3]) * src[4];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+7, tgt+8, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+7, tgt+8, prod_lo, prod_hi );
  tgt[9] = carry;
  // *** m = 8 ***
  carry = 0;
  prod = ((__uint128_t) src[0]) * src[8];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+8, tgt+9, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+8, tgt+9, prod_lo, prod_hi );
  prod = ((__uint128_t) src[1]) * src[7];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+8, tgt+9, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+8, tgt+9, prod_lo, prod_hi );
  prod = ((__uint128_t) src[2]) * src[6];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+8, tgt+9, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+8, tgt+9, prod_lo, prod_hi );
  prod = ((__uint128_t) src[3]) * src[5];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+8, tgt+9, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+8, tgt+9, prod_lo, prod_hi );
  prod = ((__uint128_t) src[4]) * src[4];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+8, tgt+9, prod_lo, prod_hi );
  tgt[10] = carry;
  // *** m = 9 ***
  carry = 0;
  prod = ((__uint128_t) src[0]) * src[9];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+9, tgt+10, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+9, tgt+10, prod_lo, prod_hi );
  prod = ((__uint128_t) src[1]) * src[8];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+9, tgt+10, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+9, tgt+10, prod_lo, prod_hi );
  prod = ((__uint128_t) src[2]) * src[7];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+9, tgt+10, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+9, tgt+10, prod_lo, prod_hi );
  prod = ((__uint128_t) src[3]) * src[6];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+9, tgt+10, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+9, tgt+10, prod_lo, prod_hi );
  prod = ((__uint128_t) src[4]) * src[5];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+9, tgt+10, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+9, tgt+10, prod_lo, prod_hi );
  tgt[11] = carry;
  // *** m = 10 ***
  carry = 0;
  prod = ((__uint128_t) src[0]) * src[10];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+10, tgt+11, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+10, tgt+11, prod_lo, prod_hi );
  prod = ((__uint128_t) src[1]) * src[9];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+10, tgt+11, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+10, tgt+11, prod_lo, prod_hi );
  prod = ((__uint128_t) src[2]) * src[8];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+10, tgt+11, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+10, tgt+11, prod_lo, prod_hi );
  prod = ((__uint128_t) src[3]) * src[7];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+10, tgt+11, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+10, tgt+11, prod_lo, prod_hi );
  prod = ((__uint128_t) src[4]) * src[6];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+10, tgt+11, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+10, tgt+11, prod_lo, prod_hi );
  prod = ((__uint128_t) src[5]) * src[5];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+10, tgt+11, prod_lo, prod_hi );
  tgt[12] = carry;
  // *** m = 11 ***
  carry = 0;
  prod = ((__uint128_t) src[0]) * src[11];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+11, tgt+12, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+11, tgt+12, prod_lo, prod_hi );
  prod = ((__uint128_t) src[1]) * src[10];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+11, tgt+12, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+11, tgt+12, prod_lo, prod_hi );
  prod = ((__uint128_t) src[2]) * src[9];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+11, tgt+12, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+11, tgt+12, prod_lo, prod_hi );
  prod = ((__uint128_t) src[3]) * src[8];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+11, tgt+12, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+11, tgt+12, prod_lo, prod_hi );
  prod = ((__uint128_t) src[4]) * src[7];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+11, tgt+12, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+11, tgt+12, prod_lo, prod_hi );
  prod = ((__uint128_t) src[5]) * src[6];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+11, tgt+12, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+11, tgt+12, prod_lo, prod_hi );
  tgt[13] = carry;
  // *** m = 12 ***
  carry = 0;
  prod = ((__uint128_t) src[1]) * src[11];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+12, tgt+13, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+12, tgt+13, prod_lo, prod_hi );
  prod = ((__uint128_t) src[2]) * src[10];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+12, tgt+13, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+12, tgt+13, prod_lo, prod_hi );
  prod = ((__uint128_t) src[3]) * src[9];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+12, tgt+13, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+12, tgt+13, prod_lo, prod_hi );
  prod = ((__uint128_t) src[4]) * src[8];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+12, tgt+13, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+12, tgt+13, prod_lo, prod_hi );
  prod = ((__uint128_t) src[5]) * src[7];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+12, tgt+13, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+12, tgt+13, prod_lo, prod_hi );
  prod = ((__uint128_t) src[6]) * src[6];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+12, tgt+13, prod_lo, prod_hi );
  tgt[14] = carry;
  // *** m = 13 ***
  carry = 0;
  prod = ((__uint128_t) src[2]) * src[11];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+13, tgt+14, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+13, tgt+14, prod_lo, prod_hi );
  prod = ((__uint128_t) src[3]) * src[10];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+13, tgt+14, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+13, tgt+14, prod_lo, prod_hi );
  prod = ((__uint128_t) src[4]) * src[9];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+13, tgt+14, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+13, tgt+14, prod_lo, prod_hi );
  prod = ((__uint128_t) src[5]) * src[8];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+13, tgt+14, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+13, tgt+14, prod_lo, prod_hi );
  prod = ((__uint128_t) src[6]) * src[7];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+13, tgt+14, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+13, tgt+14, prod_lo, prod_hi );
  tgt[15] = carry;
  // *** m = 14 ***
  carry = 0;
  prod = ((__uint128_t) src[3]) * src[11];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+14, tgt+15, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+14, tgt+15, prod_lo, prod_hi );
  prod = ((__uint128_t) src[4]) * src[10];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+14, tgt+15, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+14, tgt+15, prod_lo, prod_hi );
  prod = ((__uint128_t) src[5]) * src[9];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+14, tgt+15, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+14, tgt+15, prod_lo, prod_hi );
  prod = ((__uint128_t) src[6]) * src[8];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+14, tgt+15, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+14, tgt+15, prod_lo, prod_hi );
  prod = ((__uint128_t) src[7]) * src[7];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+14, tgt+15, prod_lo, prod_hi );
  tgt[16] = carry;
  // *** m = 15 ***
  carry = 0;
  prod = ((__uint128_t) src[4]) * src[11];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+15, tgt+16, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+15, tgt+16, prod_lo, prod_hi );
  prod = ((__uint128_t) src[5]) * src[10];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+15, tgt+16, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+15, tgt+16, prod_lo, prod_hi );
  prod = ((__uint128_t) src[6]) * src[9];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+15, tgt+16, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+15, tgt+16, prod_lo, prod_hi );
  prod = ((__uint128_t) src[7]) * src[8];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+15, tgt+16, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+15, tgt+16, prod_lo, prod_hi );
  tgt[17] = carry;
  // *** m = 16 ***
  carry = 0;
  prod = ((__uint128_t) src[5]) * src[11];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+16, tgt+17, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+16, tgt+17, prod_lo, prod_hi );
  prod = ((__uint128_t) src[6]) * src[10];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+16, tgt+17, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+16, tgt+17, prod_lo, prod_hi );
  prod = ((__uint128_t) src[7]) * src[9];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+16, tgt+17, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+16, tgt+17, prod_lo, prod_hi );
  prod = ((__uint128_t) src[8]) * src[8];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+16, tgt+17, prod_lo, prod_hi );
  tgt[18] = carry;
  // *** m = 17 ***
  carry = 0;
  prod = ((__uint128_t) src[6]) * src[11];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+17, tgt+18, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+17, tgt+18, prod_lo, prod_hi );
  prod = ((__uint128_t) src[7]) * src[10];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+17, tgt+18, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+17, tgt+18, prod_lo, prod_hi );
  prod = ((__uint128_t) src[8]) * src[9];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+17, tgt+18, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+17, tgt+18, prod_lo, prod_hi );
  tgt[19] = carry;
  // *** m = 18 ***
  carry = 0;
  prod = ((__uint128_t) src[7]) * src[11];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+18, tgt+19, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+18, tgt+19, prod_lo, prod_hi );
  prod = ((__uint128_t) src[8]) * src[10];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+18, tgt+19, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+18, tgt+19, prod_lo, prod_hi );
  prod = ((__uint128_t) src[9]) * src[9];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+18, tgt+19, prod_lo, prod_hi );
  tgt[20] = carry;
  // *** m = 19 ***
  carry = 0;
  prod = ((__uint128_t) src[8]) * src[11];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+19, tgt+20, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+19, tgt+20, prod_lo, prod_hi );
  prod = ((__uint128_t) src[9]) * src[10];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+19, tgt+20, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+19, tgt+20, prod_lo, prod_hi );
  tgt[21] = carry;
  // *** m = 20 ***
  carry = 0;
  prod = ((__uint128_t) src[9]) * src[11];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+20, tgt+21, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+20, tgt+21, prod_lo, prod_hi );
  prod = ((__uint128_t) src[10]) * src[10];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+20, tgt+21, prod_lo, prod_hi );
  tgt[22] = carry;
  // *** m = 21 ***
  carry = 0;
  prod = ((__uint128_t) src[10]) * src[11];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+21, tgt+22, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+21, tgt+22, prod_lo, prod_hi );
  tgt[23] = carry;
  // *** m = 22 ***
  carry = 0;
  prod = ((__uint128_t) src[11]) * src[11];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+22, tgt+23, prod_lo, prod_hi );
}

// multiplies two (unsigned) big integers of 12 limbs
// note: `tgt` must have space for 24 limbs!
void bigint768_mul( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  __uint128_t prod;
  uint64_t prod_hi, prod_lo;
  uint64_t carry;
  for(int m=0; m<24; m++) { tgt[m] = 0; }
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
  prod = ((__uint128_t) src1[0]) * src2[5];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+5, tgt+6, prod_lo, prod_hi );
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
  prod = ((__uint128_t) src1[5]) * src2[0];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+5, tgt+6, prod_lo, prod_hi );
  tgt[7] = carry;
  // *** m = 6 ***
  carry  = 0;
  prod = ((__uint128_t) src1[0]) * src2[6];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+6, tgt+7, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[1]) * src2[5];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+6, tgt+7, prod_lo, prod_hi );
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
  prod = ((__uint128_t) src1[5]) * src2[1];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+6, tgt+7, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[6]) * src2[0];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+6, tgt+7, prod_lo, prod_hi );
  tgt[8] = carry;
  // *** m = 7 ***
  carry  = 0;
  prod = ((__uint128_t) src1[0]) * src2[7];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+7, tgt+8, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[1]) * src2[6];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+7, tgt+8, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[2]) * src2[5];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+7, tgt+8, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[3]) * src2[4];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+7, tgt+8, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[4]) * src2[3];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+7, tgt+8, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[5]) * src2[2];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+7, tgt+8, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[6]) * src2[1];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+7, tgt+8, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[7]) * src2[0];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+7, tgt+8, prod_lo, prod_hi );
  tgt[9] = carry;
  // *** m = 8 ***
  carry  = 0;
  prod = ((__uint128_t) src1[0]) * src2[8];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+8, tgt+9, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[1]) * src2[7];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+8, tgt+9, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[2]) * src2[6];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+8, tgt+9, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[3]) * src2[5];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+8, tgt+9, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[4]) * src2[4];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+8, tgt+9, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[5]) * src2[3];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+8, tgt+9, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[6]) * src2[2];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+8, tgt+9, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[7]) * src2[1];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+8, tgt+9, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[8]) * src2[0];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+8, tgt+9, prod_lo, prod_hi );
  tgt[10] = carry;
  // *** m = 9 ***
  carry  = 0;
  prod = ((__uint128_t) src1[0]) * src2[9];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+9, tgt+10, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[1]) * src2[8];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+9, tgt+10, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[2]) * src2[7];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+9, tgt+10, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[3]) * src2[6];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+9, tgt+10, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[4]) * src2[5];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+9, tgt+10, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[5]) * src2[4];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+9, tgt+10, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[6]) * src2[3];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+9, tgt+10, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[7]) * src2[2];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+9, tgt+10, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[8]) * src2[1];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+9, tgt+10, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[9]) * src2[0];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+9, tgt+10, prod_lo, prod_hi );
  tgt[11] = carry;
  // *** m = 10 ***
  carry  = 0;
  prod = ((__uint128_t) src1[0]) * src2[10];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+10, tgt+11, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[1]) * src2[9];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+10, tgt+11, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[2]) * src2[8];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+10, tgt+11, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[3]) * src2[7];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+10, tgt+11, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[4]) * src2[6];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+10, tgt+11, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[5]) * src2[5];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+10, tgt+11, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[6]) * src2[4];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+10, tgt+11, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[7]) * src2[3];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+10, tgt+11, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[8]) * src2[2];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+10, tgt+11, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[9]) * src2[1];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+10, tgt+11, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[10]) * src2[0];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+10, tgt+11, prod_lo, prod_hi );
  tgt[12] = carry;
  // *** m = 11 ***
  carry  = 0;
  prod = ((__uint128_t) src1[0]) * src2[11];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+11, tgt+12, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[1]) * src2[10];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+11, tgt+12, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[2]) * src2[9];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+11, tgt+12, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[3]) * src2[8];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+11, tgt+12, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[4]) * src2[7];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+11, tgt+12, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[5]) * src2[6];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+11, tgt+12, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[6]) * src2[5];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+11, tgt+12, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[7]) * src2[4];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+11, tgt+12, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[8]) * src2[3];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+11, tgt+12, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[9]) * src2[2];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+11, tgt+12, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[10]) * src2[1];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+11, tgt+12, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[11]) * src2[0];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+11, tgt+12, prod_lo, prod_hi );
  tgt[13] = carry;
  // *** m = 12 ***
  carry  = 0;
  prod = ((__uint128_t) src1[1]) * src2[11];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+12, tgt+13, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[2]) * src2[10];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+12, tgt+13, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[3]) * src2[9];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+12, tgt+13, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[4]) * src2[8];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+12, tgt+13, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[5]) * src2[7];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+12, tgt+13, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[6]) * src2[6];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+12, tgt+13, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[7]) * src2[5];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+12, tgt+13, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[8]) * src2[4];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+12, tgt+13, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[9]) * src2[3];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+12, tgt+13, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[10]) * src2[2];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+12, tgt+13, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[11]) * src2[1];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+12, tgt+13, prod_lo, prod_hi );
  tgt[14] = carry;
  // *** m = 13 ***
  carry  = 0;
  prod = ((__uint128_t) src1[2]) * src2[11];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+13, tgt+14, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[3]) * src2[10];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+13, tgt+14, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[4]) * src2[9];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+13, tgt+14, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[5]) * src2[8];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+13, tgt+14, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[6]) * src2[7];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+13, tgt+14, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[7]) * src2[6];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+13, tgt+14, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[8]) * src2[5];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+13, tgt+14, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[9]) * src2[4];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+13, tgt+14, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[10]) * src2[3];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+13, tgt+14, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[11]) * src2[2];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+13, tgt+14, prod_lo, prod_hi );
  tgt[15] = carry;
  // *** m = 14 ***
  carry  = 0;
  prod = ((__uint128_t) src1[3]) * src2[11];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+14, tgt+15, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[4]) * src2[10];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+14, tgt+15, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[5]) * src2[9];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+14, tgt+15, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[6]) * src2[8];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+14, tgt+15, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[7]) * src2[7];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+14, tgt+15, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[8]) * src2[6];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+14, tgt+15, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[9]) * src2[5];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+14, tgt+15, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[10]) * src2[4];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+14, tgt+15, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[11]) * src2[3];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+14, tgt+15, prod_lo, prod_hi );
  tgt[16] = carry;
  // *** m = 15 ***
  carry  = 0;
  prod = ((__uint128_t) src1[4]) * src2[11];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+15, tgt+16, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[5]) * src2[10];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+15, tgt+16, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[6]) * src2[9];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+15, tgt+16, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[7]) * src2[8];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+15, tgt+16, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[8]) * src2[7];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+15, tgt+16, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[9]) * src2[6];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+15, tgt+16, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[10]) * src2[5];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+15, tgt+16, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[11]) * src2[4];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+15, tgt+16, prod_lo, prod_hi );
  tgt[17] = carry;
  // *** m = 16 ***
  carry  = 0;
  prod = ((__uint128_t) src1[5]) * src2[11];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+16, tgt+17, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[6]) * src2[10];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+16, tgt+17, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[7]) * src2[9];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+16, tgt+17, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[8]) * src2[8];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+16, tgt+17, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[9]) * src2[7];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+16, tgt+17, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[10]) * src2[6];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+16, tgt+17, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[11]) * src2[5];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+16, tgt+17, prod_lo, prod_hi );
  tgt[18] = carry;
  // *** m = 17 ***
  carry  = 0;
  prod = ((__uint128_t) src1[6]) * src2[11];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+17, tgt+18, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[7]) * src2[10];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+17, tgt+18, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[8]) * src2[9];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+17, tgt+18, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[9]) * src2[8];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+17, tgt+18, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[10]) * src2[7];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+17, tgt+18, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[11]) * src2[6];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+17, tgt+18, prod_lo, prod_hi );
  tgt[19] = carry;
  // *** m = 18 ***
  carry  = 0;
  prod = ((__uint128_t) src1[7]) * src2[11];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+18, tgt+19, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[8]) * src2[10];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+18, tgt+19, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[9]) * src2[9];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+18, tgt+19, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[10]) * src2[8];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+18, tgt+19, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[11]) * src2[7];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+18, tgt+19, prod_lo, prod_hi );
  tgt[20] = carry;
  // *** m = 19 ***
  carry  = 0;
  prod = ((__uint128_t) src1[8]) * src2[11];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+19, tgt+20, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[9]) * src2[10];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+19, tgt+20, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[10]) * src2[9];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+19, tgt+20, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[11]) * src2[8];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+19, tgt+20, prod_lo, prod_hi );
  tgt[21] = carry;
  // *** m = 20 ***
  carry  = 0;
  prod = ((__uint128_t) src1[9]) * src2[11];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+20, tgt+21, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[10]) * src2[10];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+20, tgt+21, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[11]) * src2[9];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+20, tgt+21, prod_lo, prod_hi );
  tgt[22] = carry;
  // *** m = 21 ***
  carry  = 0;
  prod = ((__uint128_t) src1[10]) * src2[11];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+21, tgt+22, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[11]) * src2[10];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+21, tgt+22, prod_lo, prod_hi );
  tgt[23] = carry;
  // *** m = 22 ***
  carry  = 0;
  prod = ((__uint128_t) src1[11]) * src2[11];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+22, tgt+23, prod_lo, prod_hi );
}

// multiplies two (unsigned) big integers of 12 limbs,
// and *truncates* the result 12 limbs
// (so this gives the ring of integers modulo 2^768)
void bigint768_mul_truncated( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  __uint128_t prod;
  uint64_t prod_hi, prod_lo;
  uint64_t carry;
  for(int m=0; m<12; m++) { tgt[m] = 0; }
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
  prod = ((__uint128_t) src1[0]) * src2[5];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+5, tgt+6, prod_lo, prod_hi );
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
  prod = ((__uint128_t) src1[5]) * src2[0];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+5, tgt+6, prod_lo, prod_hi );
  tgt[7] = carry;
  // *** m = 6 ***
  carry  = 0;
  prod = ((__uint128_t) src1[0]) * src2[6];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+6, tgt+7, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[1]) * src2[5];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+6, tgt+7, prod_lo, prod_hi );
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
  prod = ((__uint128_t) src1[5]) * src2[1];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+6, tgt+7, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[6]) * src2[0];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+6, tgt+7, prod_lo, prod_hi );
  tgt[8] = carry;
  // *** m = 7 ***
  carry  = 0;
  prod = ((__uint128_t) src1[0]) * src2[7];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+7, tgt+8, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[1]) * src2[6];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+7, tgt+8, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[2]) * src2[5];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+7, tgt+8, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[3]) * src2[4];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+7, tgt+8, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[4]) * src2[3];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+7, tgt+8, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[5]) * src2[2];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+7, tgt+8, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[6]) * src2[1];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+7, tgt+8, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[7]) * src2[0];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+7, tgt+8, prod_lo, prod_hi );
  tgt[9] = carry;
  // *** m = 8 ***
  carry  = 0;
  prod = ((__uint128_t) src1[0]) * src2[8];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+8, tgt+9, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[1]) * src2[7];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+8, tgt+9, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[2]) * src2[6];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+8, tgt+9, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[3]) * src2[5];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+8, tgt+9, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[4]) * src2[4];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+8, tgt+9, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[5]) * src2[3];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+8, tgt+9, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[6]) * src2[2];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+8, tgt+9, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[7]) * src2[1];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+8, tgt+9, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[8]) * src2[0];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+8, tgt+9, prod_lo, prod_hi );
  tgt[10] = carry;
  // *** m = 9 ***
  carry  = 0;
  prod = ((__uint128_t) src1[0]) * src2[9];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+9, tgt+10, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[1]) * src2[8];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+9, tgt+10, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[2]) * src2[7];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+9, tgt+10, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[3]) * src2[6];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+9, tgt+10, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[4]) * src2[5];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+9, tgt+10, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[5]) * src2[4];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+9, tgt+10, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[6]) * src2[3];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+9, tgt+10, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[7]) * src2[2];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+9, tgt+10, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[8]) * src2[1];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+9, tgt+10, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[9]) * src2[0];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+9, tgt+10, prod_lo, prod_hi );
  tgt[11] = carry;
  // *** m = 10 ***
  carry  = 0;
  prod = ((__uint128_t) src1[0]) * src2[10];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+10, tgt+11, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[1]) * src2[9];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+10, tgt+11, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[2]) * src2[8];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+10, tgt+11, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[3]) * src2[7];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+10, tgt+11, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[4]) * src2[6];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+10, tgt+11, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[5]) * src2[5];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+10, tgt+11, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[6]) * src2[4];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+10, tgt+11, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[7]) * src2[3];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+10, tgt+11, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[8]) * src2[2];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+10, tgt+11, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[9]) * src2[1];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+10, tgt+11, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[10]) * src2[0];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+10, tgt+11, prod_lo, prod_hi );
  // *** m = 11 ***
  carry  = 0;
  prod = ((__uint128_t) src1[0]) * src2[11];
  prod_lo = (uint64_t)(prod      );
  tgt[11] += prod_lo;
  prod = ((__uint128_t) src1[1]) * src2[10];
  prod_lo = (uint64_t)(prod      );
  tgt[11] += prod_lo;
  prod = ((__uint128_t) src1[2]) * src2[9];
  prod_lo = (uint64_t)(prod      );
  tgt[11] += prod_lo;
  prod = ((__uint128_t) src1[3]) * src2[8];
  prod_lo = (uint64_t)(prod      );
  tgt[11] += prod_lo;
  prod = ((__uint128_t) src1[4]) * src2[7];
  prod_lo = (uint64_t)(prod      );
  tgt[11] += prod_lo;
  prod = ((__uint128_t) src1[5]) * src2[6];
  prod_lo = (uint64_t)(prod      );
  tgt[11] += prod_lo;
  prod = ((__uint128_t) src1[6]) * src2[5];
  prod_lo = (uint64_t)(prod      );
  tgt[11] += prod_lo;
  prod = ((__uint128_t) src1[7]) * src2[4];
  prod_lo = (uint64_t)(prod      );
  tgt[11] += prod_lo;
  prod = ((__uint128_t) src1[8]) * src2[3];
  prod_lo = (uint64_t)(prod      );
  tgt[11] += prod_lo;
  prod = ((__uint128_t) src1[9]) * src2[2];
  prod_lo = (uint64_t)(prod      );
  tgt[11] += prod_lo;
  prod = ((__uint128_t) src1[10]) * src2[1];
  prod_lo = (uint64_t)(prod      );
  tgt[11] += prod_lo;
  prod = ((__uint128_t) src1[11]) * src2[0];
  prod_lo = (uint64_t)(prod      );
  tgt[11] += prod_lo;
}

// squares an (unsigned) big integers of 12 limbs,
// and *truncates* the result 12 limbs
// (so this gives the ring of integers modulo 2^768)
void bigint768_sqr_truncated( const uint64_t *src1, uint64_t *tgt ) {
  __uint128_t prod;
  uint64_t prod_hi, prod_lo;
  uint64_t carry;
  for(int m=0; m<12; m++) { tgt[m] = 0; }
  // *** m = 0 ***
  carry  = 0;
  prod = ((__uint128_t) src1[0]) * src1[0];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+0, tgt+1, prod_lo, prod_hi );
  tgt[2] = carry;
  // *** m = 1 ***
  carry  = 0;
  prod = ((__uint128_t) src1[0]) * src1[1];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+1, tgt+2, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+1, tgt+2, prod_lo, prod_hi );
  tgt[3] = carry;
  // *** m = 2 ***
  carry  = 0;
  prod = ((__uint128_t) src1[0]) * src1[2];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+2, tgt+3, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+2, tgt+3, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[1]) * src1[1];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+2, tgt+3, prod_lo, prod_hi );
  tgt[4] = carry;
  // *** m = 3 ***
  carry  = 0;
  prod = ((__uint128_t) src1[0]) * src1[3];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+3, tgt+4, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+3, tgt+4, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[1]) * src1[2];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+3, tgt+4, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+3, tgt+4, prod_lo, prod_hi );
  tgt[5] = carry;
  // *** m = 4 ***
  carry  = 0;
  prod = ((__uint128_t) src1[0]) * src1[4];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+4, tgt+5, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+4, tgt+5, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[1]) * src1[3];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+4, tgt+5, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+4, tgt+5, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[2]) * src1[2];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+4, tgt+5, prod_lo, prod_hi );
  tgt[6] = carry;
  // *** m = 5 ***
  carry  = 0;
  prod = ((__uint128_t) src1[0]) * src1[5];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+5, tgt+6, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+5, tgt+6, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[1]) * src1[4];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+5, tgt+6, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+5, tgt+6, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[2]) * src1[3];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+5, tgt+6, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+5, tgt+6, prod_lo, prod_hi );
  tgt[7] = carry;
  // *** m = 6 ***
  carry  = 0;
  prod = ((__uint128_t) src1[0]) * src1[6];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+6, tgt+7, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+6, tgt+7, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[1]) * src1[5];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+6, tgt+7, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+6, tgt+7, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[2]) * src1[4];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+6, tgt+7, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+6, tgt+7, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[3]) * src1[3];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+6, tgt+7, prod_lo, prod_hi );
  tgt[8] = carry;
  // *** m = 7 ***
  carry  = 0;
  prod = ((__uint128_t) src1[0]) * src1[7];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+7, tgt+8, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+7, tgt+8, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[1]) * src1[6];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+7, tgt+8, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+7, tgt+8, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[2]) * src1[5];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+7, tgt+8, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+7, tgt+8, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[3]) * src1[4];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+7, tgt+8, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+7, tgt+8, prod_lo, prod_hi );
  tgt[9] = carry;
  // *** m = 8 ***
  carry  = 0;
  prod = ((__uint128_t) src1[0]) * src1[8];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+8, tgt+9, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+8, tgt+9, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[1]) * src1[7];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+8, tgt+9, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+8, tgt+9, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[2]) * src1[6];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+8, tgt+9, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+8, tgt+9, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[3]) * src1[5];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+8, tgt+9, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+8, tgt+9, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[4]) * src1[4];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+8, tgt+9, prod_lo, prod_hi );
  tgt[10] = carry;
  // *** m = 9 ***
  carry  = 0;
  prod = ((__uint128_t) src1[0]) * src1[9];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+9, tgt+10, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+9, tgt+10, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[1]) * src1[8];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+9, tgt+10, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+9, tgt+10, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[2]) * src1[7];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+9, tgt+10, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+9, tgt+10, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[3]) * src1[6];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+9, tgt+10, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+9, tgt+10, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[4]) * src1[5];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+9, tgt+10, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+9, tgt+10, prod_lo, prod_hi );
  tgt[11] = carry;
  // *** m = 10 ***
  carry  = 0;
  prod = ((__uint128_t) src1[0]) * src1[10];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+10, tgt+11, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+10, tgt+11, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[1]) * src1[9];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+10, tgt+11, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+10, tgt+11, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[2]) * src1[8];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+10, tgt+11, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+10, tgt+11, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[3]) * src1[7];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+10, tgt+11, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+10, tgt+11, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[4]) * src1[6];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+10, tgt+11, prod_lo, prod_hi );
  carry += addcarry_u128_inplace( tgt+10, tgt+11, prod_lo, prod_hi );
  prod = ((__uint128_t) src1[5]) * src1[5];
  prod_lo = (uint64_t)(prod      );
  prod_hi = (uint64_t)(prod >> 64);
  carry += addcarry_u128_inplace( tgt+10, tgt+11, prod_lo, prod_hi );
  // *** m = 11 ***
  carry  = 0;
  prod = ((__uint128_t) src1[0]) * src1[11];
  prod_lo = (uint64_t)(prod      );
  tgt[11] += (2*prod_lo);
  prod = ((__uint128_t) src1[1]) * src1[10];
  prod_lo = (uint64_t)(prod      );
  tgt[11] += (2*prod_lo);
  prod = ((__uint128_t) src1[2]) * src1[9];
  prod_lo = (uint64_t)(prod      );
  tgt[11] += (2*prod_lo);
  prod = ((__uint128_t) src1[3]) * src1[8];
  prod_lo = (uint64_t)(prod      );
  tgt[11] += (2*prod_lo);
  prod = ((__uint128_t) src1[4]) * src1[7];
  prod_lo = (uint64_t)(prod      );
  tgt[11] += (2*prod_lo);
  prod = ((__uint128_t) src1[5]) * src1[6];
  prod_lo = (uint64_t)(prod      );
  tgt[11] += (2*prod_lo);
}

// shift left by 1 bit
uint8_t bigint768_shift_left_by_1( const uint64_t *src, uint64_t *tgt) {
  uint64_t tmp[12];
  tmp[0] = (src[0] << 1) ;
  tmp[1] = (src[1] << 1) | (src[0] >> 63) ;
  tmp[2] = (src[2] << 1) | (src[1] >> 63) ;
  tmp[3] = (src[3] << 1) | (src[2] >> 63) ;
  tmp[4] = (src[4] << 1) | (src[3] >> 63) ;
  tmp[5] = (src[5] << 1) | (src[4] >> 63) ;
  tmp[6] = (src[6] << 1) | (src[5] >> 63) ;
  tmp[7] = (src[7] << 1) | (src[6] >> 63) ;
  tmp[8] = (src[8] << 1) | (src[7] >> 63) ;
  tmp[9] = (src[9] << 1) | (src[8] >> 63) ;
  tmp[10] = (src[10] << 1) | (src[9] >> 63) ;
  tmp[11] = (src[11] << 1) | (src[10] >> 63) ;
  uint8_t c = src[11] >> 63;
  memcpy( tgt, tmp, 96 );
  return c;
}

// shift left by k bits, for 0 <= k < 64
uint64_t bigint768_shift_left_by_small( const uint64_t *src, uint64_t *tgt, int by ) {
  assert( (by >= 0) && (by < 64) );
  uint64_t tmp[12];
  tmp[0] = (src[0] << by) ;
  tmp[1] = (src[1] << by) | (src[0] >> (64-by)) ;
  tmp[2] = (src[2] << by) | (src[1] >> (64-by)) ;
  tmp[3] = (src[3] << by) | (src[2] >> (64-by)) ;
  tmp[4] = (src[4] << by) | (src[3] >> (64-by)) ;
  tmp[5] = (src[5] << by) | (src[4] >> (64-by)) ;
  tmp[6] = (src[6] << by) | (src[5] >> (64-by)) ;
  tmp[7] = (src[7] << by) | (src[6] >> (64-by)) ;
  tmp[8] = (src[8] << by) | (src[7] >> (64-by)) ;
  tmp[9] = (src[9] << by) | (src[8] >> (64-by)) ;
  tmp[10] = (src[10] << by) | (src[9] >> (64-by)) ;
  tmp[11] = (src[11] << by) | (src[10] >> (64-by)) ;
  uint64_t c = src[11] >> (64-by);
  memcpy( tgt, tmp, 96 );
  return c;
}

// shift left by k bits, for k >= 0
void bigint768_shift_left_by_k( const uint64_t *src, uint64_t *tgt, int by ) {
  assert(by >= 0);
  int small = by % 64;
  int move  = by / 64;
  if (by >= 768) {
    bigint768_set_zero(tgt);
  } 
  else {
    for(int i=0    ; i<move; i++) { tgt[i] = 0; }
    for(int i=move ; i<12     ; i++) { tgt[i] = src[i-move]; }
    bigint768_shift_left_by_small(tgt, tgt, small);
  }
}

// shift right by 1 bit
uint8_t bigint768_shift_right_by_1( const uint64_t *src, uint64_t *tgt) {
  uint64_t tmp[12];
  tmp[11] = (src[11] >> 1) ;
  tmp[10] = (src[10] >> 1) | (src[11] << 63) ;
  tmp[9] = (src[9] >> 1) | (src[10] << 63) ;
  tmp[8] = (src[8] >> 1) | (src[9] << 63) ;
  tmp[7] = (src[7] >> 1) | (src[8] << 63) ;
  tmp[6] = (src[6] >> 1) | (src[7] << 63) ;
  tmp[5] = (src[5] >> 1) | (src[6] << 63) ;
  tmp[4] = (src[4] >> 1) | (src[5] << 63) ;
  tmp[3] = (src[3] >> 1) | (src[4] << 63) ;
  tmp[2] = (src[2] >> 1) | (src[3] << 63) ;
  tmp[1] = (src[1] >> 1) | (src[2] << 63) ;
  tmp[0] = (src[0] >> 1) | (src[1] << 63) ;
  uint8_t c = src[0] & 1;
  memcpy( tgt, tmp, 96 );
  return c;
}

// shift right by k bits, for 0 <= k < 64
uint64_t bigint768_shift_right_by_small( const uint64_t *src, uint64_t *tgt, int by ) {
  assert( (by >= 0) && (by < 64) );
  uint64_t tmp[12];
  tmp[11] = (src[11] >> by) ;
  tmp[10] = (src[10] >> by) | (src[11] << (64-by)) ;
  tmp[9] = (src[9] >> by) | (src[10] << (64-by)) ;
  tmp[8] = (src[8] >> by) | (src[9] << (64-by)) ;
  tmp[7] = (src[7] >> by) | (src[8] << (64-by)) ;
  tmp[6] = (src[6] >> by) | (src[7] << (64-by)) ;
  tmp[5] = (src[5] >> by) | (src[6] << (64-by)) ;
  tmp[4] = (src[4] >> by) | (src[5] << (64-by)) ;
  tmp[3] = (src[3] >> by) | (src[4] << (64-by)) ;
  tmp[2] = (src[2] >> by) | (src[3] << (64-by)) ;
  tmp[1] = (src[1] >> by) | (src[2] << (64-by)) ;
  tmp[0] = (src[0] >> by) | (src[1] << (64-by)) ;
  uint64_t c = src[0] & ((1<<by) - 1);
  memcpy( tgt, tmp, 96 );
  return c;
}

// shift right by k bits, for k >= 0
void bigint768_shift_right_by_k( const uint64_t *src, uint64_t *tgt, int by ) {
  assert(by >= 0);
  int small = by % 64;
  int move  = by / 64;
  if (by >= 768) {
    bigint768_set_zero(tgt);
  } 
  else {
    for(int i=0     ; i<12-move; i++) { tgt[i] = src[i+move]; }
    for(int i=12-move; i<12     ; i++) { tgt[i] = 0;           }
    bigint768_shift_right_by_small(tgt, tgt, small);
  }
}
