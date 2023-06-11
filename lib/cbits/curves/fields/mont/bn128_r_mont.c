
// finite field arithmetic in Montgomery representation, in the prime field with 
//
//   p = 21888242871839275222246405745257275088548364400416034343698204186575808495617
//
// NOTE: generated code, do not edit!

#include <string.h>
#include <stdint.h>
#include <x86intrin.h>
#include "bn128_r_mont.h"
#include "bn128_r_std.h"
#include "bigint256.h"

#define NLIMBS 4

const uint64_t bn128_r_mont_prime[4] = { 0x43e1f593f0000001, 0x2833e84879b97091, 0xb85045b68181585d, 0x30644e72e131a029 };

inline uint8_t addcarry_u128_inplace(  uint64_t *tgt_lo, uint64_t *tgt_hi, uint64_t arg_lo, uint64_t arg_hi) {
  uint8_t c;
  c = _addcarry_u64( 0, *tgt_lo, arg_lo, tgt_lo );
  c = _addcarry_u64( c, *tgt_hi, arg_hi, tgt_hi );
  return c;
}

//------------------------------------------------------------------------------

// adds the prime p to a bigint, inplace
uint8_t bn128_r_mont_bigint256_add_prime_inplace( uint64_t *tgt ) {
  return bigint256_add_inplace( tgt, bn128_r_mont_prime);
}

// the constant `p + 1`
const uint64_t bn128_r_mont_p_plus_1[4] = { 0x43e1f593f0000002, 0x2833e84879b97091, 0xb85045b68181585d, 0x30644e72e131a029 };

// adds `p+1` to the input, inplace
uint8_t bn128_r_mont_bigint256_add_prime_plus_1_inplace( uint64_t *tgt ) {
  return bigint256_add_inplace( tgt, bn128_r_mont_p_plus_1);
}

// subtracts the prime p from a bigint, inplace
uint8_t bn128_r_mont_bigint256_sub_prime_inplace( uint64_t *tgt ) {
  return bigint256_sub_inplace( tgt, bn128_r_mont_prime);
}


// negates a field element
void bn128_r_mont_neg( const uint64_t *src, uint64_t *tgt ) {
  if (bigint256_is_zero(src)) {
    bigint256_set_zero(tgt);
  }
  else {
    // mod (-x) p = p - x
    tgt[0] = 0x43e1f593f0000001 ;
    tgt[1] = 0x2833e84879b97091 ;
    tgt[2] = 0xb85045b68181585d ;
    tgt[3] = 0x30644e72e131a029 ;
    bigint256_sub_inplace(tgt, src);
  }
}

// negates a field element
void bn128_r_mont_neg_inplace( uint64_t *tgt ) {
  if (bigint256_is_zero(tgt)) {
    return;
  }
  else {
    for(int i=0; i<4; i++) tgt[i] = ~tgt[i];
    bn128_r_mont_bigint256_add_prime_plus_1_inplace(tgt);
  }
}

// if (x > prime) then (x - prime) else x
void bn128_r_mont_bigint256_sub_prime_if_above_inplace( uint64_t *tgt ) {
  if (tgt[3] <  0x30644e72e131a029) return;
  if (tgt[3] >  0x30644e72e131a029) { bn128_r_mont_bigint256_sub_prime_inplace( tgt ); return; }
  if (tgt[2] <  0xb85045b68181585d) return;
  if (tgt[2] >  0xb85045b68181585d) { bn128_r_mont_bigint256_sub_prime_inplace( tgt ); return; }
  if (tgt[1] <  0x2833e84879b97091) return;
  if (tgt[1] >  0x2833e84879b97091) { bn128_r_mont_bigint256_sub_prime_inplace( tgt ); return; }
  if (tgt[0] <  0x43e1f593f0000001) return;
  if (tgt[0] >= 0x43e1f593f0000001) { bn128_r_mont_bigint256_sub_prime_inplace( tgt ); return; }
}

// adds two field elements
void bn128_r_mont_add( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  uint8_t c = 0;
  c = bigint256_add( src1, src2, tgt );
  bn128_r_mont_bigint256_sub_prime_if_above_inplace( tgt );
}

// adds two field elements, inplace
void bn128_r_mont_add_inplace( uint64_t *tgt, const uint64_t *src2 ) {
  uint8_t c = 0;
  c = bigint256_add_inplace( tgt, src2 );
  bn128_r_mont_bigint256_sub_prime_if_above_inplace( tgt );
}

// subtracts two field elements
void bn128_r_mont_sub( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  uint8_t b = 0;
  b = bigint256_sub( src1, src2, tgt );
  if (b) { bn128_r_mont_bigint256_add_prime_inplace( tgt ); }
}

// subtracts two field elements
void bn128_r_mont_sub_inplace( uint64_t *tgt, const uint64_t *src2 ) {
  uint8_t b = 0;
  b = bigint256_sub_inplace( tgt, src2 );
  if (b) { bn128_r_mont_bigint256_add_prime_inplace( tgt ); }
}

// tgt := src - tgt
void bn128_r_mont_sub_inplace_reverse( uint64_t *tgt, const uint64_t *src1 ) {
  uint8_t b = 0;
  b = bigint256_sub_inplace_reverse( tgt, src1 );
  if (b) { bn128_r_mont_bigint256_add_prime_inplace( tgt ); }
}

// Montgomery constants R, R^2, R^3 mod P
const uint64_t bn128_r_mont_R_modp[4] = { 0xac96341c4ffffffb, 0x36fc76959f60cd29, 0x666ea36f7879462e, 0x0e0a77c19a07df2f };
const uint64_t bn128_r_mont_R_squared[4] = { 0x1bb8e645ae216da7, 0x53fe3ab1e35c59e3, 0x8c49833d53bb8085, 0x0216d0b17f4e44a5 };
const uint64_t bn128_r_mont_R_cubed[4] = { 0x5e94d8e1b4bf0040, 0x2a489cbe1cfbb6b8, 0x893cc664a19fcfed, 0x0cf8594b7fcc657c };

// Montgomery reduction REDC algorithm
// based on <https://en.wikipedia.org/wiki/Montgomery_modular_multiplication>
// T is 9 sized bigint in Montgomery representation,
// and assumed to be < 2^256*p
// WARNING: the value in T which will be overwritten!
//
void bn128_r_mont_REDC_unsafe( uint64_t *T, uint64_t *tgt ) {
  T[8] = 0;
  for(int i=0; i<4; i++) {
    __uint128_t x;
    uint64_t c;
    uint64_t m = T[i] * 0xc2e1f593efffffff;
    // j = 0
    x = ((__uint128_t)m) * bn128_r_mont_prime[0] + T[i+0];    // note: cannot overflow in 128 bits
    c = x >> 64;
    T[i+0] = (uint64_t) x;
    // j = 1
    x = ((__uint128_t)m) * bn128_r_mont_prime[1] + T[i+1] + c;    // note: cannot overflow in 128 bits
    c = x >> 64;
    T[i+1] = (uint64_t) x;
    // j = 2
    x = ((__uint128_t)m) * bn128_r_mont_prime[2] + T[i+2] + c;    // note: cannot overflow in 128 bits
    c = x >> 64;
    T[i+2] = (uint64_t) x;
    // j = 3
    x = ((__uint128_t)m) * bn128_r_mont_prime[3] + T[i+3] + c;    // note: cannot overflow in 128 bits
    c = x >> 64;
    T[i+3] = (uint64_t) x;
    uint8_t d = _addcarry_u64( 0 , T[i+4] , c , T+i+4 );
    for(int j=5; (d>0) && (j<=8-i); j++) {
      d = _addcarry_u64( d , T[i+j] , 0 , T+i+j );
    }
  }
  memcpy( tgt, T+4, 32);
  bn128_r_mont_bigint256_sub_prime_if_above_inplace(tgt);
}

void bn128_r_mont_REDC( const uint64_t *src, uint64_t *tgt ) {
  uint64_t T[9];
  memcpy( T, src, 64 );
  bn128_r_mont_REDC_unsafe ( T, tgt );
}

void bn128_r_mont_sqr( const uint64_t *src, uint64_t *tgt) {
  uint64_t T[9];
  bigint256_sqr( src, T );
  bn128_r_mont_REDC_unsafe( T, tgt );
};

void bn128_r_mont_sqr_inplace( uint64_t *tgt ) {
  uint64_t T[9];
  bigint256_sqr( tgt, T );
  bn128_r_mont_REDC_unsafe( T, tgt );
};

void bn128_r_mont_mul( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt) {
  uint64_t T[9];
  bigint256_mul( src1, src2, T );
  bn128_r_mont_REDC_unsafe( T, tgt );
};

void bn128_r_mont_mul_inplace( uint64_t *tgt, const uint64_t *src2) {
  uint64_t T[9];
  bigint256_mul( tgt, src2, T );
  bn128_r_mont_REDC_unsafe( T, tgt );
};

// computes `x^e mod p`
void bn128_r_mont_pow_uint64( const uint64_t *src, uint64_t exponent, uint64_t *tgt ) {
  uint64_t e = exponent;
  uint64_t sqr[4];
  bigint256_copy( src, sqr );             // sqr := src
  bigint256_copy( bn128_r_mont_R_modp, tgt );          // tgt := 1
  while(e!=0) {
    if (e & 1) { bn128_r_mont_mul_inplace(tgt, sqr); }
    bn128_r_mont_mul_inplace(sqr, sqr);
    e = e >> 1;
  }
}

// computes `x^e mod p` (for `e` non-negative bigint)
void bn128_r_mont_pow_gen( const uint64_t *src, const uint64_t *expo, uint64_t *tgt, int expo_len ) {
  uint64_t sqr[4];
  bigint256_copy( src, sqr );             // sqr := src
  bigint256_copy( bn128_r_mont_R_modp, tgt );        // tgt := 1
  int s = expo_len - 1;
  while ((expo[s] == 0) && (s>0)) { s--; }          // skip the unneeded largest powers
  for(int i=0; i<=s; i++) {
    uint64_t e = expo[i];
    for(int j=0; j<64; j++) {
      if (e & 1) { bn128_r_mont_mul_inplace(tgt, sqr); }
      bn128_r_mont_mul_inplace(sqr, sqr);
      e = e >> 1;
    }
  }
}

void bn128_r_mont_inv( const uint64_t *src, uint64_t *tgt) {
  bn128_r_std_inv( src, tgt );
  bn128_r_mont_mul_inplace( tgt, bn128_r_mont_R_cubed );
};

void bn128_r_mont_inv_inplace( uint64_t *tgt ) {
  bn128_r_std_inv_inplace( tgt );
  bn128_r_mont_mul_inplace( tgt, bn128_r_mont_R_cubed );
};

void bn128_r_mont_div( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt) {
  bn128_r_std_div( src1, src2, tgt );
  bn128_r_mont_mul_inplace( tgt, bn128_r_mont_R_squared );
};

void bn128_r_mont_div_inplace( uint64_t *tgt, const uint64_t *src2) {
  bn128_r_std_div_inplace( tgt, src2 );
  bn128_r_mont_mul_inplace( tgt, bn128_r_mont_R_squared );
};

uint8_t bn128_r_mont_is_one( const uint64_t *src ) {
  bigint256_is_equal( src, bn128_r_mont_R_modp );
}

void bn128_r_mont_from_std( const uint64_t *src, uint64_t *tgt) {
  bn128_r_mont_mul( src, bn128_r_mont_R_squared, tgt );
}

void bn128_r_mont_to_std( const uint64_t *src, uint64_t *tgt) {
  uint64_t T[9];
  memcpy( T, src, 32);
  memset( T+4, 0, 32);
  bn128_r_mont_REDC_unsafe( T, tgt );
};
