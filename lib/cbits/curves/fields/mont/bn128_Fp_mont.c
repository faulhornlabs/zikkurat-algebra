
// finite field arithmetic in Montgomery representation, in the prime field with 
//
//   p = 21888242871839275222246405745257275088696311157297823662689037894645226208583
//
// NOTE: generated code, do not edit!

#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>

#include "bn128_Fp_mont.h"
#include "bn128_Fp_std.h"
#include "bigint256.h"
#include "platform.h"

#define NLIMBS 4

const uint64_t bn128_Fp_mont_prime[4] = { 0x3c208c16d87cfd47, 0x97816a916871ca8d, 0xb85045b68181585d, 0x30644e72e131a029 };

//------------------------------------------------------------------------------

// adds the prime p to a bigint, inplace
uint8_t bn128_Fp_mont_bigint256_add_prime_inplace( uint64_t *tgt ) {
  return bigint256_add_inplace( tgt, bn128_Fp_mont_prime);
}

// the constant `p + 1`
const uint64_t bn128_Fp_mont_p_plus_1[4] = { 0x3c208c16d87cfd48, 0x97816a916871ca8d, 0xb85045b68181585d, 0x30644e72e131a029 };

// adds `p+1` to the input, inplace
uint8_t bn128_Fp_mont_bigint256_add_prime_plus_1_inplace( uint64_t *tgt ) {
  return bigint256_add_inplace( tgt, bn128_Fp_mont_p_plus_1);
}

// subtracts the prime p from a bigint, inplace
uint8_t bn128_Fp_mont_bigint256_sub_prime_inplace( uint64_t *tgt ) {
  return bigint256_sub_inplace( tgt, bn128_Fp_mont_prime);
}


// negates a field element
void bn128_Fp_mont_neg( const uint64_t *src, uint64_t *tgt ) {
  if (bigint256_is_zero(src)) {
    bigint256_set_zero(tgt);
  }
  else {
    // mod (-x) p = p - x
    uint64_t tmp[NLIMBS];
    memcpy(tmp, src, 8*NLIMBS);   // if tgt==src, it would overwrite `src` below...
    tgt[0] = 0x3c208c16d87cfd47 ;
    tgt[1] = 0x97816a916871ca8d ;
    tgt[2] = 0xb85045b68181585d ;
    tgt[3] = 0x30644e72e131a029 ;
    bigint256_sub_inplace(tgt, tmp);  // src);
  }
}

// negates a field element
void bn128_Fp_mont_neg_inplace( uint64_t *tgt ) {
  if (bigint256_is_zero(tgt)) {
    return;
  }
  else {
    for(int i=0; i<4; i++) tgt[i] = ~tgt[i];
    bn128_Fp_mont_bigint256_add_prime_plus_1_inplace(tgt);
  }
}

// if (x > prime) then (x - prime) else x
void bn128_Fp_mont_bigint256_sub_prime_if_above_inplace( uint64_t *tgt ) {
  if (tgt[3] <  0x30644e72e131a029) return;
  if (tgt[3] >  0x30644e72e131a029) { bn128_Fp_mont_bigint256_sub_prime_inplace( tgt ); return; }
  if (tgt[2] <  0xb85045b68181585d) return;
  if (tgt[2] >  0xb85045b68181585d) { bn128_Fp_mont_bigint256_sub_prime_inplace( tgt ); return; }
  if (tgt[1] <  0x97816a916871ca8d) return;
  if (tgt[1] >  0x97816a916871ca8d) { bn128_Fp_mont_bigint256_sub_prime_inplace( tgt ); return; }
  if (tgt[0] <  0x3c208c16d87cfd47) return;
  if (tgt[0] >= 0x3c208c16d87cfd47) { bn128_Fp_mont_bigint256_sub_prime_inplace( tgt ); return; }
}

// adds two field elements
void bn128_Fp_mont_add( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  uint8_t c = 0;
  c = bigint256_add( src1, src2, tgt );
  bn128_Fp_mont_bigint256_sub_prime_if_above_inplace( tgt );
}

// adds two field elements, inplace
void bn128_Fp_mont_add_inplace( uint64_t *tgt, const uint64_t *src2 ) {
  uint8_t c = 0;
  c = bigint256_add_inplace( tgt, src2 );
  bn128_Fp_mont_bigint256_sub_prime_if_above_inplace( tgt );
}

// subtracts two field elements
void bn128_Fp_mont_sub( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  uint8_t b = 0;
  b = bigint256_sub( src1, src2, tgt );
  if (b) { bn128_Fp_mont_bigint256_add_prime_inplace( tgt ); }
}

// subtracts two field elements
void bn128_Fp_mont_sub_inplace( uint64_t *tgt, const uint64_t *src2 ) {
  uint8_t b = 0;
  b = bigint256_sub_inplace( tgt, src2 );
  if (b) { bn128_Fp_mont_bigint256_add_prime_inplace( tgt ); }
}

// tgt := src - tgt
void bn128_Fp_mont_sub_inplace_reverse( uint64_t *tgt, const uint64_t *src1 ) {
  uint8_t b = 0;
  b = bigint256_sub_inplace_reverse( tgt, src1 );
  if (b) { bn128_Fp_mont_bigint256_add_prime_inplace( tgt ); }
}

// Montgomery constants R, R^2, R^3 mod P
const uint64_t bn128_Fp_mont_R_modp[4] = { 0xd35d438dc58f0d9d, 0x0a78eb28f5c70b3d, 0x666ea36f7879462c, 0x0e0a77c19a07df2f };
const uint64_t bn128_Fp_mont_R_squared[4] = { 0xf32cfc5b538afa89, 0xb5e71911d44501fb, 0x47ab1eff0a417ff6, 0x06d89f71cab8351f };
const uint64_t bn128_Fp_mont_R_cubed[4] = { 0xb1cd6dafda1530df, 0x62f210e6a7283db6, 0xef7f0b0c0ada0afb, 0x20fd6e902d592544 };

// Montgomery reduction REDC algorithm
// based on <https://en.wikipedia.org/wiki/Montgomery_modular_multiplication>
// T is 9 sized bigint in Montgomery representation,
// and assumed to be < 2^256*p
// WARNING: the value in T which will be overwritten!
//
void bn128_Fp_mont_REDC_unsafe( uint64_t *T, uint64_t *tgt ) {
  T[8] = 0;
  for(int i=0; i<4; i++) {
    __uint128_t x;
    uint64_t c;
    uint64_t m = T[i] * 0x87d20782e4866389;
    // j = 0
    x = ((__uint128_t)m) * bn128_Fp_mont_prime[0] + T[i+0];    // note: cannot overflow in 128 bits
    c = x >> 64;
    T[i+0] = (uint64_t) x;
    // j = 1
    x = ((__uint128_t)m) * bn128_Fp_mont_prime[1] + T[i+1] + c;    // note: cannot overflow in 128 bits
    c = x >> 64;
    T[i+1] = (uint64_t) x;
    // j = 2
    x = ((__uint128_t)m) * bn128_Fp_mont_prime[2] + T[i+2] + c;    // note: cannot overflow in 128 bits
    c = x >> 64;
    T[i+2] = (uint64_t) x;
    // j = 3
    x = ((__uint128_t)m) * bn128_Fp_mont_prime[3] + T[i+3] + c;    // note: cannot overflow in 128 bits
    c = x >> 64;
    T[i+3] = (uint64_t) x;
    uint8_t d = addcarry_u64( 0 , T[i+4] , c , T+i+4 );
    for(int j=5; (d>0) && (j<=8-i); j++) {
      d = addcarry_u64( d , T[i+j] , 0 , T+i+j );
    }
  }
  memcpy( tgt, T+4, 32);
  bn128_Fp_mont_bigint256_sub_prime_if_above_inplace(tgt);
}

void bn128_Fp_mont_REDC( const uint64_t *src, uint64_t *tgt ) {
  uint64_t T[9];
  memcpy( T, src, 64 );
  bn128_Fp_mont_REDC_unsafe ( T, tgt );
}

void bn128_Fp_mont_sqr( const uint64_t *src, uint64_t *tgt) {
  uint64_t T[9];
  bigint256_sqr( src, T );
  bn128_Fp_mont_REDC_unsafe( T, tgt );
};

void bn128_Fp_mont_sqr_inplace( uint64_t *tgt ) {
  uint64_t T[9];
  bigint256_sqr( tgt, T );
  bn128_Fp_mont_REDC_unsafe( T, tgt );
};

void bn128_Fp_mont_mul( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt) {
  uint64_t T[9];
  bigint256_mul( src1, src2, T );
  bn128_Fp_mont_REDC_unsafe( T, tgt );
};

void bn128_Fp_mont_mul_inplace( uint64_t *tgt, const uint64_t *src2) {
  uint64_t T[9];
  bigint256_mul( tgt, src2, T );
  bn128_Fp_mont_REDC_unsafe( T, tgt );
};

void bn128_Fp_mont_inv( const uint64_t *src, uint64_t *tgt) {
  bn128_Fp_std_inv( src, tgt );
  bn128_Fp_mont_mul_inplace( tgt, bn128_Fp_mont_R_cubed );
};

void bn128_Fp_mont_inv_inplace( uint64_t *tgt ) {
  bn128_Fp_std_inv_inplace( tgt );
  bn128_Fp_mont_mul_inplace( tgt, bn128_Fp_mont_R_cubed );
};

void bn128_Fp_mont_div( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt) {
  bn128_Fp_std_div( src1, src2, tgt );
  bn128_Fp_mont_mul_inplace( tgt, bn128_Fp_mont_R_squared );
};

void bn128_Fp_mont_div_inplace( uint64_t *tgt, const uint64_t *src2) {
  bn128_Fp_std_div_inplace( tgt, src2 );
  bn128_Fp_mont_mul_inplace( tgt, bn128_Fp_mont_R_squared );
};

// computes `x^e mod p`
void bn128_Fp_mont_pow_uint64( const uint64_t *src, uint64_t exponent, uint64_t *tgt ) {
  uint64_t e = exponent;
  uint64_t sqr[4];
  bn128_Fp_mont_copy( src, sqr );                 // sqr := src
  bn128_Fp_mont_set_one( tgt );                   // tgt := 1
  while(e!=0) {
    if (e & 1) { bn128_Fp_mont_mul_inplace(tgt, sqr); }
    bn128_Fp_mont_mul_inplace(sqr, sqr);
    e = e >> 1;
  }
}

// computes `x^e mod p` (for `e` non-negative bigint)
void bn128_Fp_mont_pow_gen( const uint64_t *src, const uint64_t *expo, uint64_t *tgt, int expo_len ) {
  uint64_t sqr[4];
  bn128_Fp_mont_copy( src, sqr );                 // sqr := src
  bn128_Fp_mont_set_one( tgt );                   // tgt := 1
  int s = expo_len - 1;
  while ((expo[s] == 0) && (s>0)) { s--; }          // skip the unneeded largest powers
  for(int i=0; i<=s; i++) {
    uint64_t e = expo[i];
    for(int j=0; j<64; j++) {
      if (e & 1) { bn128_Fp_mont_mul_inplace(tgt, sqr); }
      bn128_Fp_mont_mul_inplace(sqr, sqr);
      e = e >> 1;
    }
  }
}

#define I_SRC(i)   (src    + (i)*4)
#define I_TGT(i)   (tgt    + (i)*4)
#define I_PROD(i)  (prods  + (i)*4)
#define I_RECIP(i) (recips + (i)*4)

// computes the inverse of many field elements at the same time, efficiently
// uses the Montgomery batch inversion trick
// inverse of a field element
void bn128_Fp_mont_batch_inv( int n, const uint64_t *src, uint64_t *tgt ) {
  assert( n >= 1 );
  uint64_t *prods  = malloc( 8*4*n );
  uint64_t *recips = malloc( 8*4*n );
  assert( prods  != 0 );
  assert( recips != 0 );
  
  // compute partial products (a[0]*a[1]*...*a[k]) for all k
  bn128_Fp_mont_copy( I_SRC(0) , I_PROD(0) );
  for(int i=1; i<n; i++) {
    bn128_Fp_mont_mul( I_PROD(i-1) , I_SRC(i) , I_PROD(i) );
  }
  
  // compute inverses of partial products 1/(a[0]*a[1]*...*a[k]) for all k
  bn128_Fp_mont_inv( I_PROD(n-1) , I_RECIP(n-1) );
  for(int i=n-2; i>=0; i--) {
    bn128_Fp_mont_mul( I_RECIP(i+1) , I_SRC(i+1) , I_RECIP(i) );
  }
  
  // compute the inverses 1/a[k] for all k
  bn128_Fp_mont_copy( I_RECIP(0) , I_TGT(0) );
  for(int i=1; i<n; i++) {
    bn128_Fp_mont_mul( I_RECIP(i) , I_PROD(i-1) , I_TGT(i) );
  }
  
  free(recips);
  free(prods);
}

// checks if (x < prime)
uint8_t bn128_Fp_mont_is_valid( const uint64_t *src ) {
  if (src[3] <  0x30644e72e131a029) return 1;
  if (src[3] >  0x30644e72e131a029) return 0;
  if (src[2] <  0xb85045b68181585d) return 1;
  if (src[2] >  0xb85045b68181585d) return 0;
  if (src[1] <  0x97816a916871ca8d) return 1;
  if (src[1] >  0x97816a916871ca8d) return 0;
  if (src[0] <  0x3c208c16d87cfd47) return 1;
  if (src[0] >= 0x3c208c16d87cfd47) return 0;
return 1;
}

uint8_t bn128_Fp_mont_is_zero( const uint64_t *src ) {
  return bigint256_is_zero( src );
}

uint8_t bn128_Fp_mont_is_one( const uint64_t *src ) {
  return bigint256_is_equal( src, bn128_Fp_mont_R_modp );
}

uint8_t bn128_Fp_mont_is_equal( const uint64_t *src1, const uint64_t *src2 ) {
  return bigint256_is_equal( src1 , src2 );
}

void bn128_Fp_mont_set_zero( uint64_t *tgt ) {
  bigint256_set_zero( tgt );
}

void bn128_Fp_mont_set_one( uint64_t *tgt) {
  bigint256_copy( bn128_Fp_mont_R_modp , tgt );
}

void bn128_Fp_mont_copy( const uint64_t *src, uint64_t *tgt ) {
  bigint256_copy( src , tgt );
}

void bn128_Fp_mont_from_std( const uint64_t *src, uint64_t *tgt) {
  bn128_Fp_mont_mul( src, bn128_Fp_mont_R_squared, tgt );
}

void bn128_Fp_mont_to_std( const uint64_t *src, uint64_t *tgt) {
  uint64_t T[9];
  memcpy( T, src, 32);
  memset( T+4, 0, 32);
  bn128_Fp_mont_REDC_unsafe( T, tgt );
};
