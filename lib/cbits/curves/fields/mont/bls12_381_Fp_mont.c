
// finite field arithmetic in Montgomery representation, in the prime field with 
//
//   p = 4002409555221667393417789825735904156556882819939007885332058136124031650490837864442687629129015664037894272559787
//
// NOTE: generated code, do not edit!

#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>

#include "bls12_381_Fp_mont.h"
#include "bls12_381_Fp_std.h"
#include "bigint384.h"
#include "platform.h"

#define NLIMBS 6

const uint64_t bls12_381_Fp_mont_prime[6] = { 0xb9feffffffffaaab, 0x1eabfffeb153ffff, 0x6730d2a0f6b0f624, 0x64774b84f38512bf, 0x4b1ba7b6434bacd7, 0x1a0111ea397fe69a };

//------------------------------------------------------------------------------

// adds the prime p to a bigint, inplace
uint8_t bls12_381_Fp_mont_bigint384_add_prime_inplace( uint64_t *tgt ) {
  return bigint384_add_inplace( tgt, bls12_381_Fp_mont_prime);
}

// the constant `p + 1`
const uint64_t bls12_381_Fp_mont_p_plus_1[6] = { 0xb9feffffffffaaac, 0x1eabfffeb153ffff, 0x6730d2a0f6b0f624, 0x64774b84f38512bf, 0x4b1ba7b6434bacd7, 0x1a0111ea397fe69a };

// adds `p+1` to the input, inplace
uint8_t bls12_381_Fp_mont_bigint384_add_prime_plus_1_inplace( uint64_t *tgt ) {
  return bigint384_add_inplace( tgt, bls12_381_Fp_mont_p_plus_1);
}

// subtracts the prime p from a bigint, inplace
uint8_t bls12_381_Fp_mont_bigint384_sub_prime_inplace( uint64_t *tgt ) {
  return bigint384_sub_inplace( tgt, bls12_381_Fp_mont_prime);
}


// negates a field element
void bls12_381_Fp_mont_neg( const uint64_t *src, uint64_t *tgt ) {
  if (bigint384_is_zero(src)) {
    bigint384_set_zero(tgt);
  }
  else {
    // mod (-x) p = p - x
    uint64_t tmp[NLIMBS];
    memcpy(tmp, src, 8*NLIMBS);   // if tgt==src, it would overwrite `src` below...
    tgt[0] = 0xb9feffffffffaaab ;
    tgt[1] = 0x1eabfffeb153ffff ;
    tgt[2] = 0x6730d2a0f6b0f624 ;
    tgt[3] = 0x64774b84f38512bf ;
    tgt[4] = 0x4b1ba7b6434bacd7 ;
    tgt[5] = 0x1a0111ea397fe69a ;
    bigint384_sub_inplace(tgt, tmp);  // src);
  }
}

// negates a field element
void bls12_381_Fp_mont_neg_inplace( uint64_t *tgt ) {
  if (bigint384_is_zero(tgt)) {
    return;
  }
  else {
    for(int i=0; i<6; i++) tgt[i] = ~tgt[i];
    bls12_381_Fp_mont_bigint384_add_prime_plus_1_inplace(tgt);
  }
}

// if (x > prime) then (x - prime) else x
void bls12_381_Fp_mont_bigint384_sub_prime_if_above_inplace( uint64_t *tgt ) {
  if (tgt[5] <  0x1a0111ea397fe69a) return;
  if (tgt[5] >  0x1a0111ea397fe69a) { bls12_381_Fp_mont_bigint384_sub_prime_inplace( tgt ); return; }
  if (tgt[4] <  0x4b1ba7b6434bacd7) return;
  if (tgt[4] >  0x4b1ba7b6434bacd7) { bls12_381_Fp_mont_bigint384_sub_prime_inplace( tgt ); return; }
  if (tgt[3] <  0x64774b84f38512bf) return;
  if (tgt[3] >  0x64774b84f38512bf) { bls12_381_Fp_mont_bigint384_sub_prime_inplace( tgt ); return; }
  if (tgt[2] <  0x6730d2a0f6b0f624) return;
  if (tgt[2] >  0x6730d2a0f6b0f624) { bls12_381_Fp_mont_bigint384_sub_prime_inplace( tgt ); return; }
  if (tgt[1] <  0x1eabfffeb153ffff) return;
  if (tgt[1] >  0x1eabfffeb153ffff) { bls12_381_Fp_mont_bigint384_sub_prime_inplace( tgt ); return; }
  if (tgt[0] <  0xb9feffffffffaaab) return;
  if (tgt[0] >= 0xb9feffffffffaaab) { bls12_381_Fp_mont_bigint384_sub_prime_inplace( tgt ); return; }
}

// adds two field elements
void bls12_381_Fp_mont_add( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  uint8_t c = 0;
  c = bigint384_add( src1, src2, tgt );
  bls12_381_Fp_mont_bigint384_sub_prime_if_above_inplace( tgt );
}

// adds two field elements, inplace
void bls12_381_Fp_mont_add_inplace( uint64_t *tgt, const uint64_t *src2 ) {
  uint8_t c = 0;
  c = bigint384_add_inplace( tgt, src2 );
  bls12_381_Fp_mont_bigint384_sub_prime_if_above_inplace( tgt );
}

// subtracts two field elements
void bls12_381_Fp_mont_sub( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  uint8_t b = 0;
  b = bigint384_sub( src1, src2, tgt );
  if (b) { bls12_381_Fp_mont_bigint384_add_prime_inplace( tgt ); }
}

// subtracts two field elements
void bls12_381_Fp_mont_sub_inplace( uint64_t *tgt, const uint64_t *src2 ) {
  uint8_t b = 0;
  b = bigint384_sub_inplace( tgt, src2 );
  if (b) { bls12_381_Fp_mont_bigint384_add_prime_inplace( tgt ); }
}

// tgt := src - tgt
void bls12_381_Fp_mont_sub_inplace_reverse( uint64_t *tgt, const uint64_t *src1 ) {
  uint8_t b = 0;
  b = bigint384_sub_inplace_reverse( tgt, src1 );
  if (b) { bls12_381_Fp_mont_bigint384_add_prime_inplace( tgt ); }
}

// divides by 2
void bls12_381_Fp_mont_div_by_2( const uint64_t *src, uint64_t *tgt ) {
  return bls12_381_Fp_std_div_by_2(src,tgt);
}

// divides by 2, inplace
void bls12_381_Fp_mont_div_by_2_inplace( uint64_t *tgt ) {
  return bls12_381_Fp_std_div_by_2_inplace(tgt);
}


// Montgomery constants R, R^2, R^3 mod P
const uint64_t bls12_381_Fp_mont_R_modp[6] = { 0x760900000002fffd, 0xebf4000bc40c0002, 0x5f48985753c758ba, 0x77ce585370525745, 0x5c071a97a256ec6d, 0x15f65ec3fa80e493 };
const uint64_t bls12_381_Fp_mont_R_squared[6] = { 0xf4df1f341c341746, 0x0a76e6a609d104f1, 0x8de5476c4c95b6d5, 0x67eb88a9939d83c0, 0x9a793e85b519952d, 0x11988fe592cae3aa };
const uint64_t bls12_381_Fp_mont_R_cubed[6] = { 0xed48ac6bd94ca1e0, 0x315f831e03a7adf8, 0x9a53352a615e29dd, 0x34c04e5e921e1761, 0x2512d43565724728, 0x0aa6346091755d4d };

// Montgomery reduction REDC algorithm
// based on <https://en.wikipedia.org/wiki/Montgomery_modular_multiplication>
// T is 13 sized bigint in Montgomery representation,
// and assumed to be < 2^384*p
// WARNING: the value in T which will be overwritten!
//
void bls12_381_Fp_mont_REDC_unsafe( uint64_t *T, uint64_t *tgt ) {
  T[12] = 0;
  for(int i=0; i<6; i++) {
    __uint128_t x;
    uint64_t c;
    uint64_t m = T[i] * 0x89f3fffcfffcfffd;
    // j = 0
    x = ((__uint128_t)m) * bls12_381_Fp_mont_prime[0] + T[i+0];    // note: cannot overflow in 128 bits
    c = x >> 64;
    T[i+0] = (uint64_t) x;
    // j = 1
    x = ((__uint128_t)m) * bls12_381_Fp_mont_prime[1] + T[i+1] + c;    // note: cannot overflow in 128 bits
    c = x >> 64;
    T[i+1] = (uint64_t) x;
    // j = 2
    x = ((__uint128_t)m) * bls12_381_Fp_mont_prime[2] + T[i+2] + c;    // note: cannot overflow in 128 bits
    c = x >> 64;
    T[i+2] = (uint64_t) x;
    // j = 3
    x = ((__uint128_t)m) * bls12_381_Fp_mont_prime[3] + T[i+3] + c;    // note: cannot overflow in 128 bits
    c = x >> 64;
    T[i+3] = (uint64_t) x;
    // j = 4
    x = ((__uint128_t)m) * bls12_381_Fp_mont_prime[4] + T[i+4] + c;    // note: cannot overflow in 128 bits
    c = x >> 64;
    T[i+4] = (uint64_t) x;
    // j = 5
    x = ((__uint128_t)m) * bls12_381_Fp_mont_prime[5] + T[i+5] + c;    // note: cannot overflow in 128 bits
    c = x >> 64;
    T[i+5] = (uint64_t) x;
    uint8_t d = addcarry_u64( 0 , T[i+6] , c , T+i+6 );
    for(int j=7; (d>0) && (j<=12-i); j++) {
      d = addcarry_u64( d , T[i+j] , 0 , T+i+j );
    }
  }
  memcpy( tgt, T+6, 48);
  bls12_381_Fp_mont_bigint384_sub_prime_if_above_inplace(tgt);
}

void bls12_381_Fp_mont_REDC( const uint64_t *src, uint64_t *tgt ) {
  uint64_t T[13];
  memcpy( T, src, 96 );
  bls12_381_Fp_mont_REDC_unsafe ( T, tgt );
}

void bls12_381_Fp_mont_sqr( const uint64_t *src, uint64_t *tgt) {
  uint64_t T[13];
  bigint384_sqr( src, T );
  bls12_381_Fp_mont_REDC_unsafe( T, tgt );
};

void bls12_381_Fp_mont_sqr_inplace( uint64_t *tgt ) {
  uint64_t T[13];
  bigint384_sqr( tgt, T );
  bls12_381_Fp_mont_REDC_unsafe( T, tgt );
};

void bls12_381_Fp_mont_mul( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt) {
  uint64_t T[13];
  bigint384_mul( src1, src2, T );
  bls12_381_Fp_mont_REDC_unsafe( T, tgt );
};

void bls12_381_Fp_mont_mul_inplace( uint64_t *tgt, const uint64_t *src2) {
  uint64_t T[13];
  bigint384_mul( tgt, src2, T );
  bls12_381_Fp_mont_REDC_unsafe( T, tgt );
};

void bls12_381_Fp_mont_inv( const uint64_t *src, uint64_t *tgt) {
  bls12_381_Fp_std_inv( src, tgt );
  bls12_381_Fp_mont_mul_inplace( tgt, bls12_381_Fp_mont_R_cubed );
};

void bls12_381_Fp_mont_inv_inplace( uint64_t *tgt ) {
  bls12_381_Fp_std_inv_inplace( tgt );
  bls12_381_Fp_mont_mul_inplace( tgt, bls12_381_Fp_mont_R_cubed );
};

void bls12_381_Fp_mont_div( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt) {
  bls12_381_Fp_std_div( src1, src2, tgt );
  bls12_381_Fp_mont_mul_inplace( tgt, bls12_381_Fp_mont_R_squared );
};

void bls12_381_Fp_mont_div_inplace( uint64_t *tgt, const uint64_t *src2) {
  bls12_381_Fp_std_div_inplace( tgt, src2 );
  bls12_381_Fp_mont_mul_inplace( tgt, bls12_381_Fp_mont_R_squared );
};

// computes `x^e mod p`
void bls12_381_Fp_mont_pow_uint64( const uint64_t *src, uint64_t exponent, uint64_t *tgt ) {
  uint64_t e = exponent;
  uint64_t sqr[6];
  bls12_381_Fp_mont_copy( src, sqr );                 // sqr := src
  bls12_381_Fp_mont_set_one( tgt );                   // tgt := 1
  while(e!=0) {
    if (e & 1) { bls12_381_Fp_mont_mul_inplace(tgt, sqr); }
    bls12_381_Fp_mont_mul_inplace(sqr, sqr);
    e = e >> 1;
  }
}

// computes `x^e mod p` (for `e` non-negative bigint)
void bls12_381_Fp_mont_pow_gen( const uint64_t *src, const uint64_t *expo, uint64_t *tgt, int expo_len ) {
  uint64_t sqr[6];
  bls12_381_Fp_mont_copy( src, sqr );                 // sqr := src
  bls12_381_Fp_mont_set_one( tgt );                   // tgt := 1
  int s = expo_len - 1;
  while ((expo[s] == 0) && (s>0)) { s--; }          // skip the unneeded largest powers
  for(int i=0; i<=s; i++) {
    uint64_t e = expo[i];
    for(int j=0; j<64; j++) {
      if (e & 1) { bls12_381_Fp_mont_mul_inplace(tgt, sqr); }
      bls12_381_Fp_mont_mul_inplace(sqr, sqr);
      e = e >> 1;
    }
  }
}

#define I_SRC(i)   (src    + (i)*6)
#define I_TGT(i)   (tgt    + (i)*6)
#define I_PROD(i)  (prods  + (i)*6)
#define I_RECIP(i) (recips + (i)*6)

// computes the inverse of many field elements at the same time, efficiently
// uses the Montgomery batch inversion trick
void bls12_381_Fp_mont_batch_inv( int n, const uint64_t *src, uint64_t *tgt ) {
  assert( n >= 1 );
  uint64_t *prods  = malloc( 8*6*n );
  uint64_t *recips = malloc( 8*6*n );
  assert( prods  != 0 );
  assert( recips != 0 );
  
  // compute partial products (a[0]*a[1]*...*a[k]) for all k
  bls12_381_Fp_mont_copy( I_SRC(0) , I_PROD(0) );
  for(int i=1; i<n; i++) {
    bls12_381_Fp_mont_mul( I_PROD(i-1) , I_SRC(i) , I_PROD(i) );
  }
  
  // compute inverses of partial products 1/(a[0]*a[1]*...*a[k]) for all k
  bls12_381_Fp_mont_inv( I_PROD(n-1) , I_RECIP(n-1) );
  for(int i=n-2; i>=0; i--) {
    bls12_381_Fp_mont_mul( I_RECIP(i+1) , I_SRC(i+1) , I_RECIP(i) );
  }
  
  // compute the inverses 1/a[k] for all k
  bls12_381_Fp_mont_copy( I_RECIP(0) , I_TGT(0) );
  for(int i=1; i<n; i++) {
    bls12_381_Fp_mont_mul( I_RECIP(i) , I_PROD(i-1) , I_TGT(i) );
  }
  
  free(recips);
  free(prods);
}

// checks if (x < prime)
uint8_t bls12_381_Fp_mont_is_valid( const uint64_t *src ) {
  if (src[5] <  0x1a0111ea397fe69a) return 1;
  if (src[5] >  0x1a0111ea397fe69a) return 0;
  if (src[4] <  0x4b1ba7b6434bacd7) return 1;
  if (src[4] >  0x4b1ba7b6434bacd7) return 0;
  if (src[3] <  0x64774b84f38512bf) return 1;
  if (src[3] >  0x64774b84f38512bf) return 0;
  if (src[2] <  0x6730d2a0f6b0f624) return 1;
  if (src[2] >  0x6730d2a0f6b0f624) return 0;
  if (src[1] <  0x1eabfffeb153ffff) return 1;
  if (src[1] >  0x1eabfffeb153ffff) return 0;
  if (src[0] <  0xb9feffffffffaaab) return 1;
  if (src[0] >= 0xb9feffffffffaaab) return 0;
return 1;
}

uint8_t bls12_381_Fp_mont_is_zero( const uint64_t *src ) {
  return bigint384_is_zero( src );
}

uint8_t bls12_381_Fp_mont_is_one( const uint64_t *src ) {
  return bigint384_is_equal( src, bls12_381_Fp_mont_R_modp );
}

uint8_t bls12_381_Fp_mont_is_equal( const uint64_t *src1, const uint64_t *src2 ) {
  return bigint384_is_equal( src1 , src2 );
}

void bls12_381_Fp_mont_set_zero( uint64_t *tgt ) {
  bigint384_set_zero( tgt );
}

void bls12_381_Fp_mont_set_one( uint64_t *tgt) {
  bigint384_copy( bls12_381_Fp_mont_R_modp , tgt );
}

void bls12_381_Fp_mont_copy( const uint64_t *src, uint64_t *tgt ) {
  bigint384_copy( src , tgt );
}

// convert a field elementfrom standard to Montgomery representation
void bls12_381_Fp_mont_from_std( const uint64_t *src, uint64_t *tgt) {
  bls12_381_Fp_mont_mul( src, bls12_381_Fp_mont_R_squared, tgt );
}

// convert a field element from Montgomery to standard representation
void bls12_381_Fp_mont_to_std( const uint64_t *src, uint64_t *tgt) {
  uint64_t T[13];
  memcpy( T, src, 48);
  memset( T+6, 0, 48);
  bls12_381_Fp_mont_REDC_unsafe( T, tgt );
};

// convert a vector of elements from standard to Montgomery representation
void bls12_381_Fp_mont_batch_from_std( int N, const uint64_t *src, uint64_t *tgt) {
  const uint64_t *p = src;
  uint64_t       *q = tgt;
  for(int i=0; i<N; i++) {
    bls12_381_Fp_mont_from_std(p,q);
    p += NLIMBS;
    q += NLIMBS;
  }
}

// convert a vector of elements from Montgomery to standard representation
void bls12_381_Fp_mont_batch_to_std( int N, const uint64_t *src, uint64_t *tgt) {
  const uint64_t *p = src;
  uint64_t       *q = tgt;
  for(int i=0; i<N; i++) {
    bls12_381_Fp_mont_to_std(p,q);
    p += NLIMBS;
    q += NLIMBS;
  }
};
