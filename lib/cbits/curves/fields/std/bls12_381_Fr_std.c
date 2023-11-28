
// finite field arithmetic (standard representation) in the prime field with 
//
//   p = 52435875175126190479447740508185965837690552500527637822603658699938581184513
//
// NOTE: generated code, do not edit!

#include <string.h>
#include <stdint.h>
#include <stdlib.h>
#include <assert.h>

#include "bls12_381_Fr_std.h"
#include "bigint256.h"
#include "platform.h"

#define NLIMBS 4

const uint64_t bls12_381_Fr_std_prime[4] = { 0xffffffff00000001, 0x53bda402fffe5bfe, 0x3339d80809a1d805, 0x73eda753299d7d48 };

//------------------------------------------------------------------------------

uint8_t bls12_381_Fr_std_is_zero( const uint64_t *src ) {
  return bigint256_is_zero( src );
}

uint8_t bls12_381_Fr_std_is_one( const uint64_t *src ) {
  return bigint256_is_one( src );
}

uint8_t bls12_381_Fr_std_is_equal( const uint64_t *src1, const uint64_t *src2 ) {
  return bigint256_is_equal( src1 , src2 );
}

void bls12_381_Fr_std_set_zero( uint64_t *tgt ) {
  bigint256_set_zero( tgt );
}

void bls12_381_Fr_std_set_one( uint64_t *tgt) {
  bigint256_set_one( tgt );
}

void bls12_381_Fr_std_copy( const uint64_t *src, uint64_t *tgt ) {
  bigint256_copy( src , tgt );
}

// adds the prime p to a bigint
uint8_t bls12_381_Fr_std_bigint256_add_prime( const uint64_t *src, uint64_t *tgt ) {
  return bigint256_add( src, bls12_381_Fr_std_prime, tgt );
}

// adds the prime p to a bigint, inplace
uint8_t bls12_381_Fr_std_bigint256_add_prime_inplace( uint64_t *tgt ) {
  return bigint256_add_inplace( tgt, bls12_381_Fr_std_prime);
}

// the constant `p + 1`
const uint64_t bls12_381_Fr_std_p_plus_1[4] = { 0xffffffff00000002, 0x53bda402fffe5bfe, 0x3339d80809a1d805, 0x73eda753299d7d48 };

// adds `p+1` to the input, inplace
uint8_t bls12_381_Fr_std_bigint256_add_prime_plus_1_inplace( uint64_t *tgt ) {
  return bigint256_add_inplace( tgt, bls12_381_Fr_std_p_plus_1);
}

// subtracts the prime p from a bigint
uint8_t bls12_381_Fr_std_bigint256_sub_prime( const uint64_t *src, uint64_t *tgt ) {
  return bigint256_sub( src, bls12_381_Fr_std_prime, tgt );
}

// subtracts the prime p from a bigint, inplace
uint8_t bls12_381_Fr_std_bigint256_sub_prime_inplace( uint64_t *tgt ) {
  return bigint256_sub_inplace( tgt, bls12_381_Fr_std_prime);
}


// negates a field element
void bls12_381_Fr_std_neg( const uint64_t *src, uint64_t *tgt ) {
  if (bigint256_is_zero(src)) {
    bigint256_set_zero(tgt);
  }
  else {
    // mod (-x) p = p - x
    uint64_t tmp[NLIMBS];
    memcpy(tmp, src, 8*NLIMBS);   // if tgt==src, it would overwrite `src` below...
    tgt[0] = 0xffffffff00000001 ;
    tgt[1] = 0x53bda402fffe5bfe ;
    tgt[2] = 0x3339d80809a1d805 ;
    tgt[3] = 0x73eda753299d7d48 ;
    bigint256_sub_inplace(tgt, tmp);  // src);
  }
}

// negates a field element
void bls12_381_Fr_std_neg_inplace( uint64_t *tgt ) {
  if (bigint256_is_zero(tgt)) {
    return;
  }
  else {
    for(int i=0; i<4; i++) tgt[i] = ~tgt[i];
    bls12_381_Fr_std_bigint256_add_prime_plus_1_inplace(tgt);
  }
}

// checks if (x < prime)
uint8_t bls12_381_Fr_std_is_valid( const uint64_t *src ) {
  if (src[3] <  0x73eda753299d7d48) return 1;
  if (src[3] >  0x73eda753299d7d48) return 0;
  if (src[2] <  0x3339d80809a1d805) return 1;
  if (src[2] >  0x3339d80809a1d805) return 0;
  if (src[1] <  0x53bda402fffe5bfe) return 1;
  if (src[1] >  0x53bda402fffe5bfe) return 0;
  if (src[0] <  0xffffffff00000001) return 1;
  if (src[0] >= 0xffffffff00000001) return 0;
return 1;
}

// if (x >= prime) then (x - prime) else x
void bls12_381_Fr_std_bigint256_sub_prime_if_above_inplace( uint64_t *tgt ) {
  if (tgt[3] <  0x73eda753299d7d48) return;
  if (tgt[3] >  0x73eda753299d7d48) { bls12_381_Fr_std_bigint256_sub_prime_inplace( tgt ); return; }
  if (tgt[2] <  0x3339d80809a1d805) return;
  if (tgt[2] >  0x3339d80809a1d805) { bls12_381_Fr_std_bigint256_sub_prime_inplace( tgt ); return; }
  if (tgt[1] <  0x53bda402fffe5bfe) return;
  if (tgt[1] >  0x53bda402fffe5bfe) { bls12_381_Fr_std_bigint256_sub_prime_inplace( tgt ); return; }
  if (tgt[0] <  0xffffffff00000001) return;
  if (tgt[0] >= 0xffffffff00000001) { bls12_381_Fr_std_bigint256_sub_prime_inplace( tgt ); return; }
}

// adds two field elements
void bls12_381_Fr_std_add( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  uint8_t c = 0;
  c = bigint256_add( src1, src2, tgt );
  bls12_381_Fr_std_bigint256_sub_prime_if_above_inplace( tgt );
}

// adds two field elements, inplace
void bls12_381_Fr_std_add_inplace( uint64_t *tgt, const uint64_t *src2 ) {
  uint8_t c = 0;
  c = bigint256_add_inplace( tgt, src2 );
  bls12_381_Fr_std_bigint256_sub_prime_if_above_inplace( tgt );
}

// subtracts two field elements
void bls12_381_Fr_std_sub( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  uint8_t b = 0;
  b = bigint256_sub( src1, src2, tgt );
  if (b) { bls12_381_Fr_std_bigint256_add_prime_inplace( tgt ); }
}

// subtracts two field elements
void bls12_381_Fr_std_sub_inplace( uint64_t *tgt, const uint64_t *src2 ) {
  uint8_t b = 0;
  b = bigint256_sub_inplace( tgt, src2 );
  if (b) { bls12_381_Fr_std_bigint256_add_prime_inplace( tgt ); }
}

// tgt := src - tgt
void bls12_381_Fr_std_sub_inplace_reverse( uint64_t *tgt, const uint64_t *src1 ) {
  uint8_t b = 0;
  b = bigint256_sub_inplace_reverse( tgt, src1 );
  if (b) { bls12_381_Fr_std_bigint256_add_prime_inplace( tgt ); }
}

// squares a field elements
void bls12_381_Fr_std_sqr( const uint64_t *src, uint64_t *tgt ) {
  uint64_t prod[8];
  bigint256_sqr( src, prod );
  bls12_381_Fr_std_reduce_modp( prod, tgt );
}

void bls12_381_Fr_std_sqr_inplace( uint64_t *tgt ) {
  uint64_t prod[8];
  bigint256_sqr( tgt, prod );
  bls12_381_Fr_std_reduce_modp( prod, tgt );
}

// multiplies two field elements
void bls12_381_Fr_std_mul( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  uint64_t prod[8];
  bigint256_mul( src1, src2, prod );
  bls12_381_Fr_std_reduce_modp( prod, tgt );
}

void bls12_381_Fr_std_mul_inplace( uint64_t *tgt, const uint64_t *src2 ) {
  uint64_t prod[8];
  bigint256_mul( tgt, src2, prod );
  bls12_381_Fr_std_reduce_modp( prod, tgt );
}

// table of `ceil(2^64 * (2^(64*m) mod p) / p)`
static const uint64_t bls12_381_Fr_std_qps_table[8] = { 0x0000000000000001, 0x0000000000000001, 0x0000000000000001, 0x0000000000000003, 0x355094edfede377d, 0x38b5dcb707e08ed4, 0x65043eb4be4bad72, 0x42737a020c0d6394 };

// table of `2^(64*m) mod p`
static const uint64_t bls12_381_Fr_std_mps_table[32] = { 
0x0000000000000001, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000,
0x0000000000000000, 0x0000000000000001, 0x0000000000000000, 0x0000000000000000,
0x0000000000000000, 0x0000000000000000, 0x0000000000000001, 0x0000000000000000,
0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000001,
0x00000001fffffffe, 0x5884b7fa00034802, 0x998c4fefecbc4ff5, 0x1824b159acc5056f,
0xc98da28e0121c884, 0xe6f4f4a0c7363c67, 0xb2d6ebc4e92e7df1, 0x19ae57949d26242a,
0xcf2ab21bf81f712d, 0x9277efb8ac0a600d, 0x7abbe5687369510a, 0x2dbeaf1fd4843acb,
0x59476ebc41b4528f, 0xc5a30cb243fcc152, 0x2b34e63940ccbd72, 0x1e179025ca247088,
};

// subtracts two big integers made up from `nlimbs+1` limbs
uint8_t bls12_381_Fr_std_bigint_sub_inplace_larger( uint64_t *tgt, const uint64_t *src2 ) {
  uint8_t b = 0;
  for(int j=0; j<5; j++) {
    b = subborrow_u64( b, tgt[j], src2[j], tgt+j );
  }
  return b;
}

 // reduces a number of size 8 limbs modulo p
 // similar the Barret reduction (?)
void bls12_381_Fr_std_reduce_modp( const uint64_t *src, uint64_t *tgt ) {
  uint64_t tmp1[5];
  uint64_t tmp2[5];
  for(int k=0; k<2; k++) { tgt[k] = src[k]; }
  for(int k=2; k<4; k++) { tgt[k] = 0; }
  for(int m=2; m<8; m++) {
    bigint256_scale( src[m], bls12_381_Fr_std_mps_table + 4*m, tmp1 );
    __uint128_t q = src[m];
    q = q * bls12_381_Fr_std_qps_table[m];    // this is `2^(64m) * src[m] / p` in 64-bit fixed-point form
    bigint256_scale( (uint64_t)(q>>64), bls12_381_Fr_std_prime, tmp2 );
    uint8_t b = bls12_381_Fr_std_bigint_sub_inplace_larger( tmp1, tmp2 );
    if (b) { bls12_381_Fr_std_bigint256_add_prime_inplace( tmp1 ); }
    bls12_381_Fr_std_add_inplace( tgt , tmp1);
  }
}

// `(p+1) / 2 = (div p 2) + 1`
const uint64_t bls12_381_Fr_std_half_p_plus_1[4] = { 0x7fffffff80000001, 0xa9ded2017fff2dff, 0x199cec0404d0ec02, 0x39f6d3a994cebea4 };

// multiply by the inverse of 2
// if the input is of the form `2k` then we just shift right
// if the input is of the form `2k+1`, then:
//   (2k+1)/2 = (2k+1+p)/2 = (2k+(p+1))/2 = k + (p+1)/2
// also the latter addition will never overflow.
//
void bls12_381_Fr_std_div_by_2( const uint64_t *src, uint64_t *tgt ) {
  uint8_t odd = bigint256_shift_right_by_1(src, tgt);
  if (odd) { bigint256_add_inplace(tgt, bls12_381_Fr_std_half_p_plus_1); }
}

void bls12_381_Fr_std_div_by_2_inplace( uint64_t *tgt ) {
  uint8_t odd = bigint256_shift_right_by_1(tgt, tgt);
  if (odd) { bigint256_add_inplace(tgt, bls12_381_Fr_std_half_p_plus_1); }
}

// extended binary euclidean algorithm
void bls12_381_Fr_std_euclid( uint64_t *x1, uint64_t *x2, uint64_t *u, uint64_t *v, uint64_t *tgt ) {

  while( ( (!bigint256_is_one(u)) && (!bigint256_is_one(v)) ) ) {

    // note: x1 < p
    // if x1 is odd, it can't be p-1, hence, it's at most p-2
    // then we divide by two: (p-2)/2 = (p-3)/2
    // (p-3)/2 + (p+1)/2 = (2p-2)/2 = (p-1)
    // so the addition x1 + (p+1)/2 = (x1+p)/2 will never overflow

    while (!(u[0] & 1)) {
      bigint256_shift_right_by_1(u,u);
      uint8_t odd = bigint256_shift_right_by_1(x1,x1);
      if (odd) { bigint256_add_inplace(x1, bls12_381_Fr_std_half_p_plus_1); }
    }

    while (!(v[0] & 1)) {
      bigint256_shift_right_by_1(v,v);
      uint8_t odd = bigint256_shift_right_by_1(x2,x2);
      if (odd) { bigint256_add_inplace(x2, bls12_381_Fr_std_half_p_plus_1); }
    }

    uint64_t w[4];
    uint8_t b = bigint256_sub(u,v,w);  // w = u - v 
    if (b) {
      // u-v < 0, that is, u < v
      bigint256_neg(w,v);              // v  := v  - u
      bls12_381_Fr_std_sub_inplace(x2,x1);     // x2 := x2 - x1
    }
    else {
      // u-v >= 0, that is, u >= v
      bigint256_copy(w,u);             // u  := u  - v
      bls12_381_Fr_std_sub_inplace(x1,x2);     // x1 := x1 - x2
    }
  
  }

  if (bigint256_is_one(u)) { 
    bigint256_copy( x1, tgt ); 
  } 
  else { 
    bigint256_copy( x2, tgt ); 
  }
}

// inverse of a field element
void bls12_381_Fr_std_inv( const uint64_t *src, uint64_t *tgt ) {
  if (bigint256_is_zero(src)) { 
    bigint256_set_zero(tgt); 
  } 
  else {
    uint64_t x1[4];
    uint64_t x2[4];
    uint64_t u [4];
    uint64_t v [4];

    bigint256_set_one   ( x1 );               // x1 := 1     
    bigint256_set_zero  ( x2 );               // x2 := 0     
    bigint256_copy( src       , u );    // u  := src    
    bigint256_copy( bls12_381_Fr_std_prime , v );    // v  := p      
    
    bls12_381_Fr_std_euclid(x1,x2,u,v,tgt);
  }
}

void bls12_381_Fr_std_inv_inplace( uint64_t *tgt ) {
  bls12_381_Fr_std_inv(tgt,tgt);
}

// division in the field
void bls12_381_Fr_std_div( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  if (bigint256_is_zero(src2)) { 
    bigint256_set_zero(tgt); 
  } 
  else {
    uint64_t x1[4];
    uint64_t x2[4];
    uint64_t u [4];
    uint64_t v [4];

    bigint256_copy( src1 , x1 );         // x1 := src1  
    bigint256_set_zero  ( x2 );                // x2 := 0     
    bigint256_copy( src2      , u );     // u  := src2  
    bigint256_copy( bls12_381_Fr_std_prime , v );     // v  := p     
    
    bls12_381_Fr_std_euclid(x1,x2,u,v,tgt);
  }
}

void bls12_381_Fr_std_div_inplace( uint64_t *tgt, const uint64_t *src2 ) {
  bls12_381_Fr_std_div(tgt,src2,tgt);
}

// computes `x^e mod p`
void bls12_381_Fr_std_pow_uint64( const uint64_t *src, uint64_t exponent, uint64_t *tgt ) {
  uint64_t e = exponent;
  uint64_t sqr[4];
  bls12_381_Fr_std_copy( src, sqr );                 // sqr := src
  bls12_381_Fr_std_set_one( tgt );                   // tgt := 1
  while(e!=0) {
    if (e & 1) { bls12_381_Fr_std_mul_inplace(tgt, sqr); }
    bls12_381_Fr_std_mul_inplace(sqr, sqr);
    e = e >> 1;
  }
}

// computes `x^e mod p` (for `e` non-negative bigint)
void bls12_381_Fr_std_pow_gen( const uint64_t *src, const uint64_t *expo, uint64_t *tgt, int expo_len ) {
  uint64_t sqr[4];
  bls12_381_Fr_std_copy( src, sqr );                 // sqr := src
  bls12_381_Fr_std_set_one( tgt );                   // tgt := 1
  int s = expo_len - 1;
  while ((expo[s] == 0) && (s>0)) { s--; }          // skip the unneeded largest powers
  for(int i=0; i<=s; i++) {
    uint64_t e = expo[i];
    for(int j=0; j<64; j++) {
      if (e & 1) { bls12_381_Fr_std_mul_inplace(tgt, sqr); }
      bls12_381_Fr_std_mul_inplace(sqr, sqr);
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
void bls12_381_Fr_std_batch_inv( int n, const uint64_t *src, uint64_t *tgt ) {
  assert( n >= 1 );
  uint64_t *prods  = malloc( 8*4*n );
  uint64_t *recips = malloc( 8*4*n );
  assert( prods  != 0 );
  assert( recips != 0 );
  
  // compute partial products (a[0]*a[1]*...*a[k]) for all k
  bls12_381_Fr_std_copy( I_SRC(0) , I_PROD(0) );
  for(int i=1; i<n; i++) {
    bls12_381_Fr_std_mul( I_PROD(i-1) , I_SRC(i) , I_PROD(i) );
  }
  
  // compute inverses of partial products 1/(a[0]*a[1]*...*a[k]) for all k
  bls12_381_Fr_std_inv( I_PROD(n-1) , I_RECIP(n-1) );
  for(int i=n-2; i>=0; i--) {
    bls12_381_Fr_std_mul( I_RECIP(i+1) , I_SRC(i+1) , I_RECIP(i) );
  }
  
  // compute the inverses 1/a[k] for all k
  bls12_381_Fr_std_copy( I_RECIP(0) , I_TGT(0) );
  for(int i=1; i<n; i++) {
    bls12_381_Fr_std_mul( I_RECIP(i) , I_PROD(i-1) , I_TGT(i) );
  }
  
  free(recips);
  free(prods);
}
