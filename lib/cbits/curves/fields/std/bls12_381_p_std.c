
// finite field arithmetic (standard representation) in the prime field with 
//
//   p = 4002409555221667393417789825735904156556882819939007885332058136124031650490837864442687629129015664037894272559787
//
// NOTE: generated code, do not edit!

#include <string.h>
#include <stdint.h>
#include <x86intrin.h>
#include "bls12_381_p_std.h"
#include "bigint384.h"

#define NLIMBS 6

const uint64_t bls12_381_p_std_prime[6] = { 0xb9feffffffffaaab, 0x1eabfffeb153ffff, 0x6730d2a0f6b0f624, 0x64774b84f38512bf, 0x4b1ba7b6434bacd7, 0x1a0111ea397fe69a };

//------------------------------------------------------------------------------

uint8_t bls12_381_p_std_is_zero( const uint64_t *src ) {
  return bigint384_is_zero( src );
}

uint8_t bls12_381_p_std_is_one( const uint64_t *src ) {
  return bigint384_is_one( src );
}

uint8_t bls12_381_p_std_is_equal( const uint64_t *src1, const uint64_t *src2 ) {
  return bigint384_is_equal( src1 , src2 );
}

void bls12_381_p_std_set_zero( uint64_t *tgt ) {
  bigint384_set_zero( tgt );
}

void bls12_381_p_std_set_one( uint64_t *tgt) {
  bigint384_set_one( tgt );
}

void bls12_381_p_std_copy( const uint64_t *src, uint64_t *tgt ) {
  bigint384_copy( src , tgt );
}

// adds the prime p to a bigint
uint8_t bls12_381_p_std_bigint384_add_prime( const uint64_t *src, uint64_t *tgt ) {
  return bigint384_add( src, bls12_381_p_std_prime, tgt );
}

// adds the prime p to a bigint, inplace
uint8_t bls12_381_p_std_bigint384_add_prime_inplace( uint64_t *tgt ) {
  return bigint384_add_inplace( tgt, bls12_381_p_std_prime);
}

// the constant `p + 1`
const uint64_t bls12_381_p_std_p_plus_1[6] = { 0xb9feffffffffaaac, 0x1eabfffeb153ffff, 0x6730d2a0f6b0f624, 0x64774b84f38512bf, 0x4b1ba7b6434bacd7, 0x1a0111ea397fe69a };

// adds `p+1` to the input, inplace
uint8_t bls12_381_p_std_bigint384_add_prime_plus_1_inplace( uint64_t *tgt ) {
  return bigint384_add_inplace( tgt, bls12_381_p_std_p_plus_1);
}

// subtracts the prime p from a bigint
uint8_t bls12_381_p_std_bigint384_sub_prime( const uint64_t *src, uint64_t *tgt ) {
  return bigint384_sub( src, bls12_381_p_std_prime, tgt );
}

// subtracts the prime p from a bigint, inplace
uint8_t bls12_381_p_std_bigint384_sub_prime_inplace( uint64_t *tgt ) {
  return bigint384_sub_inplace( tgt, bls12_381_p_std_prime);
}


// negates a field element
void bls12_381_p_std_neg( const uint64_t *src, uint64_t *tgt ) {
  if (bigint384_is_zero(src)) {
    bigint384_set_zero(tgt);
  }
  else {
    // mod (-x) p = p - x
    tgt[0] = 0xb9feffffffffaaab ;
    tgt[1] = 0x1eabfffeb153ffff ;
    tgt[2] = 0x6730d2a0f6b0f624 ;
    tgt[3] = 0x64774b84f38512bf ;
    tgt[4] = 0x4b1ba7b6434bacd7 ;
    tgt[5] = 0x1a0111ea397fe69a ;
    bigint384_sub_inplace(tgt, src);
  }
}

// negates a field element
void bls12_381_p_std_neg_inplace( uint64_t *tgt ) {
  if (bigint384_is_zero(tgt)) {
    return;
  }
  else {
    for(int i=0; i<6; i++) tgt[i] = ~tgt[i];
    bls12_381_p_std_bigint384_add_prime_plus_1_inplace(tgt);
  }
}

// checks if (x < prime)
uint8_t bls12_381_p_std_is_valid( const uint64_t *src ) {
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

// if (x >= prime) then (x - prime) else x
void bls12_381_p_std_bigint384_sub_prime_if_above_inplace( uint64_t *tgt ) {
  if (tgt[5] <  0x1a0111ea397fe69a) return;
  if (tgt[5] >  0x1a0111ea397fe69a) { bls12_381_p_std_bigint384_sub_prime_inplace( tgt ); return; }
  if (tgt[4] <  0x4b1ba7b6434bacd7) return;
  if (tgt[4] >  0x4b1ba7b6434bacd7) { bls12_381_p_std_bigint384_sub_prime_inplace( tgt ); return; }
  if (tgt[3] <  0x64774b84f38512bf) return;
  if (tgt[3] >  0x64774b84f38512bf) { bls12_381_p_std_bigint384_sub_prime_inplace( tgt ); return; }
  if (tgt[2] <  0x6730d2a0f6b0f624) return;
  if (tgt[2] >  0x6730d2a0f6b0f624) { bls12_381_p_std_bigint384_sub_prime_inplace( tgt ); return; }
  if (tgt[1] <  0x1eabfffeb153ffff) return;
  if (tgt[1] >  0x1eabfffeb153ffff) { bls12_381_p_std_bigint384_sub_prime_inplace( tgt ); return; }
  if (tgt[0] <  0xb9feffffffffaaab) return;
  if (tgt[0] >= 0xb9feffffffffaaab) { bls12_381_p_std_bigint384_sub_prime_inplace( tgt ); return; }
}

// adds two field elements
void bls12_381_p_std_add( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  uint8_t c = 0;
  c = bigint384_add( src1, src2, tgt );
  bls12_381_p_std_bigint384_sub_prime_if_above_inplace( tgt );
}

// adds two field elements, inplace
void bls12_381_p_std_add_inplace( uint64_t *tgt, const uint64_t *src2 ) {
  uint8_t c = 0;
  c = bigint384_add_inplace( tgt, src2 );
  bls12_381_p_std_bigint384_sub_prime_if_above_inplace( tgt );
}

// subtracts two field elements
void bls12_381_p_std_sub( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  uint8_t b = 0;
  b = bigint384_sub( src1, src2, tgt );
  if (b) { bls12_381_p_std_bigint384_add_prime_inplace( tgt ); }
}

// subtracts two field elements
void bls12_381_p_std_sub_inplace( uint64_t *tgt, const uint64_t *src2 ) {
  uint8_t b = 0;
  b = bigint384_sub_inplace( tgt, src2 );
  if (b) { bls12_381_p_std_bigint384_add_prime_inplace( tgt ); }
}

// tgt := src - tgt
void bls12_381_p_std_sub_inplace_reverse( uint64_t *tgt, const uint64_t *src1 ) {
  uint8_t b = 0;
  b = bigint384_sub_inplace_reverse( tgt, src1 );
  if (b) { bls12_381_p_std_bigint384_add_prime_inplace( tgt ); }
}

// squares a field elements
void bls12_381_p_std_sqr( const uint64_t *src, uint64_t *tgt ) {
  uint64_t prod[12];
  bigint384_sqr( src, prod );
  bls12_381_p_std_reduce_modp( prod, tgt );
}

void bls12_381_p_std_sqr_inplace( uint64_t *tgt ) {
  uint64_t prod[12];
  bigint384_sqr( tgt, prod );
  bls12_381_p_std_reduce_modp( prod, tgt );
}

// multiplies two field elements
void bls12_381_p_std_mul( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  uint64_t prod[12];
  bigint384_mul( src1, src2, prod );
  bls12_381_p_std_reduce_modp( prod, tgt );
}

void bls12_381_p_std_mul_inplace( uint64_t *tgt, const uint64_t *src2 ) {
  uint64_t prod[12];
  bigint384_mul( tgt, src2, prod );
  bls12_381_p_std_reduce_modp( prod, tgt );
}

// table of `ceil(2^64 * (2^(64*m) mod p) / p)`
static const uint64_t bls12_381_p_std_qps_table[12] = { 0x0000000000000001, 0x0000000000000001, 0x0000000000000001, 0x0000000000000001, 0x0000000000000001, 0x000000000000000a, 0xd835d2f3cc9e45cf, 0x28101b0cc7a6ba2a, 0x1b82741ff6a0a94c, 0xdf4771e0286779d4, 0x997167a058f1c07c, 0x13e207f56591ba2f };

// table of `2^(64*m) mod p`
static const uint64_t bls12_381_p_std_mps_table[72] = { 
0x0000000000000001, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000,
0x0000000000000000, 0x0000000000000001, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000,
0x0000000000000000, 0x0000000000000000, 0x0000000000000001, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000,
0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000001, 0x0000000000000000, 0x0000000000000000,
0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000001, 0x0000000000000000,
0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000001,
0x760900000002fffd, 0xebf4000bc40c0002, 0x5f48985753c758ba, 0x77ce585370525745, 0x5c071a97a256ec6d, 0x15f65ec3fa80e493,
0x42b7fde37dba9366, 0x7784894e27525bc3, 0xb2b91b9dc1f5b1e9, 0x206f497dfcafb872, 0x594137cc89a9b0bb, 0x0411cd9d20d7e399,
0x967d3988a62b6c9d, 0x4532383fa8eaff4e, 0x8238308703310846, 0x15efebb5d396d7cf, 0xfab7cd07ee4e16bc, 0x02cb5d3a884e56c4,
0x9e2a2b803b8e1ce7, 0xcede519d701f5ab3, 0xa0a6217e81ab4621, 0xde9fabaf9eee6510, 0xef148e598e4a8041, 0x16ae30783dff1a47,
0x075b3cd7c5ce820f, 0x3ec6ba621c3edb0b, 0x168a13d82bff6bce, 0x87663c4bf8c449d2, 0x15f34c83ddc8d830, 0x0f9628b49caa2e85,
0xfc8aa5c5cd2dbfd7, 0x4874042cd26528c7, 0xd01b07fd232a8e28, 0x23d6a1069b97e001, 0xa1a742c38b4c65b3, 0x02050a1527e00758,
};

// subtracts two big integers made up from `nlimbs+1` limbs
uint8_t bls12_381_p_std_bigint_sub_inplace_larger( uint64_t *tgt, const uint64_t *src2 ) {
  uint8_t b = 0;
  for(int j=0; j<7; j++) {
    b = _subborrow_u64( b, tgt[j], src2[j], tgt+j );
  }
  return b;
}

 // reduces a number of size 12 limbs modulo p
 // similar the Barret reduction (?)
void bls12_381_p_std_reduce_modp( const uint64_t *src, uint64_t *tgt ) {
  uint64_t tmp1[7];
  uint64_t tmp2[7];
  for(int k=0; k<4; k++) { tgt[k] = src[k]; }
  for(int k=4; k<6; k++) { tgt[k] = 0; }
  for(int m=4; m<12; m++) {
    bigint384_scale( src[m], bls12_381_p_std_mps_table + 6*m, tmp1 );
    __uint128_t q = src[m];
    q = q * bls12_381_p_std_qps_table[m];    // this is `2^(64m) * src[m] / p` in 64-bit fixed-point form
    bigint384_scale( (uint64_t)(q>>64), bls12_381_p_std_prime, tmp2 );
    uint8_t b = bls12_381_p_std_bigint_sub_inplace_larger( tmp1, tmp2 );
    if (b) { bls12_381_p_std_bigint384_add_prime_inplace( tmp1 ); }
    bls12_381_p_std_add_inplace( tgt , tmp1);
  }
}

// computes `x^e mod p`
void bls12_381_p_std_pow_uint64( const uint64_t *src, uint64_t exponent, uint64_t *tgt ) {
  uint64_t e = exponent;
  uint64_t sqr[6];
  bigint384_copy( src, sqr );             // sqr := src
  bigint384_set_one( tgt );                     // tgt := 1
  while(e!=0) {
    if (e & 1) { bls12_381_p_std_mul_inplace(tgt, sqr); }
    bls12_381_p_std_mul_inplace(sqr, sqr);
    e = e >> 1;
  }
}

// computes `x^e mod p` (for `e` non-negative bigint)
void bls12_381_p_std_pow_gen( const uint64_t *src, const uint64_t *expo, uint64_t *tgt, int expo_len ) {
  uint64_t sqr[6];
  bigint384_copy( src, sqr );             // sqr := src
  bigint384_set_one( tgt );                     // tgt := 1
  int s = expo_len - 1;
  while ((expo[s] == 0) && (s>0)) { s--; }          // skip the unneeded largest powers
  for(int i=0; i<=s; i++) {
    uint64_t e = expo[i];
    for(int j=0; j<64; j++) {
      if (e & 1) { bls12_381_p_std_mul_inplace(tgt, sqr); }
      bls12_381_p_std_mul_inplace(sqr, sqr);
      e = e >> 1;
    }
  }
}

// `(p+1) / 2 = (div p 2) + 1`
const uint64_t bls12_381_p_std_half_p_plus_1[6] = { 0xdcff7fffffffd556, 0x0f55ffff58a9ffff, 0xb39869507b587b12, 0xb23ba5c279c2895f, 0x258dd3db21a5d66b, 0x0d0088f51cbff34d };

// multiply by the inverse of 2
// if the input is of the form `2k` then we just shift right
// if the input is of the form `2k+1`, then:
//   (2k+1)/2 = (2k+1+p)/2 = (2k+(p+1))/2 = k + (p+1)/2
// also the latter addition will never overflow.
//
void bls12_381_p_std_div_by_2( const uint64_t *src, uint64_t *tgt ) {
  uint8_t odd = bigint384_shift_right_by_1(src, tgt);
  if (odd) { bigint384_add_inplace(tgt, bls12_381_p_std_half_p_plus_1); }
}

void bls12_381_p_std_div_by_2_inplace( uint64_t *tgt ) {
  uint8_t odd = bigint384_shift_right_by_1(tgt, tgt);
  if (odd) { bigint384_add_inplace(tgt, bls12_381_p_std_half_p_plus_1); }
}

// extended binary euclidean algorithm
void bls12_381_p_std_euclid( uint64_t *x1, uint64_t *x2, uint64_t *u, uint64_t *v, uint64_t *tgt ) {

  while( ( (!bigint384_is_one(u)) && (!bigint384_is_one(v)) ) ) {

    // note: x1 < p
    // if x1 is odd, it can't be p-1, hence, it's at most p-2
    // then we divide by two: (p-2)/2 = (p-3)/2
    // (p-3)/2 + (p+1)/2 = (2p-2)/2 = (p-1)
    // so the addition x1 + (p+1)/2 = (x1+p)/2 will never overflow

    while (!(u[0] & 1)) {
      bigint384_shift_right_by_1(u,u);
      uint8_t odd = bigint384_shift_right_by_1(x1,x1);
      if (odd) { bigint384_add_inplace(x1, bls12_381_p_std_half_p_plus_1); }
    }

    while (!(v[0] & 1)) {
      bigint384_shift_right_by_1(v,v);
      uint8_t odd = bigint384_shift_right_by_1(x2,x2);
      if (odd) { bigint384_add_inplace(x2, bls12_381_p_std_half_p_plus_1); }
    }

    uint64_t w[6];
    uint8_t b = bigint384_sub(u,v,w);  // w = u - v 
    if (b) {
      // u-v < 0, that is, u < v
      bigint384_neg(w,v);              // v  := v  - u
      bls12_381_p_std_sub_inplace(x2,x1);     // x2 := x2 - x1
    }
    else {
      // u-v >= 0, that is, u >= v
      bigint384_copy(w,u);             // u  := u  - v
      bls12_381_p_std_sub_inplace(x1,x2);     // x1 := x1 - x2
    }
  
  }

  if (bigint384_is_one(u)) { 
    bigint384_copy( x1, tgt ); 
  } 
  else { 
    bigint384_copy( x2, tgt ); 
  }
}

// inverse of a field element
void bls12_381_p_std_inv( const uint64_t *src, uint64_t *tgt ) {
  if (bigint384_is_zero(src)) { 
    bigint384_set_zero(tgt); 
  } 
  else {
    uint64_t x1[6];
    uint64_t x2[6];
    uint64_t u [6];
    uint64_t v [6];

    bigint384_set_one   ( x1 );               // x1 := 1     
    bigint384_set_zero  ( x2 );               // x2 := 0     
    bigint384_copy( src       , u );    // u  := src    
    bigint384_copy( bls12_381_p_std_prime , v );    // v  := p      
    
    bls12_381_p_std_euclid(x1,x2,u,v,tgt);
  }
}

void bls12_381_p_std_inv_inplace( uint64_t *tgt ) {
  bls12_381_p_std_inv(tgt,tgt);
}

// division in the field
void bls12_381_p_std_div( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  if (bigint384_is_zero(src2)) { 
    bigint384_set_zero(tgt); 
  } 
  else {
    uint64_t x1[6];
    uint64_t x2[6];
    uint64_t u [6];
    uint64_t v [6];

    bigint384_copy( src1 , x1 );         // x1 := src1  
    bigint384_set_zero  ( x2 );                // x2 := 0     
    bigint384_copy( src2      , u );     // u  := src2  
    bigint384_copy( bls12_381_p_std_prime , v );     // v  := p     
    
    bls12_381_p_std_euclid(x1,x2,u,v,tgt);
  }
}

void bls12_381_p_std_div_inplace( uint64_t *tgt, const uint64_t *src2 ) {
  bls12_381_p_std_div(tgt,src2,tgt);
}
