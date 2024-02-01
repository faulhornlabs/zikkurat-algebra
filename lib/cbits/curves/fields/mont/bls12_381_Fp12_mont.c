
// extension field `BLS12_381/Fp12`
//
// NOTE: generated code, do not edit!

#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>

#include "bls12_381_Fp12_mont.h"
#include "bls12_381_Fp6_mont.h"
#include "bls12_381_Fp_mont.h"

#define PRIME_NWORDS 6
#define BASE_NWORDS  36
#define EXT_NWORDS   72

#define EXT_DEGREE   2
#define PRIME_DEGREE 12

#define SRC1(i) ((src1) + (i)*BASE_NWORDS)
#define SRC2(i) ((src2) + (i)*BASE_NWORDS)
#define TGT(i)  (( tgt) + (i)*BASE_NWORDS)
#define PROD(i) ((prod) + (i)*BASE_NWORDS)
#define IRRED(i) (bls12_381_Fp12_mont_irred_coeffs + (i)*BASE_NWORDS)

#define MIN(a,b) ( ((a)<=(b)) ? (a) : (b) )
#define MAX(a,b) ( ((a)>=(b)) ? (a) : (b) )

uint64_t bls12_381_Fp12_mont_irred_coeffs[72] =
  { 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000
  , 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000
  , 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000
  , 0x43f5fffffffcaaae, 0x32b7fff2ed47fffd, 0x07e83a49a2e99d69, 0xeca8f3318332bb7a
  , 0xef148d1ea0f4c069, 0x040ab3263eff0206, 0x0000000000000000, 0x0000000000000000
  , 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000
  , 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000
  , 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000
  , 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000
  , 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000
  , 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000
  , 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000
  , 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000
  , 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000
  , 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000
  , 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000
  , 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000
  , 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000
  };


//------------------------------------------------------------------------------


void bls12_381_Fp12_mont_from_base_field ( const uint64_t *src , uint64_t *tgt ) {
  bls12_381_Fp12_mont_set_zero( tgt );
  bls12_381_Fp6_mont_copy( src, tgt );
}

void bls12_381_Fp12_mont_from_prime_field( const uint64_t *src , uint64_t *tgt ) {
  bls12_381_Fp12_mont_set_zero( tgt );
  bls12_381_Fp_mont_copy( src, tgt );
}

void bls12_381_Fp12_mont_scale_by_base_field ( const uint64_t *coeff , const uint64_t *src1, uint64_t *tgt ) {
  for(int i=0; i<EXT_DEGREE; i++) {
    bls12_381_Fp6_mont_mul( coeff, SRC1(i), TGT(i) );
  }
}

void bls12_381_Fp12_mont_scale_by_prime_field( const uint64_t *coeff , const uint64_t *src1, uint64_t *tgt ) {
  for(int i=0; i<PRIME_DEGREE; i++) {
    bls12_381_Fp_mont_mul( coeff , src1 + i*PRIME_NWORDS , tgt + i*PRIME_NWORDS );
  }
}

void bls12_381_Fp12_mont_div_by_2( const uint64_t *src1, uint64_t *tgt ) {
  for(int i=0; i<PRIME_DEGREE; i++) {
    bls12_381_Fp_mont_div_by_2( src1 + i*PRIME_NWORDS , tgt + i*PRIME_NWORDS );
  }
}

void bls12_381_Fp12_mont_div_by_2_inplace(  uint64_t *tgt ) {
  for(int i=0; i<PRIME_DEGREE; i++) {
    bls12_381_Fp_mont_div_by_2_inplace( tgt + i*PRIME_NWORDS );
  }
}

uint8_t bls12_381_Fp12_mont_is_valid ( const uint64_t *src1 ) {
  uint8_t ok = 1;
  for(int k=0; k<EXT_DEGREE; k++) {
    if (!bls12_381_Fp6_mont_is_valid( SRC1(k) )) { ok=0; break; }
  }
  return ok;
}

uint8_t bls12_381_Fp12_mont_is_zero  ( const uint64_t *src1 ) {
  uint8_t ok = 1;
  for(int k=0; k<EXT_DEGREE; k++) {
    if (!bls12_381_Fp6_mont_is_zero( SRC1(k) )) { ok=0; break; }
  }
  return ok;
}

uint8_t bls12_381_Fp12_mont_is_one ( const uint64_t *src1 ) {
  uint8_t ok = bls12_381_Fp6_mont_is_one( SRC1(0) );
  if (!ok) { return ok; }
  for(int k=1; k<EXT_DEGREE; k++) {
    if (!bls12_381_Fp6_mont_is_zero( SRC1(k) )) { ok=0; break; }
  }
  return ok;
}

uint8_t bls12_381_Fp12_mont_is_equal ( const uint64_t *src1, const uint64_t *src2 ) {
  uint8_t ok = 1;
  for(int k=0; k<EXT_DEGREE; k++) {
    if (!bls12_381_Fp6_mont_is_equal( SRC1(k) , SRC2(k) )) { ok=0; break; }
  }
  return ok;
}

void bls12_381_Fp12_mont_set_zero ( uint64_t *tgt ) {
  for(int k=0; k<EXT_DEGREE; k++) {
    bls12_381_Fp6_mont_set_zero( TGT(k) );
  }
}

void bls12_381_Fp12_mont_set_one ( uint64_t *tgt ) {
  bls12_381_Fp6_mont_set_one( tgt );
  for(int k=1; k<EXT_DEGREE; k++) {
    bls12_381_Fp6_mont_set_zero( TGT(k) );
  }
}

// src: base field
// tgt: extension field
void bls12_381_Fp12_mont_set_const ( const uint64_t *src1, uint64_t *tgt ) {
  bls12_381_Fp6_mont_copy( src1, tgt );
  for(int k=1; k<EXT_DEGREE; k++) {
    bls12_381_Fp6_mont_set_zero( TGT(k) );
  }
}

void bls12_381_Fp12_mont_copy ( const uint64_t *src1 , uint64_t *tgt ) {
  memcpy( tgt, src1, 8*EXT_NWORDS );
  // for(int k=0; k<EXT_DEGREE; k++) {
  //   bls12_381_Fp6_mont_copy( SRC1(k) , TGT(k) );
  // }
}


void bls12_381_Fp12_mont_neg ( const uint64_t *src1, uint64_t *tgt ) {
  for(int k=0; k<EXT_DEGREE; k++) {
    bls12_381_Fp6_mont_neg( SRC1(k) , TGT(k) );
  }
}

void bls12_381_Fp12_mont_add ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  for(int k=0; k<EXT_DEGREE; k++) {
    bls12_381_Fp6_mont_add( SRC1(k) , SRC2(k) , TGT(k) );
  }
}

void bls12_381_Fp12_mont_sub ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  for(int k=0; k<EXT_DEGREE; k++) {
    bls12_381_Fp6_mont_sub( SRC1(k) , SRC2(k) , TGT(k) );
  }
}

//--------------------------------------

void bls12_381_Fp12_mont_neg_inplace ( uint64_t *tgt ) {
  for(int k=0; k<EXT_DEGREE; k++) {
    bls12_381_Fp6_mont_neg_inplace( TGT(k) );
  }
}
void bls12_381_Fp12_mont_add_inplace ( uint64_t *tgt , const uint64_t *src2 ) {
  for(int k=0; k<EXT_DEGREE; k++) {
    bls12_381_Fp6_mont_add_inplace( TGT(k) , SRC2(k) );
  }
}

void bls12_381_Fp12_mont_sub_inplace ( uint64_t *tgt , const uint64_t *src2 ) {
  for(int k=0; k<EXT_DEGREE; k++) {
    bls12_381_Fp6_mont_sub_inplace( TGT(k) , SRC2(k) );
  }
}

void bls12_381_Fp12_mont_sub_inplace_reverse ( uint64_t *tgt , const uint64_t *src1 ) {
  for(int k=0; k<EXT_DEGREE; k++) {
    bls12_381_Fp6_mont_sub_inplace_reverse( TGT(k) , SRC1(k) );
  }
}


// we use Karatsuba trick to have only 3 multiplications
void bls12_381_Fp12_mont_mul ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  uint64_t p[BASE_NWORDS];
  uint64_t q[BASE_NWORDS];
  uint64_t r[BASE_NWORDS];
  uint64_t tmp[BASE_NWORDS];
  bls12_381_Fp6_mont_mul( SRC1(0) , SRC2(0) , p );         // a0*b0
  bls12_381_Fp6_mont_mul( SRC1(1) , SRC2(1) , r );         // a1*b1
  bls12_381_Fp6_mont_add( SRC1(0) , SRC1(1) , q );         // (a0+a1)
  bls12_381_Fp6_mont_add( SRC2(0) , SRC2(1) , tmp );       // (b0+b1)
  bls12_381_Fp6_mont_mul_inplace( q , tmp );               // (a0+a1)*(b0+b1)
  bls12_381_Fp6_mont_sub_inplace( q , p );
  bls12_381_Fp6_mont_sub_inplace( q , r );
  bls12_381_Fp6_mont_mul(  r , IRRED(0) , tmp );
  bls12_381_Fp6_mont_sub(  p , tmp , TGT(0) );
  bls12_381_Fp6_mont_copy( q ,     TGT(1) );
}

// we use Karatsuba trick to have only 3 squarings
void bls12_381_Fp12_mont_sqr ( const uint64_t *src1, uint64_t *tgt ) {
  uint64_t p[BASE_NWORDS];
  uint64_t q[BASE_NWORDS];
  uint64_t r[BASE_NWORDS];
  uint64_t tmp[BASE_NWORDS];
  bls12_381_Fp6_mont_sqr( SRC1(0) , p );              // a0^2
  bls12_381_Fp6_mont_sqr( SRC1(1) , r );              // a1^2
  bls12_381_Fp6_mont_add( SRC1(0) , SRC1(1) , q );    // (a0+a1)
  bls12_381_Fp6_mont_sqr_inplace( q );                // (a0+a1)^2
  bls12_381_Fp6_mont_sub_inplace( q , p );
  bls12_381_Fp6_mont_sub_inplace( q , r );
  bls12_381_Fp6_mont_mul(  r , IRRED(0) , tmp );
  bls12_381_Fp6_mont_sub(  p , tmp , TGT(0) );
  bls12_381_Fp6_mont_copy( q ,     TGT(1) );
}


void bls12_381_Fp12_mont_mul_inplace ( uint64_t *tgt , const uint64_t *src2 ) {
  bls12_381_Fp12_mont_mul( tgt, src2, tgt );
}

void bls12_381_Fp12_mont_sqr_inplace ( uint64_t *tgt ) {
  bls12_381_Fp12_mont_sqr( tgt, tgt );
}

void bls12_381_Fp12_mont_inv ( const uint64_t *src1, uint64_t *tgt ) {
  uint64_t denom     [BASE_NWORDS];
  uint64_t b_minus_ap[BASE_NWORDS];
  uint64_t tmp       [BASE_NWORDS];
  bls12_381_Fp6_mont_copy( SRC1(0) , b_minus_ap );                   // b-a*p = b (because p=0)
  bls12_381_Fp6_mont_sqr( SRC1(1) , denom );                         // a^2
  bls12_381_Fp6_mont_mul_inplace( denom , IRRED(0) );                // a^2*q
  bls12_381_Fp6_mont_mul( SRC1(0) , b_minus_ap , tmp );              // b*(b-a*p) = b^2-a*b*p
  bls12_381_Fp6_mont_add_inplace( denom , tmp );                     // (b^2 - a*b*p) + a^2*q
  bls12_381_Fp6_mont_inv_inplace( denom );                           // 1/(b^2 - a*b*p - a^2*q)
  bls12_381_Fp6_mont_neg( SRC1(1) , tmp );                           // -a
  bls12_381_Fp6_mont_mul( tmp        , denom , TGT(1) );             // c := -a/denom
  bls12_381_Fp6_mont_mul( b_minus_ap , denom, TGT(0) );              // d := (b-a*p)/denom
}

void bls12_381_Fp12_mont_inv_inplace ( uint64_t *tgt ) {
  bls12_381_Fp12_mont_inv ( tgt , tgt ); 
}

void bls12_381_Fp12_mont_div ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  uint64_t inv[EXT_NWORDS];
  bls12_381_Fp12_mont_inv ( src2 , inv );
  bls12_381_Fp12_mont_mul ( src1 , inv , tgt );
}

void bls12_381_Fp12_mont_div_inplace ( uint64_t *tgt, const uint64_t *src2 ) {
  bls12_381_Fp12_mont_div ( tgt , src2 , tgt ); 
}


const uint64_t bls12_381_Fp12_mont_frobenius_sparse_indices[18] = 
  { 0x0000000000000000, 0x0000000000010001, 0x0000000000020003, 0x0000000000030002, 0x0000000000040004, 0x0000000000050005, 0x0000000000060006, 0x0000000000060007, 0x0000000000070006, 0x0000000000070007, 0x0000000000080008, 0x0000000000080009, 0x0000000000090008, 0x0000000000090009, 0x00000000000a000a, 0x00000000000a000b, 0x00000000000b000a, 0x00000000000b000b
  };


const uint64_t bls12_381_Fp12_mont_frobenius_sparse_entries[108] = 
  { 0x760900000002fffd, 0xebf4000bc40c0002, 0x5f48985753c758ba, 0x77ce585370525745, 0x5c071a97a256ec6d, 0x15f65ec3fa80e493
  , 0x43f5fffffffcaaae, 0x32b7fff2ed47fffd, 0x07e83a49a2e99d69, 0xeca8f3318332bb7a, 0xef148d1ea0f4c069, 0x040ab3263eff0206
  , 0xcd03c9e48671f071, 0x5dab22461fcda5d2, 0x587042afd3851b95, 0x8eb60ebe01bacb9e, 0x03f97d6e83d050d2, 0x18f0206554638741
  , 0xcd03c9e48671f071, 0x5dab22461fcda5d2, 0x587042afd3851b95, 0x8eb60ebe01bacb9e, 0x03f97d6e83d050d2, 0x18f0206554638741
  , 0x890dc9e4867545c3, 0x2af322533285a5d5, 0x50880866309b7e2c, 0xa20d1b8c7e881024, 0x14e4f04fe2db9068, 0x14e56d3f1564853a
  , 0x30f1361b798a64e8, 0xf3b8ddab7ece5a2a, 0x16a8ca3ac61577f7, 0xc26a2ff874fd029b, 0x3636b76660701c6e, 0x051ba4ab241b6160
  , 0x07089552b319d465, 0xc6695f92b50a8313, 0x97e83cccd117228f, 0xa35baecab2dc29ee, 0x1ce393ea5daace4d, 0x08f2220fb0fb66eb
  , 0xb2f66aad4ce5d646, 0x5842a06bfc497cec, 0xcf4895d42599d394, 0xc11b9cba40a8e8d0, 0x2e3813cbe5a0de89, 0x110eefda88847faf
  , 0xb2f66aad4ce5d646, 0x5842a06bfc497cec, 0xcf4895d42599d394, 0xc11b9cba40a8e8d0, 0x2e3813cbe5a0de89, 0x110eefda88847faf
  , 0xb2f66aad4ce5d646, 0x5842a06bfc497cec, 0xcf4895d42599d394, 0xc11b9cba40a8e8d0, 0x2e3813cbe5a0de89, 0x110eefda88847faf
  , 0x7bcfa7a25aa30fda, 0xdc17dec12a927e7c, 0x2f088dd86b4ebef1, 0xd1ca2087da74d4a7, 0x2da2596696cebc1d, 0x0e2b7eedbbfd87d2
  , 0x7bcfa7a25aa30fda, 0xdc17dec12a927e7c, 0x2f088dd86b4ebef1, 0xd1ca2087da74d4a7, 0x2da2596696cebc1d, 0x0e2b7eedbbfd87d2
  , 0x7bcfa7a25aa30fda, 0xdc17dec12a927e7c, 0x2f088dd86b4ebef1, 0xd1ca2087da74d4a7, 0x2da2596696cebc1d, 0x0e2b7eedbbfd87d2
  , 0x3e2f585da55c9ad1, 0x4294213d86c18183, 0x382844c88b623732, 0x92ad2afd19103e18, 0x1d794e4fac7cf0b9, 0x0bd592fc7d825ec8
  , 0x82d83cf50dbce43f, 0xa2813e53df9d018f, 0xc6f0caa53c65e181, 0x7525cf528d50fe95, 0x4a85ed50f4798a6b, 0x171da0fd6cf8eebd
  , 0x3726c30af242c66c, 0x7c2ac1aad1b6fe70, 0xa04007fbba4b14a2, 0xef517c3266341429, 0x0095ba654ed2226b, 0x02e370eccc86f7dd
  , 0x3726c30af242c66c, 0x7c2ac1aad1b6fe70, 0xa04007fbba4b14a2, 0xef517c3266341429, 0x0095ba654ed2226b, 0x02e370eccc86f7dd
  , 0x3726c30af242c66c, 0x7c2ac1aad1b6fe70, 0xa04007fbba4b14a2, 0xef517c3266341429, 0x0095ba654ed2226b, 0x02e370eccc86f7dd
  };


void bls12_381_Fp12_mont_frobenius ( const uint64_t *src, uint64_t *tgt ) {
  uint64_t acc[EXT_NWORDS];
  uint64_t tmp[PRIME_NWORDS];
  bls12_381_Fp12_mont_set_zero(acc);
  for(int k=0; k<18; k++) {
    uint64_t ij = bls12_381_Fp12_mont_frobenius_sparse_indices[k];
    uint64_t i  = (ij >> 16);
    uint64_t j  = (ij &  0xffff);
    bls12_381_Fp_mont_mul( src + i*PRIME_NWORDS , bls12_381_Fp12_mont_frobenius_sparse_entries + k*PRIME_NWORDS , tmp );
    bls12_381_Fp_mont_add_inplace( acc + j*PRIME_NWORDS , tmp );
  }
  bls12_381_Fp12_mont_copy( acc, tgt );
}

void bls12_381_Fp12_mont_frobenius_inplace ( uint64_t *tgt ) {
  bls12_381_Fp12_mont_frobenius( tgt, tgt );
}

// computes `x^e mod p`
void bls12_381_Fp12_mont_pow_uint64( const uint64_t *src, uint64_t exponent, uint64_t *tgt ) {
  uint64_t e = exponent;
  uint64_t sqr[72];
  bls12_381_Fp12_mont_copy( src, sqr );                 // sqr := src
  bls12_381_Fp12_mont_set_one( tgt );                   // tgt := 1
  while(e!=0) {
    if (e & 1) { bls12_381_Fp12_mont_mul_inplace(tgt, sqr); }
    bls12_381_Fp12_mont_mul_inplace(sqr, sqr);
    e = e >> 1;
  }
}

// computes `x^e mod p` (for `e` non-negative bigint)
void bls12_381_Fp12_mont_pow_gen( const uint64_t *src, const uint64_t *expo, uint64_t *tgt, int expo_len ) {
  uint64_t sqr[72];
  bls12_381_Fp12_mont_copy( src, sqr );                 // sqr := src
  bls12_381_Fp12_mont_set_one( tgt );                   // tgt := 1
  int s = expo_len - 1;
  while ((expo[s] == 0) && (s>0)) { s--; }          // skip the unneeded largest powers
  for(int i=0; i<=s; i++) {
    uint64_t e = expo[i];
    for(int j=0; j<64; j++) {
      if (e & 1) { bls12_381_Fp12_mont_mul_inplace(tgt, sqr); }
      bls12_381_Fp12_mont_mul_inplace(sqr, sqr);
      e = e >> 1;
    }
  }
}

#define I_SRC(i)   (src    + (i)*72)
#define I_TGT(i)   (tgt    + (i)*72)
#define I_PROD(i)  (prods  + (i)*72)
#define I_RECIP(i) (recips + (i)*72)

// computes the inverse of many field elements at the same time, efficiently
// uses the Montgomery batch inversion trick
void bls12_381_Fp12_mont_batch_inv( int n, const uint64_t *src, uint64_t *tgt ) {
  assert( n >= 1 );
  uint64_t *prods  = malloc( 8*72*n );
  uint64_t *recips = malloc( 8*72*n );
  assert( prods  != 0 );
  assert( recips != 0 );
  
  // compute partial products (a[0]*a[1]*...*a[k]) for all k
  bls12_381_Fp12_mont_copy( I_SRC(0) , I_PROD(0) );
  for(int i=1; i<n; i++) {
    bls12_381_Fp12_mont_mul( I_PROD(i-1) , I_SRC(i) , I_PROD(i) );
  }
  
  // compute inverses of partial products 1/(a[0]*a[1]*...*a[k]) for all k
  bls12_381_Fp12_mont_inv( I_PROD(n-1) , I_RECIP(n-1) );
  for(int i=n-2; i>=0; i--) {
    bls12_381_Fp12_mont_mul( I_RECIP(i+1) , I_SRC(i+1) , I_RECIP(i) );
  }
  
  // compute the inverses 1/a[k] for all k
  bls12_381_Fp12_mont_copy( I_RECIP(0) , I_TGT(0) );
  for(int i=1; i<n; i++) {
    bls12_381_Fp12_mont_mul( I_RECIP(i) , I_PROD(i-1) , I_TGT(i) );
  }
  
  free(recips);
  free(prods);
}
