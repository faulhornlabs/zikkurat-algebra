
// extension field `BLS12_381/Fp2`
//
// NOTE: generated code, do not edit!

#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>

#include "bls12_381_Fp2_mont.h"
#include "bls12_381_Fp_mont.h"

#define BASE_NWORDS 6
#define EXT_NWORDS  12
#define EXT_DEGREE  2

#define SRC1(i) ((src1) + (i)*BASE_NWORDS)
#define SRC2(i) ((src2) + (i)*BASE_NWORDS)
#define TGT(i)  (( tgt) + (i)*BASE_NWORDS)
#define PROD(i) ((prod) + (i)*BASE_NWORDS)
#define IRRED(i) (bls12_381_Fp2_mont_irred_coeffs + (i)*BASE_NWORDS)

#define MIN(a,b) ( ((a)<=(b)) ? (a) : (b) )
#define MAX(a,b) ( ((a)>=(b)) ? (a) : (b) )

uint64_t bls12_381_Fp2_mont_irred_coeffs[12] =
  { 0x760900000002fffd, 0xebf4000bc40c0002, 0x5f48985753c758ba, 0x77ce585370525745
  , 0x5c071a97a256ec6d, 0x15f65ec3fa80e493, 0x0000000000000000, 0x0000000000000000
  , 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000
  };


//------------------------------------------------------------------------------

uint8_t bls12_381_Fp2_mont_is_valid ( const uint64_t *src1 ) {
  uint8_t ok = 1;
  for(int k=0; k<EXT_DEGREE; k++) {
    if (!bls12_381_Fp_mont_is_valid( SRC1(k) )) { ok=0; break; }
  }
  return ok;
}

uint8_t bls12_381_Fp2_mont_is_zero  ( const uint64_t *src1 ) {
  uint8_t ok = 1;
  for(int k=0; k<EXT_DEGREE; k++) {
    if (!bls12_381_Fp_mont_is_zero( SRC1(k) )) { ok=0; break; }
  }
  return ok;
}

uint8_t bls12_381_Fp2_mont_is_one ( const uint64_t *src1 ) {
  uint8_t ok = bls12_381_Fp_mont_is_one( SRC1(0) );
  if (!ok) { return ok; }
  for(int k=1; k<EXT_DEGREE; k++) {
    if (!bls12_381_Fp_mont_is_zero( SRC1(k) )) { ok=0; break; }
  }
  return ok;
}

uint8_t bls12_381_Fp2_mont_is_equal ( const uint64_t *src1, const uint64_t *src2 ) {
  uint8_t ok = 1;
  for(int k=0; k<EXT_DEGREE; k++) {
    if (!bls12_381_Fp_mont_is_equal( SRC1(k) , SRC2(k) )) { ok=0; break; }
  }
  return ok;
}

void bls12_381_Fp2_mont_set_zero ( uint64_t *tgt ) {
  for(int k=0; k<EXT_DEGREE; k++) {
    bls12_381_Fp_mont_set_zero( TGT(k) );
  }
}

void bls12_381_Fp2_mont_set_one ( uint64_t *tgt ) {
  bls12_381_Fp_mont_set_one( tgt );
  for(int k=1; k<EXT_DEGREE; k++) {
    bls12_381_Fp_mont_set_zero( TGT(k) );
  }
}

// src: base field
// tgt: extension field
void bls12_381_Fp2_mont_set_const ( const uint64_t *src1, uint64_t *tgt ) {
  bls12_381_Fp_mont_copy( src1, tgt );
  for(int k=1; k<EXT_DEGREE; k++) {
    bls12_381_Fp_mont_set_zero( TGT(k) );
  }
}

void bls12_381_Fp2_mont_copy ( const uint64_t *src1 , uint64_t *tgt ) {
  memcpy( tgt, src1, 8*EXT_NWORDS );
  // for(int k=0; k<EXT_DEGREE; k++) {
  //   bls12_381_Fp_mont_copy( SRC1(k) , TGT(k) );
  // }
}


void bls12_381_Fp2_mont_neg ( const uint64_t *src1, uint64_t *tgt ) {
  for(int k=0; k<EXT_DEGREE; k++) {
    bls12_381_Fp_mont_neg( SRC1(k) , TGT(k) );
  }
}

void bls12_381_Fp2_mont_add ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  for(int k=0; k<EXT_DEGREE; k++) {
    bls12_381_Fp_mont_add( SRC1(k) , SRC2(k) , TGT(k) );
  }
}

void bls12_381_Fp2_mont_sub ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  for(int k=0; k<EXT_DEGREE; k++) {
    bls12_381_Fp_mont_sub( SRC1(k) , SRC2(k) , TGT(k) );
  }
}

//--------------------------------------

void bls12_381_Fp2_mont_neg_inplace ( uint64_t *tgt ) {
  for(int k=0; k<EXT_DEGREE; k++) {
    bls12_381_Fp_mont_neg_inplace( TGT(k) );
  }
}
void bls12_381_Fp2_mont_add_inplace ( uint64_t *tgt , const uint64_t *src2 ) {
  for(int k=0; k<EXT_DEGREE; k++) {
    bls12_381_Fp_mont_add_inplace( TGT(k) , SRC2(k) );
  }
}

void bls12_381_Fp2_mont_sub_inplace ( uint64_t *tgt , const uint64_t *src2 ) {
  for(int k=0; k<EXT_DEGREE; k++) {
    bls12_381_Fp_mont_sub_inplace( TGT(k) , SRC2(k) );
  }
}

void bls12_381_Fp2_mont_sub_inplace_reverse ( uint64_t *tgt , const uint64_t *src1 ) {
  for(int k=0; k<EXT_DEGREE; k++) {
    bls12_381_Fp_mont_sub_inplace_reverse( TGT(k) , SRC1(k) );
  }
}


void bls12_381_Fp2_mont_mul ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  uint64_t p[BASE_NWORDS];
  uint64_t q[BASE_NWORDS];
  uint64_t r[BASE_NWORDS];
  uint64_t tmp[BASE_NWORDS];
  bls12_381_Fp_mont_mul( SRC1(0) , SRC2(0) , p );
  bls12_381_Fp_mont_mul( SRC1(0) , SRC2(1) , q );
  bls12_381_Fp_mont_mul( SRC1(1) , SRC2(0) , tmp );
  bls12_381_Fp_mont_add_inplace( q , tmp );
  bls12_381_Fp_mont_mul( SRC1(1) , SRC2(1) , r );
  bls12_381_Fp_mont_sub(  p , r , TGT(0) );
  bls12_381_Fp_mont_copy( q ,     TGT(1) );
}

void bls12_381_Fp2_mont_sqr ( const uint64_t *src1, uint64_t *tgt ) {
  uint64_t p[BASE_NWORDS];
  uint64_t q[BASE_NWORDS];
  uint64_t r[BASE_NWORDS];
  uint64_t tmp[BASE_NWORDS];
  bls12_381_Fp_mont_sqr( SRC1(0) , p );
  bls12_381_Fp_mont_mul( SRC1(0) , SRC1(1) , q );
  bls12_381_Fp_mont_add_inplace( q , q );
  bls12_381_Fp_mont_sqr( SRC1(1) , r );
  bls12_381_Fp_mont_sub(  p , r , TGT(0) );
  bls12_381_Fp_mont_copy( q ,     TGT(1) );
}


void bls12_381_Fp2_mont_mul_inplace ( uint64_t *tgt , const uint64_t *src2 ) {
  bls12_381_Fp2_mont_mul( tgt, src2, tgt );
}

void bls12_381_Fp2_mont_sqr_inplace ( uint64_t *tgt ) {
  bls12_381_Fp2_mont_sqr( tgt, tgt );
}

void bls12_381_Fp2_mont_inv ( const uint64_t *src1, uint64_t *tgt ) {
  uint64_t denom     [BASE_NWORDS];
  uint64_t b_minus_ap[BASE_NWORDS];
  uint64_t tmp       [BASE_NWORDS];
  bls12_381_Fp_mont_copy( SRC1(0) , b_minus_ap );                   // b-a*p = b (because p=0)
  bls12_381_Fp_mont_sqr( SRC1(1) , denom );                         // a^2
  bls12_381_Fp_mont_mul( SRC1(0) , b_minus_ap , tmp );              // b*(b-a*p) = b^2-a*b*p
  bls12_381_Fp_mont_add_inplace( denom , tmp );                     // (b^2 - a*b*p) + a^2*q
  bls12_381_Fp_mont_inv_inplace( denom );                           // 1/(b^2 - a*b*p - a^2*q)
  bls12_381_Fp_mont_neg( SRC1(1) , tmp );                           // -a
  bls12_381_Fp_mont_mul( tmp        , denom , TGT(1) );             // c := -a/denom
  bls12_381_Fp_mont_mul( b_minus_ap , denom, TGT(0) );              // d := (b-a*p)/denom
}

void bls12_381_Fp2_mont_inv_inplace ( uint64_t *tgt ) {
  bls12_381_Fp2_mont_inv ( tgt , tgt ); 
}

void bls12_381_Fp2_mont_div ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  uint64_t inv[EXT_NWORDS];
  bls12_381_Fp2_mont_inv ( src2 , inv );
  bls12_381_Fp2_mont_mul ( src1 , inv , tgt );
}

void bls12_381_Fp2_mont_div_inplace ( uint64_t *tgt, const uint64_t *src2 ) {
  bls12_381_Fp2_mont_div ( tgt , src2 , tgt ); 
}

// computes `x^e mod p`
void bls12_381_Fp2_mont_pow_uint64( const uint64_t *src, uint64_t exponent, uint64_t *tgt ) {
  uint64_t e = exponent;
  uint64_t sqr[12];
  bls12_381_Fp2_mont_copy( src, sqr );                 // sqr := src
  bls12_381_Fp2_mont_set_one( tgt );                   // tgt := 1
  while(e!=0) {
    if (e & 1) { bls12_381_Fp2_mont_mul_inplace(tgt, sqr); }
    bls12_381_Fp2_mont_mul_inplace(sqr, sqr);
    e = e >> 1;
  }
}

// computes `x^e mod p` (for `e` non-negative bigint)
void bls12_381_Fp2_mont_pow_gen( const uint64_t *src, const uint64_t *expo, uint64_t *tgt, int expo_len ) {
  uint64_t sqr[12];
  bls12_381_Fp2_mont_copy( src, sqr );                 // sqr := src
  bls12_381_Fp2_mont_set_one( tgt );                   // tgt := 1
  int s = expo_len - 1;
  while ((expo[s] == 0) && (s>0)) { s--; }          // skip the unneeded largest powers
  for(int i=0; i<=s; i++) {
    uint64_t e = expo[i];
    for(int j=0; j<64; j++) {
      if (e & 1) { bls12_381_Fp2_mont_mul_inplace(tgt, sqr); }
      bls12_381_Fp2_mont_mul_inplace(sqr, sqr);
      e = e >> 1;
    }
  }
}

#define I_SRC(i)   (src    + (i)*12)
#define I_TGT(i)   (tgt    + (i)*12)
#define I_PROD(i)  (prods  + (i)*12)
#define I_RECIP(i) (recips + (i)*12)

// computes the inverse of many field elements at the same time, efficiently
// uses the Montgomery batch inversion trick
// inverse of a field element
void bls12_381_Fp2_mont_batch_inv( int n, const uint64_t *src, uint64_t *tgt ) {
  assert( n >= 1 );
  uint64_t *prods  = malloc( 8*12*n );
  uint64_t *recips = malloc( 8*12*n );
  assert( prods  != 0 );
  assert( recips != 0 );
  
  // compute partial products (a[0]*a[1]*...*a[k]) for all k
  bls12_381_Fp2_mont_copy( I_SRC(0) , I_PROD(0) );
  for(int i=1; i<n; i++) {
    bls12_381_Fp2_mont_mul( I_PROD(i-1) , I_SRC(i) , I_PROD(i) );
  }
  
  // compute inverses of partial products 1/(a[0]*a[1]*...*a[k]) for all k
  bls12_381_Fp2_mont_inv( I_PROD(n-1) , I_RECIP(n-1) );
  for(int i=n-2; i>=0; i--) {
    bls12_381_Fp2_mont_mul( I_RECIP(i+1) , I_SRC(i+1) , I_RECIP(i) );
  }
  
  // compute the inverses 1/a[k] for all k
  bls12_381_Fp2_mont_copy( I_RECIP(0) , I_TGT(0) );
  for(int i=1; i<n; i++) {
    bls12_381_Fp2_mont_mul( I_RECIP(i) , I_PROD(i-1) , I_TGT(i) );
  }
  
  free(recips);
  free(prods);
}
