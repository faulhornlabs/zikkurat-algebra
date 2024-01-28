
// extension field `BN128/Fp2`
//
// NOTE: generated code, do not edit!

#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>

#include "bn128_Fp2_mont.h"
#include "bn128_Fp_mont.h"


#define PRIME_NWORDS 4
#define BASE_NWORDS  4
#define EXT_NWORDS   8

#define EXT_DEGREE   2
#define PRIME_DEGREE 2

#define SRC1(i) ((src1) + (i)*BASE_NWORDS)
#define SRC2(i) ((src2) + (i)*BASE_NWORDS)
#define TGT(i)  (( tgt) + (i)*BASE_NWORDS)
#define PROD(i) ((prod) + (i)*BASE_NWORDS)
#define IRRED(i) (bn128_Fp2_mont_irred_coeffs + (i)*BASE_NWORDS)

#define MIN(a,b) ( ((a)<=(b)) ? (a) : (b) )
#define MAX(a,b) ( ((a)>=(b)) ? (a) : (b) )

uint64_t bn128_Fp2_mont_irred_coeffs[8] =
  { 0xd35d438dc58f0d9d, 0x0a78eb28f5c70b3d, 0x666ea36f7879462c, 0x0e0a77c19a07df2f
  , 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000
  };


//------------------------------------------------------------------------------


void bn128_Fp2_mont_from_base_field ( const uint64_t *src , uint64_t *tgt ) {
  bn128_Fp2_mont_set_zero( tgt );
  bn128_Fp_mont_copy( src, tgt );
}

void bn128_Fp2_mont_from_prime_field( const uint64_t *src , uint64_t *tgt ) {
  bn128_Fp2_mont_set_zero( tgt );
  bn128_Fp_mont_copy( src, tgt );
}

void bn128_Fp2_mont_scale_by_base_field ( const uint64_t *coeff , const uint64_t *src1, uint64_t *tgt ) {
  for(int i=0; i<EXT_DEGREE; i++) {
    bn128_Fp_mont_mul( coeff, SRC1(i), TGT(i) );
  }
}

void bn128_Fp2_mont_scale_by_prime_field( const uint64_t *coeff , const uint64_t *src1, uint64_t *tgt ) {
  for(int i=0; i<PRIME_DEGREE; i++) {
    bn128_Fp_mont_mul( coeff , src1 + i*PRIME_NWORDS , tgt + i*PRIME_NWORDS );
  }
}


uint8_t bn128_Fp2_mont_is_valid ( const uint64_t *src1 ) {
  uint8_t ok = 1;
  for(int k=0; k<EXT_DEGREE; k++) {
    if (!bn128_Fp_mont_is_valid( SRC1(k) )) { ok=0; break; }
  }
  return ok;
}

uint8_t bn128_Fp2_mont_is_zero  ( const uint64_t *src1 ) {
  uint8_t ok = 1;
  for(int k=0; k<EXT_DEGREE; k++) {
    if (!bn128_Fp_mont_is_zero( SRC1(k) )) { ok=0; break; }
  }
  return ok;
}

uint8_t bn128_Fp2_mont_is_one ( const uint64_t *src1 ) {
  uint8_t ok = bn128_Fp_mont_is_one( SRC1(0) );
  if (!ok) { return ok; }
  for(int k=1; k<EXT_DEGREE; k++) {
    if (!bn128_Fp_mont_is_zero( SRC1(k) )) { ok=0; break; }
  }
  return ok;
}

uint8_t bn128_Fp2_mont_is_equal ( const uint64_t *src1, const uint64_t *src2 ) {
  uint8_t ok = 1;
  for(int k=0; k<EXT_DEGREE; k++) {
    if (!bn128_Fp_mont_is_equal( SRC1(k) , SRC2(k) )) { ok=0; break; }
  }
  return ok;
}

void bn128_Fp2_mont_set_zero ( uint64_t *tgt ) {
  for(int k=0; k<EXT_DEGREE; k++) {
    bn128_Fp_mont_set_zero( TGT(k) );
  }
}

void bn128_Fp2_mont_set_one ( uint64_t *tgt ) {
  bn128_Fp_mont_set_one( tgt );
  for(int k=1; k<EXT_DEGREE; k++) {
    bn128_Fp_mont_set_zero( TGT(k) );
  }
}

// src: base field
// tgt: extension field
void bn128_Fp2_mont_set_const ( const uint64_t *src1, uint64_t *tgt ) {
  bn128_Fp_mont_copy( src1, tgt );
  for(int k=1; k<EXT_DEGREE; k++) {
    bn128_Fp_mont_set_zero( TGT(k) );
  }
}

void bn128_Fp2_mont_copy ( const uint64_t *src1 , uint64_t *tgt ) {
  memcpy( tgt, src1, 8*EXT_NWORDS );
  // for(int k=0; k<EXT_DEGREE; k++) {
  //   bn128_Fp_mont_copy( SRC1(k) , TGT(k) );
  // }
}


void bn128_Fp2_mont_neg ( const uint64_t *src1, uint64_t *tgt ) {
  for(int k=0; k<EXT_DEGREE; k++) {
    bn128_Fp_mont_neg( SRC1(k) , TGT(k) );
  }
}

void bn128_Fp2_mont_add ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  for(int k=0; k<EXT_DEGREE; k++) {
    bn128_Fp_mont_add( SRC1(k) , SRC2(k) , TGT(k) );
  }
}

void bn128_Fp2_mont_sub ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  for(int k=0; k<EXT_DEGREE; k++) {
    bn128_Fp_mont_sub( SRC1(k) , SRC2(k) , TGT(k) );
  }
}

//--------------------------------------

void bn128_Fp2_mont_neg_inplace ( uint64_t *tgt ) {
  for(int k=0; k<EXT_DEGREE; k++) {
    bn128_Fp_mont_neg_inplace( TGT(k) );
  }
}
void bn128_Fp2_mont_add_inplace ( uint64_t *tgt , const uint64_t *src2 ) {
  for(int k=0; k<EXT_DEGREE; k++) {
    bn128_Fp_mont_add_inplace( TGT(k) , SRC2(k) );
  }
}

void bn128_Fp2_mont_sub_inplace ( uint64_t *tgt , const uint64_t *src2 ) {
  for(int k=0; k<EXT_DEGREE; k++) {
    bn128_Fp_mont_sub_inplace( TGT(k) , SRC2(k) );
  }
}

void bn128_Fp2_mont_sub_inplace_reverse ( uint64_t *tgt , const uint64_t *src1 ) {
  for(int k=0; k<EXT_DEGREE; k++) {
    bn128_Fp_mont_sub_inplace_reverse( TGT(k) , SRC1(k) );
  }
}


// we use Karatsuba trick to have only 3 multiplications
void bn128_Fp2_mont_mul ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  uint64_t p[BASE_NWORDS];
  uint64_t q[BASE_NWORDS];
  uint64_t r[BASE_NWORDS];
  uint64_t tmp[BASE_NWORDS];
  bn128_Fp_mont_mul( SRC1(0) , SRC2(0) , p );         // a0*b0
  bn128_Fp_mont_mul( SRC1(1) , SRC2(1) , r );         // a1*b1
  bn128_Fp_mont_add( SRC1(0) , SRC1(1) , q );         // (a0+a1)
  bn128_Fp_mont_add( SRC2(0) , SRC2(1) , tmp );       // (b0+b1)
  bn128_Fp_mont_mul_inplace( q , tmp );               // (a0+a1)*(b0+b1)
  bn128_Fp_mont_sub_inplace( q , p );
  bn128_Fp_mont_sub_inplace( q , r );
  bn128_Fp_mont_sub(  p , r , TGT(0) );
  bn128_Fp_mont_copy( q ,     TGT(1) );
}

// we use Karatsuba trick to have only 3 squarings
void bn128_Fp2_mont_sqr ( const uint64_t *src1, uint64_t *tgt ) {
  uint64_t p[BASE_NWORDS];
  uint64_t q[BASE_NWORDS];
  uint64_t r[BASE_NWORDS];
  uint64_t tmp[BASE_NWORDS];
  bn128_Fp_mont_sqr( SRC1(0) , p );              // a0^2
  bn128_Fp_mont_sqr( SRC1(1) , r );              // a1^2
  bn128_Fp_mont_add( SRC1(0) , SRC1(1) , q );    // (a0+a1)
  bn128_Fp_mont_sqr_inplace( q );                // (a0+a1)^2
  bn128_Fp_mont_sub_inplace( q , p );
  bn128_Fp_mont_sub_inplace( q , r );
  bn128_Fp_mont_sub(  p , r , TGT(0) );
  bn128_Fp_mont_copy( q ,     TGT(1) );
}


void bn128_Fp2_mont_mul_inplace ( uint64_t *tgt , const uint64_t *src2 ) {
  bn128_Fp2_mont_mul( tgt, src2, tgt );
}

void bn128_Fp2_mont_sqr_inplace ( uint64_t *tgt ) {
  bn128_Fp2_mont_sqr( tgt, tgt );
}

void bn128_Fp2_mont_inv ( const uint64_t *src1, uint64_t *tgt ) {
  uint64_t denom     [BASE_NWORDS];
  uint64_t b_minus_ap[BASE_NWORDS];
  uint64_t tmp       [BASE_NWORDS];
  bn128_Fp_mont_copy( SRC1(0) , b_minus_ap );                   // b-a*p = b (because p=0)
  bn128_Fp_mont_sqr( SRC1(1) , denom );                         // a^2
  bn128_Fp_mont_mul( SRC1(0) , b_minus_ap , tmp );              // b*(b-a*p) = b^2-a*b*p
  bn128_Fp_mont_add_inplace( denom , tmp );                     // (b^2 - a*b*p) + a^2*q
  bn128_Fp_mont_inv_inplace( denom );                           // 1/(b^2 - a*b*p - a^2*q)
  bn128_Fp_mont_neg( SRC1(1) , tmp );                           // -a
  bn128_Fp_mont_mul( tmp        , denom , TGT(1) );             // c := -a/denom
  bn128_Fp_mont_mul( b_minus_ap , denom, TGT(0) );              // d := (b-a*p)/denom
}

void bn128_Fp2_mont_inv_inplace ( uint64_t *tgt ) {
  bn128_Fp2_mont_inv ( tgt , tgt ); 
}

void bn128_Fp2_mont_div ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  uint64_t inv[EXT_NWORDS];
  bn128_Fp2_mont_inv ( src2 , inv );
  bn128_Fp2_mont_mul ( src1 , inv , tgt );
}

void bn128_Fp2_mont_div_inplace ( uint64_t *tgt, const uint64_t *src2 ) {
  bn128_Fp2_mont_div ( tgt , src2 , tgt ); 
}


const uint64_t bn128_Fp2_mont_frobenius_basis[16] = 
  { 0xd35d438dc58f0d9d, 0x0a78eb28f5c70b3d, 0x666ea36f7879462c, 0x0e0a77c19a07df2f, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000
  , 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x68c3488912edefaa, 0x8d087f6872aabf4f, 0x51e1a24709081231, 0x2259d6b14729c0fa
  };


void bn128_Fp2_mont_frobenius ( const uint64_t *src, uint64_t *tgt ) {
  uint64_t acc[EXT_NWORDS];
  uint64_t tmp[EXT_NWORDS];
  bn128_Fp2_mont_set_zero(acc);
  for(int i=0; i<PRIME_DEGREE; i++) {
    bn128_Fp2_mont_scale_by_prime_field( src + i*PRIME_NWORDS , bn128_Fp2_mont_frobenius_basis + i*EXT_NWORDS , tmp );
    bn128_Fp2_mont_add_inplace( acc, tmp );
  }
  bn128_Fp2_mont_copy( acc, tgt );
}

void bn128_Fp2_mont_frobenius_inplace ( uint64_t *tgt ) {
  bn128_Fp2_mont_frobenius( tgt, tgt );
}

// computes `x^e mod p`
void bn128_Fp2_mont_pow_uint64( const uint64_t *src, uint64_t exponent, uint64_t *tgt ) {
  uint64_t e = exponent;
  uint64_t sqr[8];
  bn128_Fp2_mont_copy( src, sqr );                 // sqr := src
  bn128_Fp2_mont_set_one( tgt );                   // tgt := 1
  while(e!=0) {
    if (e & 1) { bn128_Fp2_mont_mul_inplace(tgt, sqr); }
    bn128_Fp2_mont_mul_inplace(sqr, sqr);
    e = e >> 1;
  }
}

// computes `x^e mod p` (for `e` non-negative bigint)
void bn128_Fp2_mont_pow_gen( const uint64_t *src, const uint64_t *expo, uint64_t *tgt, int expo_len ) {
  uint64_t sqr[8];
  bn128_Fp2_mont_copy( src, sqr );                 // sqr := src
  bn128_Fp2_mont_set_one( tgt );                   // tgt := 1
  int s = expo_len - 1;
  while ((expo[s] == 0) && (s>0)) { s--; }          // skip the unneeded largest powers
  for(int i=0; i<=s; i++) {
    uint64_t e = expo[i];
    for(int j=0; j<64; j++) {
      if (e & 1) { bn128_Fp2_mont_mul_inplace(tgt, sqr); }
      bn128_Fp2_mont_mul_inplace(sqr, sqr);
      e = e >> 1;
    }
  }
}

#define I_SRC(i)   (src    + (i)*8)
#define I_TGT(i)   (tgt    + (i)*8)
#define I_PROD(i)  (prods  + (i)*8)
#define I_RECIP(i) (recips + (i)*8)

// computes the inverse of many field elements at the same time, efficiently
// uses the Montgomery batch inversion trick
void bn128_Fp2_mont_batch_inv( int n, const uint64_t *src, uint64_t *tgt ) {
  assert( n >= 1 );
  uint64_t *prods  = malloc( 8*8*n );
  uint64_t *recips = malloc( 8*8*n );
  assert( prods  != 0 );
  assert( recips != 0 );
  
  // compute partial products (a[0]*a[1]*...*a[k]) for all k
  bn128_Fp2_mont_copy( I_SRC(0) , I_PROD(0) );
  for(int i=1; i<n; i++) {
    bn128_Fp2_mont_mul( I_PROD(i-1) , I_SRC(i) , I_PROD(i) );
  }
  
  // compute inverses of partial products 1/(a[0]*a[1]*...*a[k]) for all k
  bn128_Fp2_mont_inv( I_PROD(n-1) , I_RECIP(n-1) );
  for(int i=n-2; i>=0; i--) {
    bn128_Fp2_mont_mul( I_RECIP(i+1) , I_SRC(i+1) , I_RECIP(i) );
  }
  
  // compute the inverses 1/a[k] for all k
  bn128_Fp2_mont_copy( I_RECIP(0) , I_TGT(0) );
  for(int i=1; i<n; i++) {
    bn128_Fp2_mont_mul( I_RECIP(i) , I_PROD(i-1) , I_TGT(i) );
  }
  
  free(recips);
  free(prods);
}
