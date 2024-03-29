
// extension field `BN128/Fp6`
//
// NOTE: generated code, do not edit!

#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>

#include "bn128_Fp6_mont.h"
#include "bn128_Fp2_mont.h"
#include "bn128_Fp_mont.h"

#define PRIME_NWORDS 4
#define BASE_NWORDS  8
#define EXT_NWORDS   24

#define EXT_DEGREE   3
#define PRIME_DEGREE 6

#define SRC1(i) ((src1) + (i)*BASE_NWORDS)
#define SRC2(i) ((src2) + (i)*BASE_NWORDS)
#define TGT(i)  (( tgt) + (i)*BASE_NWORDS)
#define PROD(i) ((prod) + (i)*BASE_NWORDS)
#define IRRED(i) (bn128_Fp6_mont_irred_coeffs + (i)*BASE_NWORDS)

#define MIN(a,b) ( ((a)<=(b)) ? (a) : (b) )
#define MAX(a,b) ( ((a)>=(b)) ? (a) : (b) )

uint64_t bn128_Fp6_mont_irred_coeffs[24] =
  { 0x461a4448976f7d50, 0x6843fb439555fa7b, 0x8f0d12384840918c, 0x12ceb58a394e07d2
  , 0x68c3488912edefaa, 0x8d087f6872aabf4f, 0x51e1a24709081231, 0x2259d6b14729c0fa
  , 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000
  , 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000
  , 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000
  , 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000
  };


//------------------------------------------------------------------------------


void bn128_Fp6_mont_from_base_field ( const uint64_t *src , uint64_t *tgt ) {
  bn128_Fp6_mont_set_zero( tgt );
  bn128_Fp2_mont_copy( src, tgt );
}

void bn128_Fp6_mont_from_prime_field( const uint64_t *src , uint64_t *tgt ) {
  bn128_Fp6_mont_set_zero( tgt );
  bn128_Fp_mont_copy( src, tgt );
}

void bn128_Fp6_mont_scale_by_base_field ( const uint64_t *coeff , const uint64_t *src1, uint64_t *tgt ) {
  for(int i=0; i<EXT_DEGREE; i++) {
    bn128_Fp2_mont_mul( coeff, SRC1(i), TGT(i) );
  }
}

void bn128_Fp6_mont_scale_by_prime_field( const uint64_t *coeff , const uint64_t *src1, uint64_t *tgt ) {
  for(int i=0; i<PRIME_DEGREE; i++) {
    bn128_Fp_mont_mul( coeff , src1 + i*PRIME_NWORDS , tgt + i*PRIME_NWORDS );
  }
}

void bn128_Fp6_mont_div_by_2( const uint64_t *src1, uint64_t *tgt ) {
  for(int i=0; i<PRIME_DEGREE; i++) {
    bn128_Fp_mont_div_by_2( src1 + i*PRIME_NWORDS , tgt + i*PRIME_NWORDS );
  }
}

void bn128_Fp6_mont_div_by_2_inplace(  uint64_t *tgt ) {
  for(int i=0; i<PRIME_DEGREE; i++) {
    bn128_Fp_mont_div_by_2_inplace( tgt + i*PRIME_NWORDS );
  }
}

uint8_t bn128_Fp6_mont_is_valid ( const uint64_t *src1 ) {
  uint8_t ok = 1;
  for(int k=0; k<EXT_DEGREE; k++) {
    if (!bn128_Fp2_mont_is_valid( SRC1(k) )) { ok=0; break; }
  }
  return ok;
}

uint8_t bn128_Fp6_mont_is_zero  ( const uint64_t *src1 ) {
  uint8_t ok = 1;
  for(int k=0; k<EXT_DEGREE; k++) {
    if (!bn128_Fp2_mont_is_zero( SRC1(k) )) { ok=0; break; }
  }
  return ok;
}

uint8_t bn128_Fp6_mont_is_one ( const uint64_t *src1 ) {
  uint8_t ok = bn128_Fp2_mont_is_one( SRC1(0) );
  if (!ok) { return ok; }
  for(int k=1; k<EXT_DEGREE; k++) {
    if (!bn128_Fp2_mont_is_zero( SRC1(k) )) { ok=0; break; }
  }
  return ok;
}

uint8_t bn128_Fp6_mont_is_equal ( const uint64_t *src1, const uint64_t *src2 ) {
  uint8_t ok = 1;
  for(int k=0; k<EXT_DEGREE; k++) {
    if (!bn128_Fp2_mont_is_equal( SRC1(k) , SRC2(k) )) { ok=0; break; }
  }
  return ok;
}

void bn128_Fp6_mont_set_zero ( uint64_t *tgt ) {
  for(int k=0; k<EXT_DEGREE; k++) {
    bn128_Fp2_mont_set_zero( TGT(k) );
  }
}

void bn128_Fp6_mont_set_one ( uint64_t *tgt ) {
  bn128_Fp2_mont_set_one( tgt );
  for(int k=1; k<EXT_DEGREE; k++) {
    bn128_Fp2_mont_set_zero( TGT(k) );
  }
}

// src: base field
// tgt: extension field
void bn128_Fp6_mont_set_const ( const uint64_t *src1, uint64_t *tgt ) {
  bn128_Fp2_mont_copy( src1, tgt );
  for(int k=1; k<EXT_DEGREE; k++) {
    bn128_Fp2_mont_set_zero( TGT(k) );
  }
}

void bn128_Fp6_mont_copy ( const uint64_t *src1 , uint64_t *tgt ) {
  memcpy( tgt, src1, 8*EXT_NWORDS );
  // for(int k=0; k<EXT_DEGREE; k++) {
  //   bn128_Fp2_mont_copy( SRC1(k) , TGT(k) );
  // }
}


void bn128_Fp6_mont_neg ( const uint64_t *src1, uint64_t *tgt ) {
  for(int k=0; k<EXT_DEGREE; k++) {
    bn128_Fp2_mont_neg( SRC1(k) , TGT(k) );
  }
}

void bn128_Fp6_mont_add ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  for(int k=0; k<EXT_DEGREE; k++) {
    bn128_Fp2_mont_add( SRC1(k) , SRC2(k) , TGT(k) );
  }
}

void bn128_Fp6_mont_sub ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  for(int k=0; k<EXT_DEGREE; k++) {
    bn128_Fp2_mont_sub( SRC1(k) , SRC2(k) , TGT(k) );
  }
}

//--------------------------------------

void bn128_Fp6_mont_neg_inplace ( uint64_t *tgt ) {
  for(int k=0; k<EXT_DEGREE; k++) {
    bn128_Fp2_mont_neg_inplace( TGT(k) );
  }
}
void bn128_Fp6_mont_add_inplace ( uint64_t *tgt , const uint64_t *src2 ) {
  for(int k=0; k<EXT_DEGREE; k++) {
    bn128_Fp2_mont_add_inplace( TGT(k) , SRC2(k) );
  }
}

void bn128_Fp6_mont_sub_inplace ( uint64_t *tgt , const uint64_t *src2 ) {
  for(int k=0; k<EXT_DEGREE; k++) {
    bn128_Fp2_mont_sub_inplace( TGT(k) , SRC2(k) );
  }
}

void bn128_Fp6_mont_sub_inplace_reverse ( uint64_t *tgt , const uint64_t *src1 ) {
  for(int k=0; k<EXT_DEGREE; k++) {
    bn128_Fp2_mont_sub_inplace_reverse( TGT(k) , SRC1(k) );
  }
}


void bn128_Fp6_mont_subtract_irred_generic( const uint64_t *scalar, uint64_t *tgt ) {
  for(int k=0; k<3; k++) {
    uint64_t tmp[BASE_NWORDS];
    bn128_Fp2_mont_mul( scalar , IRRED(k) , tmp );
    bn128_Fp2_mont_sub_inplace( TGT(k) , tmp );
  }
}

// we use Karatsuba trick to have only 6 multiplications
void bn128_Fp6_mont_mul ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  uint64_t prod[5*BASE_NWORDS];
  uint64_t tmp [  BASE_NWORDS];
  uint64_t tmp2[  BASE_NWORDS];
  bn128_Fp2_mont_mul( SRC1(0) , SRC2(0) , PROD(0) );      // p = a0 * b0
  bn128_Fp2_mont_mul( SRC1(1) , SRC2(1) , PROD(2) );      // q = a1 * b1
  bn128_Fp2_mont_mul( SRC1(2) , SRC2(2) , PROD(4) );      // r = a2 * b2

  bn128_Fp2_mont_add( SRC1(0) , SRC1(1) , PROD(1) );      // (a0+a1)
  bn128_Fp2_mont_add( SRC2(0) , SRC2(1) , tmp );          // (b0+b1)
  bn128_Fp2_mont_mul_inplace( PROD(1) , tmp );            // (a0+a1)*(b0+b1)
  bn128_Fp2_mont_sub_inplace( PROD(1) , PROD(0) );
  bn128_Fp2_mont_sub_inplace( PROD(1) , PROD(2) );        // a0*b1 + a1*b0

  bn128_Fp2_mont_add( SRC1(1) , SRC1(2) , PROD(3) );      // (a1+a2)
  bn128_Fp2_mont_add( SRC2(1) , SRC2(2) , tmp );          // (b1+b2)
  bn128_Fp2_mont_mul_inplace( PROD(3) , tmp );            // (a1+a2)*(b1+b2)
  bn128_Fp2_mont_sub_inplace( PROD(3) , PROD(2) );
  bn128_Fp2_mont_sub_inplace( PROD(3) , PROD(4) );        // a0*b1 + a1*b0

  bn128_Fp2_mont_add( SRC1(0) , SRC1(2) , tmp2 );         // (a0+a2)
  bn128_Fp2_mont_add( SRC2(0) , SRC2(2) , tmp  );         // (b0+b2)
  bn128_Fp2_mont_mul_inplace( tmp2 , tmp );               // (a0+a2)*(b0+b2)
  bn128_Fp2_mont_sub_inplace( tmp2 , PROD(0) );
  bn128_Fp2_mont_sub_inplace( tmp2 , PROD(4) );           // a0*b2 + a2*b0
  bn128_Fp2_mont_add_inplace( PROD(2) , tmp2 );           // a0*b2 + a1*b1 + a2*b0
  bn128_Fp6_mont_subtract_irred_generic( PROD(4) , PROD(1) );
  bn128_Fp6_mont_subtract_irred_generic( PROD(3) , PROD(0) );
  memcpy( tgt, prod, 8*EXT_NWORDS );
}

// we use Karatsuba trick to have only 6 squarings
void bn128_Fp6_mont_sqr ( const uint64_t *src1, uint64_t *tgt ) {
  uint64_t prod[5*BASE_NWORDS];
  uint64_t tmp [  BASE_NWORDS];
  bn128_Fp2_mont_sqr( SRC1(0) , PROD(0) );                // p = a0^2
  bn128_Fp2_mont_sqr( SRC1(1) , PROD(2) );                // q = a1^2
  bn128_Fp2_mont_sqr( SRC1(2) , PROD(4) );                // r = a2^2

  bn128_Fp2_mont_add( SRC1(0) , SRC1(1) , PROD(1) );      // (a0+a1)
  bn128_Fp2_mont_sqr_inplace( PROD(1) );                  // (a0+a1)^2
  bn128_Fp2_mont_sub_inplace( PROD(1) , PROD(0) );
  bn128_Fp2_mont_sub_inplace( PROD(1) , PROD(2) );        // a0*a1 + a1*a0

  bn128_Fp2_mont_add( SRC1(1) , SRC1(2) , PROD(3) );      // (a1+a2)
  bn128_Fp2_mont_sqr_inplace( PROD(3) );                  // (a1+a2)^2
  bn128_Fp2_mont_sub_inplace( PROD(3) , PROD(2) );
  bn128_Fp2_mont_sub_inplace( PROD(3) , PROD(4) );        // a1*a2 + a2*a1

  bn128_Fp2_mont_add( SRC1(0) , SRC1(2) , tmp  );         // (a0+a2)
  bn128_Fp2_mont_sqr_inplace( tmp   );                    // (a0+a2)^2
  bn128_Fp2_mont_sub_inplace( tmp , PROD(0) );
  bn128_Fp2_mont_sub_inplace( tmp , PROD(4) );            // a0*a2 + a2*a0
  bn128_Fp2_mont_add_inplace( PROD(2) , tmp );            // a0*a2 + a1*a1 + a2*a0
  bn128_Fp6_mont_subtract_irred_generic( PROD(4) , PROD(1) );
  bn128_Fp6_mont_subtract_irred_generic( PROD(3) , PROD(0) );
  memcpy( tgt, prod, 8*EXT_NWORDS );
}


void bn128_Fp6_mont_mul_inplace ( uint64_t *tgt , const uint64_t *src2 ) {
  bn128_Fp6_mont_mul( tgt, src2, tgt );
}

void bn128_Fp6_mont_sqr_inplace ( uint64_t *tgt ) {
  bn128_Fp6_mont_sqr( tgt, tgt );
}

void bn128_Fp6_mont_inv ( const uint64_t *src1, uint64_t *tgt ) {
  uint64_t denom     [BASE_NWORDS];
  uint64_t a0a0[BASE_NWORDS];
  uint64_t a1a1[BASE_NWORDS];
  uint64_t a2a2[BASE_NWORDS];
  uint64_t a0a1[BASE_NWORDS];
  uint64_t a0a2[BASE_NWORDS];
  uint64_t a1a2[BASE_NWORDS];
  uint64_t a1a1a1[BASE_NWORDS];
  uint64_t a1a2p0[BASE_NWORDS];
  uint64_t a2a2p0[BASE_NWORDS];
  uint64_t tmp[BASE_NWORDS];
  bn128_Fp2_mont_sqr( SRC1(0) , a0a0 );                // a0^2
  bn128_Fp2_mont_sqr( SRC1(1) , a1a1 );                // a1^2
  bn128_Fp2_mont_sqr( SRC1(2) , a2a2 );                // a2^2
  bn128_Fp2_mont_mul( SRC1(0) , SRC1(1) , a0a1 );      // a0*a1
  bn128_Fp2_mont_mul( SRC1(0) , SRC1(2) , a0a2 );      // a0*a2
  bn128_Fp2_mont_mul( SRC1(1) , SRC1(2) , a1a2 );      // a1*a2
  bn128_Fp2_mont_mul( a1a1 , SRC1(1)  , a1a1a1 );      // a1^3
  bn128_Fp2_mont_mul( a1a2 , IRRED(0) , a1a2p0 );      // a1*a2*p0
  bn128_Fp2_mont_mul( a2a2 , IRRED(0) , a2a2p0 );      // a2^2*p0
  // --- 
  bn128_Fp2_mont_add( a1a2p0 , a1a2p0 , denom );       // 2*a1*a2*p0
  bn128_Fp2_mont_add_inplace( denom , a1a2p0  );       // 3*a1*a2*p0
  bn128_Fp2_mont_add_inplace( denom , a0a0    );       // a0^2 + 3*a1*a2*p0
  bn128_Fp2_mont_mul_inplace( denom , SRC1(0) );       // a0^3 + 3*a0*a1*a2*p0
  bn128_Fp2_mont_mul( a2a2p0 , SRC1(2) , tmp );        // a2^3*p0
  bn128_Fp2_mont_sub_inplace( tmp , a1a1a1 );          // a2^3*p0 - a1^3
  bn128_Fp2_mont_mul_inplace( tmp , IRRED(0) );        // a2^3*p0^2 - a1^3*p0
  bn128_Fp2_mont_add_inplace( denom, tmp );
  // --- 
  bn128_Fp2_mont_inv_inplace( denom );
  bn128_Fp2_mont_add( a0a0 , a1a2p0 , TGT(0) );         // a0*a0 + a1*a2*p0
  bn128_Fp2_mont_add( a0a1 , a2a2p0 , TGT(1) );         // a0*a1 + a2^2*p0
  bn128_Fp2_mont_neg_inplace( TGT(1) );                 // -(a0*a1 + a2^2*p0)
  bn128_Fp2_mont_sub( a1a1 , a0a2    , TGT(2) );        // a1^2 - a0*a2
  bn128_Fp2_mont_mul_inplace( TGT(0) , denom );
  bn128_Fp2_mont_mul_inplace( TGT(1) , denom );
  bn128_Fp2_mont_mul_inplace( TGT(2) , denom );
}

void bn128_Fp6_mont_inv_inplace ( uint64_t *tgt ) {
  bn128_Fp6_mont_inv ( tgt , tgt ); 
}

void bn128_Fp6_mont_div ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  uint64_t inv[EXT_NWORDS];
  bn128_Fp6_mont_inv ( src2 , inv );
  bn128_Fp6_mont_mul ( src1 , inv , tgt );
}

void bn128_Fp6_mont_div_inplace ( uint64_t *tgt, const uint64_t *src2 ) {
  bn128_Fp6_mont_div ( tgt , src2 , tgt ); 
}


const uint64_t bn128_Fp6_mont_frobenius_sparse_indices[10] = 
  { 0x0000000000000000, 0x0000000000010001, 0x0000000000020002, 0x0000000000020003, 0x0000000000030002, 0x0000000000030003, 0x0000000000040004, 0x0000000000040005, 0x0000000000050004, 0x0000000000050005
  };


const uint64_t bn128_Fp6_mont_frobenius_sparse_entries[40] = 
  { 0xd35d438dc58f0d9d, 0x0a78eb28f5c70b3d, 0x666ea36f7879462c, 0x0e0a77c19a07df2f
  , 0x68c3488912edefaa, 0x8d087f6872aabf4f, 0x51e1a24709081231, 0x2259d6b14729c0fa
  , 0xb5773b104563ab30, 0x347f91c8a9aa6454, 0x7a007127242e0991, 0x1956bcd8118214ec
  , 0x6e849f1ea0aa4757, 0xaa1c7b6d89f89141, 0xb6e713cdfae0ca3a, 0x26694fbb4e82ebc3
  , 0x6e849f1ea0aa4757, 0xaa1c7b6d89f89141, 0xb6e713cdfae0ca3a, 0x26694fbb4e82ebc3
  , 0x86a9510693195217, 0x6301d8c8bec76638, 0x3e4fd48f5d534ecc, 0x170d919acfaf8b3d
  , 0x7361d77f843abe92, 0xa5bb2bd3273411fb, 0x9c941f314b3e2399, 0x15df9cddbb9fd3ec
  , 0x5dddfd154bd8c949, 0x62cb29a5a4445b60, 0x37bc870a0c7dd2b9, 0x24830a9d3171f0fd
  , 0x5dddfd154bd8c949, 0x62cb29a5a4445b60, 0x37bc870a0c7dd2b9, 0x24830a9d3171f0fd
  , 0xc8beb49754423eb5, 0xf1c63ebe413db891, 0x1bbc2685364334c3, 0x1a84b1952591cc3d
  };


void bn128_Fp6_mont_frobenius ( const uint64_t *src, uint64_t *tgt ) {
  uint64_t acc[EXT_NWORDS];
  uint64_t tmp[PRIME_NWORDS];
  bn128_Fp6_mont_set_zero(acc);
  for(int k=0; k<10; k++) {
    uint64_t ij = bn128_Fp6_mont_frobenius_sparse_indices[k];
    uint64_t i  = (ij >> 16);
    uint64_t j  = (ij &  0xffff);
    bn128_Fp_mont_mul( src + i*PRIME_NWORDS , bn128_Fp6_mont_frobenius_sparse_entries + k*PRIME_NWORDS , tmp );
    bn128_Fp_mont_add_inplace( acc + j*PRIME_NWORDS , tmp );
  }
  bn128_Fp6_mont_copy( acc, tgt );
}

void bn128_Fp6_mont_frobenius_inplace ( uint64_t *tgt ) {
  bn128_Fp6_mont_frobenius( tgt, tgt );
}

// computes `x^e mod p`
void bn128_Fp6_mont_pow_uint64( const uint64_t *src, uint64_t exponent, uint64_t *tgt ) {
  uint64_t e = exponent;
  uint64_t sqr[24];
  bn128_Fp6_mont_copy( src, sqr );                 // sqr := src
  bn128_Fp6_mont_set_one( tgt );                   // tgt := 1
  while(e!=0) {
    if (e & 1) { bn128_Fp6_mont_mul_inplace(tgt, sqr); }
    bn128_Fp6_mont_mul_inplace(sqr, sqr);
    e = e >> 1;
  }
}

// computes `x^e mod p` (for `e` non-negative bigint)
void bn128_Fp6_mont_pow_gen( const uint64_t *src, const uint64_t *expo, uint64_t *tgt, int expo_len ) {
  uint64_t sqr[24];
  bn128_Fp6_mont_copy( src, sqr );                 // sqr := src
  bn128_Fp6_mont_set_one( tgt );                   // tgt := 1
  int s = expo_len - 1;
  while ((expo[s] == 0) && (s>0)) { s--; }          // skip the unneeded largest powers
  for(int i=0; i<=s; i++) {
    uint64_t e = expo[i];
    for(int j=0; j<64; j++) {
      if (e & 1) { bn128_Fp6_mont_mul_inplace(tgt, sqr); }
      bn128_Fp6_mont_mul_inplace(sqr, sqr);
      e = e >> 1;
    }
  }
}

#define I_SRC(i)   (src    + (i)*24)
#define I_TGT(i)   (tgt    + (i)*24)
#define I_PROD(i)  (prods  + (i)*24)
#define I_RECIP(i) (recips + (i)*24)

// computes the inverse of many field elements at the same time, efficiently
// uses the Montgomery batch inversion trick
void bn128_Fp6_mont_batch_inv( int n, const uint64_t *src, uint64_t *tgt ) {
  assert( n >= 1 );
  uint64_t *prods  = malloc( 8*24*n );
  uint64_t *recips = malloc( 8*24*n );
  assert( prods  != 0 );
  assert( recips != 0 );
  
  // compute partial products (a[0]*a[1]*...*a[k]) for all k
  bn128_Fp6_mont_copy( I_SRC(0) , I_PROD(0) );
  for(int i=1; i<n; i++) {
    bn128_Fp6_mont_mul( I_PROD(i-1) , I_SRC(i) , I_PROD(i) );
  }
  
  // compute inverses of partial products 1/(a[0]*a[1]*...*a[k]) for all k
  bn128_Fp6_mont_inv( I_PROD(n-1) , I_RECIP(n-1) );
  for(int i=n-2; i>=0; i--) {
    bn128_Fp6_mont_mul( I_RECIP(i+1) , I_SRC(i+1) , I_RECIP(i) );
  }
  
  // compute the inverses 1/a[k] for all k
  bn128_Fp6_mont_copy( I_RECIP(0) , I_TGT(0) );
  for(int i=1; i<n; i++) {
    bn128_Fp6_mont_mul( I_RECIP(i) , I_PROD(i-1) , I_TGT(i) );
  }
  
  free(recips);
  free(prods);
}
