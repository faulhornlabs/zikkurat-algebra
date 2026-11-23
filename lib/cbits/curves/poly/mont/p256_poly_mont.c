
// dense univariate polynomials with coefficients in a field

#include <stdint.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "p256_Fr_mont.h"

#define NLIMBS 4

#define MIN(a,b) ( ((a)<=(b)) ? (a) : (b) )
#define MAX(a,b) ( ((a)>=(b)) ? (a) : (b) )

#define SRC1(i) (src1 + (i)*NLIMBS)
#define SRC2(i) (src2 + (i)*NLIMBS)
#define TGT(i)  (tgt  + (i)*NLIMBS)
#define QUOT(i) (quot + (i)*NLIMBS)
#define REM(i)  (rem  + (i)*NLIMBS)


// returns the degree of the polynomial (can be smaller than the size)
// by definition, the constant zero polynomial has degree -1
int p256_poly_mont_degree( int n1, const uint64_t *src1 ) {
  int deg = -1;
  for(int i=n1-1; i>=0; i--) {
    if (!p256_Fr_mont_is_zero( SRC1(i) )) { deg=i; break; }
  }
  return deg;
}

// get the k-th coefficient
void p256_poly_mont_get_coeff( int n1, const uint64_t *src1, int k, uint64_t *tgt ) {
  if ( (k<0) || (k>=n1) ) {
    p256_Fr_mont_set_zero( tgt );
  }
  else {
    p256_Fr_mont_copy( SRC1(k), tgt ); 
  }
}

// check for zero polynomials
uint8_t p256_poly_mont_is_zero( int n1, const uint64_t *src1 ) {
  uint8_t ok = 1;
  for(int i=0; i<n1; i++) {
    if (!p256_Fr_mont_is_zero( SRC1(i) )) { ok = 0; break; }
  }
  return ok;
}

// check polynomial equality
uint8_t p256_poly_mont_is_equal
  ( int  n1, const uint64_t *src1
  , int  n2, const uint64_t *src2
  ) {
  int M = MIN( n1 , n2 );
  int N = MAX( n1 , n2 );

  uint8_t ok = 1;

  for(int i=0; i<M; i++) {
    if (!p256_Fr_mont_is_equal( SRC1(i) , SRC2(i) )) { ok = 0; break; }
  }
  if (!ok) return ok;

  if (n1 >= n2) {
    for(int i=M; i<N; i++) {
      if (!p256_Fr_mont_is_zero( SRC1(i) )) { ok = 0; break; }
    }
  }
  else {
    // n2 > n1
    for(int i=M; i<N; i++) {
      if (!p256_Fr_mont_is_zero( SRC2(i) )) { ok = 0; break; }
    }
  }  
  return ok;
}

// Negates a polynomial. 
// Requires a target buffer of size `n1`.
void p256_poly_mont_neg
  ( int  n1, const uint64_t *src1
  ,                uint64_t *tgt ) {

  for(int i=0; i<n1; i++) {
    p256_Fr_mont_neg( SRC1(i) , TGT(i) );
  }
}

// Add two polynomials. 
// Requires a target buffer of size `max(n1,n2)`
void p256_poly_mont_add
  ( int  n1, const uint64_t *src1
  , int  n2, const uint64_t *src2
  ,                uint64_t *tgt ) {

  int M = MIN( n1 , n2 );
  int N = MAX( n1 , n2 );

  for(int i=0; i<M; i++) {
    p256_Fr_mont_add( SRC1(i) , SRC2(i) , TGT(i) );    
  }
  if (n1 >= n2) {
    for(int i=M; i<N; i++) {
      p256_Fr_mont_copy( SRC1(i) , TGT(i) );    
    }
  }
  else {
    // n2 > n1
    for(int i=M; i<N; i++) {
      p256_Fr_mont_copy( SRC2(i) , TGT(i) );    
    }
  }
}

// Subtract two polynomials. 
// Requires a target buffer of size `max(n1,n2)`
void p256_poly_mont_sub
  ( int  n1, const uint64_t *src1
  , int  n2, const uint64_t *src2
  ,                uint64_t *tgt ) {

  int M = (n1 <= n2) ? n1 : n2;     // min
  int N = (n1 >= n2) ? n1 : n2;     // max

  for(int i=0; i<M; i++) {
    p256_Fr_mont_sub( SRC1(i) , SRC2(i) , TGT(i) );    
  }
  if (n1 >= n2) {
    for(int i=M; i<N; i++) {
      p256_Fr_mont_copy( SRC1(i) , TGT(i) );    
    }
  }
  else {
    // n2 > n1
    for(int i=M; i<N; i++) {
      p256_Fr_mont_neg( SRC2(i) , TGT(i) );    
    }
  }
}

// Multiplies a polynomial by a constant. 
// Requires a target buffer of size `n1`.
void p256_poly_mont_scale
  (          const uint64_t *kst1
  , int n2 , const uint64_t *src2
  ,          uint64_t *tgt ) {
  if (p256_Fr_mont_is_zero(kst1)) {
    // multiply by zero
    for(int i=0; i<n2; i++) { p256_Fr_mont_set_zero( TGT(i) ); }
    return;
  }
  if (p256_Fr_mont_is_one(kst1)) {
    // multiply by one
    for(int i=0; i<n2; i++) { p256_Fr_mont_copy( SRC2(i) , TGT(i) ); }
    return;
  }
  // generic scaling
  for(int i=0; i<n2; i++) {
    p256_Fr_mont_mul( kst1 , SRC2(i) , TGT(i) );
  }
}


// Multiply two polynomials, naive algorithm. 
// Requires a target buffer of size `(n1+n2-1)` (!)
void p256_poly_mont_mul_naive( int  n1, const uint64_t *src1
                   , int  n2, const uint64_t *src2
                   ,                uint64_t *tgt ) {

  int N = n1+n2-1;
  for(int k=0; k<N; k++) {
    uint64_t acc[NLIMBS];
    p256_Fr_mont_set_zero( acc );
    // 0 <= i <= min(k , n1-1)
    // 0 <= j <= min(k , n2-1)
    // k = i + j
    // 0 >= i = k - j >= k - min(k , n2-1)
    // 0 >= j = k - i >= k - min(k , n1-1)
    int A = MAX( 0 , k - MIN(k , n2-1) );
    int B = MIN( k , n1-1 );
    for( int i = A ; i <= B ; i++ ) {
      uint64_t tmp[NLIMBS];
      int j = k - i;
      p256_Fr_mont_mul( SRC1(i) , SRC2(j) , tmp );
      p256_Fr_mont_add_inplace( acc, tmp );      
    }
    p256_Fr_mont_copy( acc, TGT(k) );
  }
}

// evaluate a polynomial at a single point
void p256_poly_mont_eval_at( int  n1, const uint64_t *src1, const uint64_t *loc, uint64_t *tgt ) {
  uint64_t run[NLIMBS];               // x^i
  p256_Fr_mont_set_zero(tgt);
  p256_Fr_mont_set_one (run);
  for(int i=0; i<n1; i++) {
    uint64_t tmp[NLIMBS];
    if (i>0) { 
      p256_Fr_mont_mul( SRC1(i) , run , tmp );
      p256_Fr_mont_add_inplace( tgt, tmp ); 
    }
    else {
      // constant term
      p256_Fr_mont_copy( SRC1(i) , tgt );
    }
    if (i < n1-1) {
      p256_Fr_mont_mul_inplace( run, loc );
    }
  }
}


// polynomial long division
// allocate at least `deg(p) - deg(q) + 1` field elements for the quotient
// and at least `deg(q)` for the remainder
void p256_poly_mont_long_div( int n1, const uint64_t *src1, int n2, const uint64_t *src2, int nquot, uint64_t *quot, int nrem, uint64_t *rem ) {
  int deg_p = p256_poly_mont_degree( n1, src1 );
  int deg_q = p256_poly_mont_degree( n2, src2 );
  assert( (!quot) || (nquot >= deg_p - deg_q + 1) );
  assert( (!rem ) || (nrem  >= deg_q)             );

  if (deg_q < 0) {
    // division by zero
    if (quot) { for(int j=0; j<nquot; j++) { p256_Fr_mont_set_zero( QUOT(j) ); } }
    if (rem ) { for(int j=0; j<nrem ; j++) { p256_Fr_mont_set_zero( REM(j)  ); } }
    return;
  }

  if (deg_p < deg_q) {
    // quotient == 0
    if (quot) { for(int j=0; j<nquot; j++) { p256_Fr_mont_set_zero( QUOT(j) ); } }
    if (rem ) {
      for(int j=deg_p+1; j<nrem; j++) { p256_Fr_mont_set_zero( REM(j) ); }
      assert( nrem >= deg_p+1 ); 
      memcpy( rem, src1, 8*(deg_p+1)*NLIMBS );
    }
    return;
  }

  if (quot) { for(int j=MAX(0,deg_p-deg_q+1); j<nquot; j++) { p256_Fr_mont_set_zero( QUOT(j) ); } }
  if (rem ) { for(int j=MAX(0,deg_q        ); j<nrem ; j++) { p256_Fr_mont_set_zero( REM(j)  ); } }

  uint64_t *tgt = malloc( 8*(deg_p+1)*NLIMBS );
  assert( tgt != 0 );
  memcpy( tgt, src1, 8*(deg_p+1)*NLIMBS );

  uint64_t lead_inv[NLIMBS];
  p256_Fr_mont_inv( SRC2(deg_q) , lead_inv );

  for(int k=deg_p; k>=deg_q; k--) {
    uint64_t scl[NLIMBS];
    p256_Fr_mont_mul( TGT(k) , lead_inv , scl );
    for(int i=0; i<=deg_q; i++) {
      uint64_t tmp[NLIMBS];
      p256_Fr_mont_mul( SRC2(i) , scl , tmp );
      p256_Fr_mont_sub_inplace( TGT(k-deg_q+i) , tmp );
    }
    if (quot) { p256_Fr_mont_copy( scl , QUOT(k-deg_q) ); } 
  }

  if (rem) { memcpy( rem , tgt , 8*NLIMBS * MIN(deg_p+1,deg_q) ); } 

  free(tgt);
}

// polynomial quotient
// allocate at least `deg(p) - deg(q) + 1` field elements for quotient
void p256_poly_mont_quot( int n1, const uint64_t *src1, int n2, const uint64_t *src2, int nquot, uint64_t *quot ) {
  p256_poly_mont_long_div( n1, src1, n2, src2, nquot, quot, 0, 0 );
}

// polynomial remainder
// allocate at least `deg(q)` field elements for the remainder
void p256_poly_mont_rem( int n1, const uint64_t *src1, int n2, const uint64_t *src2, int nrem, uint64_t *rem ) {
  p256_poly_mont_long_div( n1, src1, n2, src2, 0, 0, nrem, rem );
}


// divide by the vanishing polynomial of a coset `(x^n - eta)`
// This should be much faster than the general-purpose long division
// Remark: the case `eta = 1` corresponds to a subgroup
// allocate at least `deg(p) - n + 1` field elements for the quotient
// and at least `n` for the remainder
void p256_poly_mont_div_by_vanishing( int n1, const uint64_t *src1, int expo_n, const uint64_t *eta, int nquot, uint64_t *quot, int nrem, uint64_t *rem ) {
  int deg_p = p256_poly_mont_degree( n1, src1 );
  int n = expo_n;
  assert( quot );  // NOTE: quot cannot be NULL for this routine (or maybe we could allocate a temp buffer in that case?)
  assert( (!quot) || (nquot >= deg_p - n + 1) );
  assert( (!rem ) || (nrem  >= n)             );
  assert( n >= 1 );

  if (deg_p < n) {
    // quotient == 0
    if (quot) { for(int j=0; j<nquot; j++) { p256_Fr_mont_set_zero( QUOT(j) ); } }
    if (rem ) {
      for(int j=deg_p+1; j<nrem; j++) { p256_Fr_mont_set_zero( REM(j) ); }
      assert( nrem >= deg_p+1 ); 
      memcpy( rem, src1, 8*(deg_p+1)*NLIMBS );
    }
    return;
  }

  if (quot) { for(int j=MAX(0,deg_p-n+1); j<nquot; j++) { p256_Fr_mont_set_zero( QUOT(j) ); } }
  if (rem ) { for(int j=MAX(0,n        ); j<nrem ; j++) { p256_Fr_mont_set_zero( REM(j)  ); } }

  if (p256_Fr_mont_is_one(eta)) {
    // 
    // eta = 1, we don't need to multiply by it
    // 
    for(int j=deg_p-n; j>=0; j--) {
      if (j+n <= deg_p-n) {
        // as[j+n] + bs[j+n]
        p256_Fr_mont_add( SRC1(j+n) , QUOT(j+n) , QUOT(j) );
      }
      else {
        // bs[j+n] is zero
        p256_Fr_mont_copy( SRC1(j+n) , QUOT(j) );
      }
    }
    if (rem) {
      for(int j=0; j<n; j++) {
        if (j <= deg_p-n) {
          // as[j] + bs[j]
          p256_Fr_mont_add( SRC1(j) , QUOT(j) , REM(j) );
        }
        else {
          // bs[j] is zero
          p256_Fr_mont_copy( SRC1(j) , REM(j) );
        }
      }
    }
  }
  else {
    // 
    // eta != 1, generic case
    // 
    for(int j=deg_p-n; j>=0; j--) {
      if (j+n <= deg_p-n) {
        uint64_t tmp[NLIMBS];
        // as[j+n] + eta * bs[j+n]
        p256_Fr_mont_mul( QUOT(j+n) , eta , tmp );
        p256_Fr_mont_add( SRC1(j+n) , tmp , QUOT(j) );
      }
      else {
        // bs[j+n] is zero
        p256_Fr_mont_copy( SRC1(j+n) , QUOT(j) );
      }
    }
    if (rem) {
      for(int j=0; j<n; j++) {
        if (j <= deg_p-n) {
          uint64_t tmp[NLIMBS];
          // as[j] + eta * bs[j]
          p256_Fr_mont_mul( QUOT(j) , eta , tmp );
          p256_Fr_mont_add( SRC1(j) , tmp , REM(j) );
        }
        else {
          // bs[j] is zero
          p256_Fr_mont_copy( SRC1(j) , REM(j) );
        }
      }
    }
  }
}

// divide by the vanishing polynomial `x^n - eta`
// returns True if the remainder is zero
// TODO: this could be implemented with no allocation, but i don't want to copy-paste the whole code right now
uint8_t p256_poly_mont_quot_by_vanishing( int n1, const uint64_t *src1, int expo_n, const uint64_t *eta, int nquot, uint64_t *quot ) {
  int nrem = expo_n;
  uint64_t *rem = malloc( 8*NLIMBS*nrem );
  assert( rem != 0 );
  p256_poly_mont_div_by_vanishing( n1, src1, expo_n, eta, nquot, quot, nrem, rem );
  int ok = 1;
  for(int j=0; j<nrem; j++) {
    if (!p256_Fr_mont_is_zero(REM(j))) { ok = 0; break; }
  }
  free(rem);
  return ok;
}


// substitutes `x -> x*eta` into the polynomial
void p256_poly_mont_shift_poly_mul( int n, const uint64_t *eta, const uint64_t *src1, uint64_t *tgt ) {
  uint64_t acc[NLIMBS];
  p256_Fr_mont_set_one(acc);
  for(int i=0; i<n; i++) {
    p256_Fr_mont_mul        ( SRC1(i) , acc , TGT(i) );
    p256_Fr_mont_mul_inplace( acc, eta );
  }
}

// substitutes `x -> x+eta` into the polynomial
void p256_poly_mont_shift_poly_add( int n, const uint64_t *eta, const uint64_t *src1, uint64_t *tgt ) {
  uint64_t *etaPows  = (uint64_t *) malloc( 8 * NLIMBS * n);
  uint64_t *binomial = (uint64_t *) malloc( 8 * NLIMBS * n);
  
  uint64_t acc[NLIMBS];
  p256_Fr_mont_set_one( acc );
  for(int i=0; i<n; i++) {
    p256_Fr_mont_set_one(    binomial + i*NLIMBS );
    p256_Fr_mont_copy( acc , etaPows  + i*NLIMBS );
    p256_Fr_mont_mul_inplace( acc, eta );
  }
  
  for(int j=0; j<n; j++) {
    uint64_t tmp[NLIMBS];
    
    // calculate `acc := sum_k c_k * binom(k,j) * eta^(k-j)
    p256_Fr_mont_set_zero( acc );
    for(int k=j; k<n; k++) {
      p256_Fr_mont_mul        ( SRC1(k) , etaPows  + (k-j)*NLIMBS , tmp );
      p256_Fr_mont_mul_inplace( tmp     , binomial + (k-j)*NLIMBS );
      p256_Fr_mont_add_inplace( acc     , tmp);
    }
    p256_Fr_mont_copy( acc , TGT(j) );
    
    // update binomials
    p256_Fr_mont_set_zero( tmp );
    for(int i=0; i<n-j-1; i++) {
      p256_Fr_mont_add_inplace( tmp , binomial + i*NLIMBS );
      p256_Fr_mont_copy       ( tmp , binomial + i*NLIMBS );
    }
  }
  
  free(binomial);
  free(etaPows);
}



/*
void p256_poly_mont_signed_rotation_to_right( int N, const uint64_t *src, uint64_t *tgt) {
  memcpy( tgt + NLIMBS , src , (N-1)*NLIMBS*8 );
  p256_Fr_mont_neg( src + (N-1)*NLIMBS , tgt );
}
*/

// assumes the inputs has size power of two (and you need to supply the logarithm)
// only efficient for relatively small inputs (eg. 256 is still good)
//
// Product in @R-_m = R[z]/(z^m + 1)@ (the negacylic ring).
// Uses  @R-_{2m} = R-_m[z]/(z^2 - u)@ and Karatsuba:
//
//   (A0 + z*A1) * (B0 + z*B1)  ==  (A0*B0 + u*A1*B1) + z*(A0*B1 + A1*B0)
//
// Remark: This would works over any commutative ring
//
void p256_poly_mont_negacyclic_conv_Karatsuba_CRT( int logN, int logStrideIn, const uint64_t *src1, const uint64_t *src2, uint64_t *tgt) {
  if (logN==0) {
    p256_Fr_mont_mul( src1 , src2 , tgt );
  }
  else {
    int N         = (1<<logN);
    int strideIn  = (1<<logStrideIn);
    int halfN     = (N>>1);
    int step      = strideIn * NLIMBS;
    
    uint64_t *p0     = (uint64_t*) malloc( halfN * NLIMBS * 8 );
    uint64_t *p1     = (uint64_t*) malloc( halfN * NLIMBS * 8 );
    uint64_t *p2     = (uint64_t*) malloc( halfN * NLIMBS * 8 );
    uint64_t *tmp1   = (uint64_t*) malloc( halfN * NLIMBS * 8 );
    uint64_t *tmp2   = (uint64_t*) malloc( halfN * NLIMBS * 8 );
    
    assert( p0   != 0 );
    assert( p1   != 0 );
    assert( p2   != 0 );
    assert( tmp1 != 0 );
    assert( tmp2 != 0 );
    
    p256_poly_mont_negacyclic_conv_Karatsuba_CRT( logN-1 , logStrideIn+1 , src1        , src2        , p0 );  // evens
    p256_poly_mont_negacyclic_conv_Karatsuba_CRT( logN-1 , logStrideIn+1 , src1 + step , src2 + step , p1 );  // odds 
    
    for(int i=0; i<halfN; i++) {
      p256_Fr_mont_add( src1 + (2*i)*step , src1 + (2*i+1)*step , tmp1 + i*NLIMBS );   // f_even + f_odd
      p256_Fr_mont_add( src2 + (2*i)*step , src2 + (2*i+1)*step , tmp2 + i*NLIMBS );   // g_even + g_odd
    }
    p256_poly_mont_negacyclic_conv_Karatsuba_CRT( logN-1 , 0 , tmp1 , tmp2 , p2 );  // mixed
    
    // out_evens := p0 + signedRotRight(p1)
    p256_Fr_mont_sub( p0 , p1 + (halfN-1)*NLIMBS , tgt );
    for(int i=1; i<halfN; i++) {
      p256_Fr_mont_add( p0 + i*NLIMBS , p1 + (i-1)*NLIMBS , tgt + (2*i)*NLIMBS );
    }
    // out_odds := p2 - p0 - p1
    for(int i=0; i<halfN; i++) {
      uint64_t p0_plus_p1[NLIMBS];
      p256_Fr_mont_add( p0 + i*NLIMBS , p1 + i*NLIMBS , p0_plus_p1 );
      p256_Fr_mont_sub( p2 + i*NLIMBS , p0_plus_p1 , tgt + (2*i+1)*NLIMBS );
    }
    
    free(tmp2);
    free(tmp1);
    free(p2);
    free(p1);
    free(p0);
  }
}

// assumes the inputs has size power of two (and you need to supply the logarithm)
// only efficient for relatively small inputs (eg. 256 is still good)
//
void p256_poly_mont_cyclic_conv_Karatsuba_CRT(int logN, const uint64_t *src1, const uint64_t *src2, uint64_t *tgt) {
  if (logN==0) {
    p256_Fr_mont_mul( src1 , src2 , tgt );
  }
  else {
    int N      = (1<<logN);
    int halfN  = (N>>1);
    
    uint64_t *tmp1  = (uint64_t*) malloc( halfN * NLIMBS * 8 );
    uint64_t *tmp2  = (uint64_t*) malloc( halfN * NLIMBS * 8 );
    uint64_t *posi  = (uint64_t*) malloc( halfN * NLIMBS * 8 );
    uint64_t *nega  = (uint64_t*) malloc( halfN * NLIMBS * 8 );
    
    assert( tmp1 != 0 );
    assert( tmp2 != 0 );
    assert( posi != 0 );
    assert( nega != 0 );
    
    for(int i=0; i<halfN; i++) {
      p256_Fr_mont_add( src1 + i*NLIMBS , src1 + (i+halfN)*NLIMBS , tmp1 + i*NLIMBS );   // (flo .+. fhi)
      p256_Fr_mont_add( src2 + i*NLIMBS , src2 + (i+halfN)*NLIMBS , tmp2 + i*NLIMBS );   // (glo .+. ghi)
    }
    p256_poly_mont_cyclic_conv_Karatsuba_CRT( logN-1 , tmp1 , tmp2 , posi );
    
    for(int i=0; i<halfN; i++) {
      p256_Fr_mont_sub( src1 + i*NLIMBS , src1 + (i+halfN)*NLIMBS , tmp1 + i*NLIMBS );   // (flo .-. fhi)
      p256_Fr_mont_sub( src2 + i*NLIMBS , src2 + (i+halfN)*NLIMBS , tmp2 + i*NLIMBS );   // (glo .-. ghi)
    }
    p256_poly_mont_negacyclic_conv_Karatsuba_CRT( logN-1 , 0 , tmp1 , tmp2 , nega );
    
    for(int i=0; i<halfN; i++) {
      p256_Fr_mont_add( posi + i*NLIMBS , nega + i*NLIMBS , tgt +  i       *NLIMBS );
      p256_Fr_mont_sub( posi + i*NLIMBS , nega + i*NLIMBS , tgt + (i+halfN)*NLIMBS );
      p256_Fr_mont_div_by_2_inplace( tgt +  i       *NLIMBS );
      p256_Fr_mont_div_by_2_inplace( tgt + (i+halfN)*NLIMBS );
    }
    
    free(nega);
    free(posi);
    free(tmp2);
    free(tmp1);
  }
}



// create the permutation for Rader's NTT
// the length N should be a prime
// and `mgen` should be a multiplicative generator of the finite field of size N
// the output will have length `N-1`, and it's supposed to be a permutation of [1..N-1]
void p256_poly_mont_make_Rader_perm( int N , int64_t mgen , int64_t *tgt ) {
  int64_t a = 1;
  for(int i=0; i<N-1; i++) {
    tgt[i] = a;
    a = ((a * mgen) % N);
  }
}

void p256_poly_mont_prepare_for_Rader( int N, const int64_t *perm, const uint64_t *omega, const uint64_t *src, uint64_t *tgt_Y0, uint64_t *tgt_XS, uint64_t *tgt_HS ) {

  uint64_t *omegaPows = (uint64_t*) malloc( 8 * N * NLIMBS );
  assert( omegaPows != 0);

  uint64_t acc[NLIMBS];
  p256_Fr_mont_set_one( acc );
  for(int i=0; i<N; i++) {
    p256_Fr_mont_copy( acc , omegaPows + i*NLIMBS );
    p256_Fr_mont_mul_inplace( acc , omega );
  }
  
  for(int i=1; i<N; i++) {
     int64_t j = perm[i-1];
     p256_Fr_mont_copy( omegaPows + j*NLIMBS , tgt_HS + (i-1)*NLIMBS );
     int64_t k;
     if (i==1) { k=1; } else { k=perm[N-i]; }
     p256_Fr_mont_copy( src + k*NLIMBS , tgt_XS + (i-1)*NLIMBS );
  }
  
  // y0 = (a0 + sum xs)
  p256_Fr_mont_copy( src , tgt_Y0 );
  for(int i=1; i<N; i++) {
  p256_Fr_mont_add_inplace( tgt_Y0 , tgt_XS + (i-1)*NLIMBS );
  }
  
  free(omegaPows);
}

void p256_poly_mont_postprocess_Rader( int N, const int64_t *perm, const uint64_t *Y0, const uint64_t *src, uint64_t *YS, uint64_t *tgt) {
  for(int i=1; i<N; i++) {
    int64_t j = perm[i-1];
    p256_Fr_mont_copy( YS + (i-1)*NLIMBS , tgt + j*NLIMBS );  // scatter
    p256_Fr_mont_add_inplace( tgt + j*NLIMBS , src );         // += a0
  }
  p256_Fr_mont_copy( Y0 , tgt );
}
