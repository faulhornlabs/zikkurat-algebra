
// dense univariate polynomials with coefficients in a field

#include <stdint.h>
#include <assert.h>

#include "bn128_r_mont.h"

#define NLIMBS 4

#define MIN(a,b) ( ((a)<=(b)) ? (a) : (b) )
#define MAX(a,b) ( ((a)>=(b)) ? (a) : (b) )

#define SRC1(i) (src1 + i*NLIMBS)
#define SRC2(i) (src2 + i*NLIMBS)
#define TGT(i)  (tgt  + i*NLIMBS)


// returns the degree of the polynomial (can be smaller than the size)
// by definition, the constant zero polynomials has degree -1
int bn128_poly_mont_degree( int n1, const uint64_t *src1 ) {
  int deg = -1;
  for(int i=0; i<n1; i++) {
    if (!bn128_r_mont_is_zero( SRC1(i) )) { deg = i; }
  }
  return deg;
}

// get the k-th coefficient
void bn128_poly_mont_get_coeff( int n1, const uint64_t *src1, int k, uint64_t *tgt ) {
  if ( (k<0) || (k>=n1) ) {
    bn128_r_mont_set_zero( tgt );
  }
  else {
    bn128_r_mont_copy( SRC1(k), tgt ); 
  }
}

// check for zero polynomials
uint8_t bn128_poly_mont_is_zero( int n1, const uint64_t *src1 ) {
  uint8_t ok = 1;
  for(int i=0; i<n1; i++) {
    if (!bn128_r_mont_is_zero( SRC1(i) )) { ok = 0; break; }
  }
  return ok;
}

// check polynomial equality
uint8_t bn128_poly_mont_is_equal
  ( int  n1, const uint64_t *src1
  , int  n2, const uint64_t *src2
  ) {
  int M = MIN( n1 , n2 );
  int N = MAX( n1 , n2 );

  uint8_t ok = 1;

  for(int i=0; i<M; i++) {
    if (!bn128_r_mont_is_equal( SRC1(i) , SRC2(i) )) { ok = 0; break; }
  }
  if (!ok) return ok;

  if (n1 >= n2) {
    for(int i=M; i<N; i++) {
      if (!bn128_r_mont_is_zero( SRC1(i) )) { ok = 0; break; }
    }
  }
  else {
    // n2 > n1
    for(int i=M; i<N; i++) {
      if (!bn128_r_mont_is_zero( SRC2(i) )) { ok = 0; break; }
    }
  }  
  return ok;
}

// Negates a polynomial. 
// Requires a target buffer of size `n1`.
void bn128_poly_mont_neg
  ( int  n1, const uint64_t *src1
  ,                uint64_t *tgt ) {

  for(int i=0; i<n1; i++) {
    bn128_r_mont_neg( SRC1(i) , TGT(i) );
  }
}

// Add two polynomials. 
// Requires a target buffer of size `max(n1,n2)`
void bn128_poly_mont_add
  ( int  n1, const uint64_t *src1
  , int  n2, const uint64_t *src2
  ,                uint64_t *tgt ) {

  int M = MIN( n1 , n2 );
  int N = MAX( n1 , n2 );

  for(int i=0; i<M; i++) {
    bn128_r_mont_add( SRC1(i) , SRC2(i) , TGT(i) );    
  }
  if (n1 >= n2) {
    for(int i=M; i<N; i++) {
      bn128_r_mont_copy( SRC1(i) , TGT(i) );    
    }
  }
  else {
    // n2 > n1
    for(int i=M; i<N; i++) {
      bn128_r_mont_copy( SRC2(i) , TGT(i) );    
    }
  }
}

// Subtract two polynomials. 
// Requires a target buffer of size `max(n1,n2)`
void bn128_poly_mont_sub
  ( int  n1, const uint64_t *src1
  , int  n2, const uint64_t *src2
  ,                uint64_t *tgt ) {

  int M = (n1 <= n2) ? n1 : n2;     // min
  int N = (n1 >= n2) ? n1 : n2;     // max

  for(int i=0; i<M; i++) {
    bn128_r_mont_sub( SRC1(i) , SRC2(i) , TGT(i) );    
  }
  if (n1 >= n2) {
    for(int i=M; i<N; i++) {
      bn128_r_mont_copy( SRC1(i) , TGT(i) );    
    }
  }
  else {
    // n2 > n1
    for(int i=M; i<N; i++) {
      bn128_r_mont_neg( SRC2(i) , TGT(i) );    
    }
  }
}

// Multiplies a polynomial by a constant. 
// Requires a target buffer of size `n1`.
void bn128_poly_mont_scale
  (          const uint64_t *kst1
  , int n2 , const uint64_t *src2
  ,          uint64_t *tgt ) {
  if (bn128_r_mont_is_zero(kst1)) {
    // multiply by zero
    for(int i=0; i<n2; i++) { bn128_r_mont_set_zero( TGT(i) ); }
    return;
  }
  if (bn128_r_mont_is_one(kst1)) {
    // multiply by one
    for(int i=0; i<n2; i++) { bn128_r_mont_copy( SRC2(i) , TGT(i) ); }
    return;
  }
  // generic scaling
  for(int i=0; i<n2; i++) {
    bn128_r_mont_mul( kst1 , SRC2(i) , TGT(i) );
  }
}

// Linear combination of K polynomials. 
// Requires a target buffer of size max{ n_k | k=0..K-1 }
void bn128_poly_mont_lincomb
  ( int  K                           // number of polynomials
  , const int *ns                    // sizes of the polynomials
  , const uint64_t **coeffs          // pointers to the combination coefficients
  , const uint64_t **polys           // pointers to the polynomials
  ,       uint64_t *tgt              // target buffer 
  ) {

  int N = 0;
  for(int k=0; k<K; k++) { 
    if (ns[k] > N) { N = ns[k]; }
  }

  for(int i=0; i<N; i++) {
    uint64_t acc[NLIMBS];
    bn128_r_mont_set_zero(acc);
    for(int k=0; k<K; k++) {
      int n = ns[k];
      if (i < n) {
        uint64_t tmp[NLIMBS];
        bn128_r_mont_mul( coeffs[k] , polys[k]+i*NLIMBS , tmp );
        bn128_r_mont_add_inplace( acc , tmp );
      }
    }
    bn128_r_mont_copy( acc, tgt+i*NLIMBS );
  }
}

// Multiply two polynomials, naive algorithm. 
// Requires a target buffer of size `(n1+n2-1)` (!)
void bn128_poly_mont_mul_naive( int  n1, const uint64_t *src1
                   , int  n2, const uint64_t *src2
                   ,                uint64_t *tgt ) {

  int N = n1+n2-1;
  for(int k=0; k<N; k++) {
    uint64_t acc[NLIMBS];
    bn128_r_mont_set_zero( acc );
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
      bn128_r_mont_mul( SRC1(i) , SRC2(j) , tmp );
      bn128_r_mont_add_inplace( acc, tmp );      
    }
    bn128_r_mont_copy( acc, TGT(k) );
  }
}

// evaluate a polynomial at a single point
void bn128_poly_mont_eval_at( int  n1, const uint64_t *src1, const uint64_t *loc, uint64_t *tgt ) {
  uint64_t run[NLIMBS];               // x^i
  bn128_r_mont_set_zero(tgt);
  bn128_r_mont_set_one (run);
  for(int i=0; i<n1; i++) {
    uint64_t tmp[NLIMBS];
    if (i>0) { 
      bn128_r_mont_mul( SRC1(i) , run , tmp );
      bn128_r_mont_add_inplace( tgt, tmp );      
    }
    else {
      // constant term
      bn128_r_mont_copy( SRC1(i) , tgt );      
    }
    if (i < n1-1) {
      bn128_r_mont_mul_inplace( run, loc );
    }
  }
}
