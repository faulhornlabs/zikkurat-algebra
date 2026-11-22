
// dense univariate polynomials with coefficients in a field

#include <stdint.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "bls12_381_Fr_mont.h"

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
int bls12_381_poly_mont_degree( int n1, const uint64_t *src1 ) {
  int deg = -1;
  for(int i=n1-1; i>=0; i--) {
    if (!bls12_381_Fr_mont_is_zero( SRC1(i) )) { deg=i; break; }
  }
  return deg;
}

// get the k-th coefficient
void bls12_381_poly_mont_get_coeff( int n1, const uint64_t *src1, int k, uint64_t *tgt ) {
  if ( (k<0) || (k>=n1) ) {
    bls12_381_Fr_mont_set_zero( tgt );
  }
  else {
    bls12_381_Fr_mont_copy( SRC1(k), tgt ); 
  }
}

// check for zero polynomials
uint8_t bls12_381_poly_mont_is_zero( int n1, const uint64_t *src1 ) {
  uint8_t ok = 1;
  for(int i=0; i<n1; i++) {
    if (!bls12_381_Fr_mont_is_zero( SRC1(i) )) { ok = 0; break; }
  }
  return ok;
}

// check polynomial equality
uint8_t bls12_381_poly_mont_is_equal
  ( int  n1, const uint64_t *src1
  , int  n2, const uint64_t *src2
  ) {
  int M = MIN( n1 , n2 );
  int N = MAX( n1 , n2 );

  uint8_t ok = 1;

  for(int i=0; i<M; i++) {
    if (!bls12_381_Fr_mont_is_equal( SRC1(i) , SRC2(i) )) { ok = 0; break; }
  }
  if (!ok) return ok;

  if (n1 >= n2) {
    for(int i=M; i<N; i++) {
      if (!bls12_381_Fr_mont_is_zero( SRC1(i) )) { ok = 0; break; }
    }
  }
  else {
    // n2 > n1
    for(int i=M; i<N; i++) {
      if (!bls12_381_Fr_mont_is_zero( SRC2(i) )) { ok = 0; break; }
    }
  }  
  return ok;
}

// Negates a polynomial. 
// Requires a target buffer of size `n1`.
void bls12_381_poly_mont_neg
  ( int  n1, const uint64_t *src1
  ,                uint64_t *tgt ) {

  for(int i=0; i<n1; i++) {
    bls12_381_Fr_mont_neg( SRC1(i) , TGT(i) );
  }
}

// Add two polynomials. 
// Requires a target buffer of size `max(n1,n2)`
void bls12_381_poly_mont_add
  ( int  n1, const uint64_t *src1
  , int  n2, const uint64_t *src2
  ,                uint64_t *tgt ) {

  int M = MIN( n1 , n2 );
  int N = MAX( n1 , n2 );

  for(int i=0; i<M; i++) {
    bls12_381_Fr_mont_add( SRC1(i) , SRC2(i) , TGT(i) );    
  }
  if (n1 >= n2) {
    for(int i=M; i<N; i++) {
      bls12_381_Fr_mont_copy( SRC1(i) , TGT(i) );    
    }
  }
  else {
    // n2 > n1
    for(int i=M; i<N; i++) {
      bls12_381_Fr_mont_copy( SRC2(i) , TGT(i) );    
    }
  }
}

// Subtract two polynomials. 
// Requires a target buffer of size `max(n1,n2)`
void bls12_381_poly_mont_sub
  ( int  n1, const uint64_t *src1
  , int  n2, const uint64_t *src2
  ,                uint64_t *tgt ) {

  int M = (n1 <= n2) ? n1 : n2;     // min
  int N = (n1 >= n2) ? n1 : n2;     // max

  for(int i=0; i<M; i++) {
    bls12_381_Fr_mont_sub( SRC1(i) , SRC2(i) , TGT(i) );    
  }
  if (n1 >= n2) {
    for(int i=M; i<N; i++) {
      bls12_381_Fr_mont_copy( SRC1(i) , TGT(i) );    
    }
  }
  else {
    // n2 > n1
    for(int i=M; i<N; i++) {
      bls12_381_Fr_mont_neg( SRC2(i) , TGT(i) );    
    }
  }
}

// Multiplies a polynomial by a constant. 
// Requires a target buffer of size `n1`.
void bls12_381_poly_mont_scale
  (          const uint64_t *kst1
  , int n2 , const uint64_t *src2
  ,          uint64_t *tgt ) {
  if (bls12_381_Fr_mont_is_zero(kst1)) {
    // multiply by zero
    for(int i=0; i<n2; i++) { bls12_381_Fr_mont_set_zero( TGT(i) ); }
    return;
  }
  if (bls12_381_Fr_mont_is_one(kst1)) {
    // multiply by one
    for(int i=0; i<n2; i++) { bls12_381_Fr_mont_copy( SRC2(i) , TGT(i) ); }
    return;
  }
  // generic scaling
  for(int i=0; i<n2; i++) {
    bls12_381_Fr_mont_mul( kst1 , SRC2(i) , TGT(i) );
  }
}

// Linear combination of K polynomials. 
// Requires a target buffer of size max{ n_k | k=0..K-1 }
void bls12_381_poly_mont_lincomb
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
    bls12_381_Fr_mont_set_zero(acc);
    for(int k=0; k<K; k++) {
      int n = ns[k];
      if (i < n) {
        uint64_t tmp[NLIMBS];
        bls12_381_Fr_mont_mul( coeffs[k] , polys[k]+i*NLIMBS , tmp );
        bls12_381_Fr_mont_add_inplace( acc , tmp );
      }
    }
    bls12_381_Fr_mont_copy( acc, tgt+i*NLIMBS );
  }
}

// Multiply two polynomials, naive algorithm. 
// Requires a target buffer of size `(n1+n2-1)` (!)
void bls12_381_poly_mont_mul_naive( int  n1, const uint64_t *src1
                   , int  n2, const uint64_t *src2
                   ,                uint64_t *tgt ) {

  int N = n1+n2-1;
  for(int k=0; k<N; k++) {
    uint64_t acc[NLIMBS];
    bls12_381_Fr_mont_set_zero( acc );
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
      bls12_381_Fr_mont_mul( SRC1(i) , SRC2(j) , tmp );
      bls12_381_Fr_mont_add_inplace( acc, tmp );      
    }
    bls12_381_Fr_mont_copy( acc, TGT(k) );
  }
}

// evaluate a polynomial at a single point
void bls12_381_poly_mont_eval_at( int  n1, const uint64_t *src1, const uint64_t *loc, uint64_t *tgt ) {
  uint64_t run[NLIMBS];               // x^i
  bls12_381_Fr_mont_set_zero(tgt);
  bls12_381_Fr_mont_set_one (run);
  for(int i=0; i<n1; i++) {
    uint64_t tmp[NLIMBS];
    if (i>0) { 
      bls12_381_Fr_mont_mul( SRC1(i) , run , tmp );
      bls12_381_Fr_mont_add_inplace( tgt, tmp ); 
    }
    else {
      // constant term
      bls12_381_Fr_mont_copy( SRC1(i) , tgt );
    }
    if (i < n1-1) {
      bls12_381_Fr_mont_mul_inplace( run, loc );
    }
  }
}


// polynomial long division
// allocate at least `deg(p) - deg(q) + 1` field elements for the quotient
// and at least `deg(q)` for the remainder
void bls12_381_poly_mont_long_div( int n1, const uint64_t *src1, int n2, const uint64_t *src2, int nquot, uint64_t *quot, int nrem, uint64_t *rem ) {
  int deg_p = bls12_381_poly_mont_degree( n1, src1 );
  int deg_q = bls12_381_poly_mont_degree( n2, src2 );
  assert( (!quot) || (nquot >= deg_p - deg_q + 1) );
  assert( (!rem ) || (nrem  >= deg_q)             );

  if (deg_q < 0) {
    // division by zero
    if (quot) { for(int j=0; j<nquot; j++) { bls12_381_Fr_mont_set_zero( QUOT(j) ); } }
    if (rem ) { for(int j=0; j<nrem ; j++) { bls12_381_Fr_mont_set_zero( REM(j)  ); } }
    return;
  }

  if (deg_p < deg_q) {
    // quotient == 0
    if (quot) { for(int j=0; j<nquot; j++) { bls12_381_Fr_mont_set_zero( QUOT(j) ); } }
    if (rem ) {
      for(int j=deg_p+1; j<nrem; j++) { bls12_381_Fr_mont_set_zero( REM(j) ); }
      assert( nrem >= deg_p+1 ); 
      memcpy( rem, src1, 8*(deg_p+1)*NLIMBS );
    }
    return;
  }

  if (quot) { for(int j=MAX(0,deg_p-deg_q+1); j<nquot; j++) { bls12_381_Fr_mont_set_zero( QUOT(j) ); } }
  if (rem ) { for(int j=MAX(0,deg_q        ); j<nrem ; j++) { bls12_381_Fr_mont_set_zero( REM(j)  ); } }

  uint64_t *tgt = malloc( 8*(deg_p+1)*NLIMBS );
  assert( tgt != 0 );
  memcpy( tgt, src1, 8*(deg_p+1)*NLIMBS );

  uint64_t lead_inv[NLIMBS];
  bls12_381_Fr_mont_inv( SRC2(deg_q) , lead_inv );

  for(int k=deg_p; k>=deg_q; k--) {
    uint64_t scl[NLIMBS];
    bls12_381_Fr_mont_mul( TGT(k) , lead_inv , scl );
    for(int i=0; i<=deg_q; i++) {
      uint64_t tmp[NLIMBS];
      bls12_381_Fr_mont_mul( SRC2(i) , scl , tmp );
      bls12_381_Fr_mont_sub_inplace( TGT(k-deg_q+i) , tmp );
    }
    if (quot) { bls12_381_Fr_mont_copy( scl , QUOT(k-deg_q) ); } 
  }

  if (rem) { memcpy( rem , tgt , 8*NLIMBS * MIN(deg_p+1,deg_q) ); } 

  free(tgt);
}

// polynomial quotient
// allocate at least `deg(p) - deg(q) + 1` field elements for quotient
void bls12_381_poly_mont_quot( int n1, const uint64_t *src1, int n2, const uint64_t *src2, int nquot, uint64_t *quot ) {
  bls12_381_poly_mont_long_div( n1, src1, n2, src2, nquot, quot, 0, 0 );
}

// polynomial remainder
// allocate at least `deg(q)` field elements for the remainder
void bls12_381_poly_mont_rem( int n1, const uint64_t *src1, int n2, const uint64_t *src2, int nrem, uint64_t *rem ) {
  bls12_381_poly_mont_long_div( n1, src1, n2, src2, 0, 0, nrem, rem );
}


// divide by the vanishing polynomial of a coset `(x^n - eta)`
// This should be much faster than the general-purpose long division
// Remark: the case `eta = 1` corresponds to a subgroup
// allocate at least `deg(p) - n + 1` field elements for the quotient
// and at least `n` for the remainder
void bls12_381_poly_mont_div_by_vanishing( int n1, const uint64_t *src1, int expo_n, const uint64_t *eta, int nquot, uint64_t *quot, int nrem, uint64_t *rem ) {
  int deg_p = bls12_381_poly_mont_degree( n1, src1 );
  int n = expo_n;
  assert( quot );  // NOTE: quot cannot be NULL for this routine (or maybe we could allocate a temp buffer in that case?)
  assert( (!quot) || (nquot >= deg_p - n + 1) );
  assert( (!rem ) || (nrem  >= n)             );
  assert( n >= 1 );

  if (deg_p < n) {
    // quotient == 0
    if (quot) { for(int j=0; j<nquot; j++) { bls12_381_Fr_mont_set_zero( QUOT(j) ); } }
    if (rem ) {
      for(int j=deg_p+1; j<nrem; j++) { bls12_381_Fr_mont_set_zero( REM(j) ); }
      assert( nrem >= deg_p+1 ); 
      memcpy( rem, src1, 8*(deg_p+1)*NLIMBS );
    }
    return;
  }

  if (quot) { for(int j=MAX(0,deg_p-n+1); j<nquot; j++) { bls12_381_Fr_mont_set_zero( QUOT(j) ); } }
  if (rem ) { for(int j=MAX(0,n        ); j<nrem ; j++) { bls12_381_Fr_mont_set_zero( REM(j)  ); } }

  if (bls12_381_Fr_mont_is_one(eta)) {
    // 
    // eta = 1, we don't need to multiply by it
    // 
    for(int j=deg_p-n; j>=0; j--) {
      if (j+n <= deg_p-n) {
        // as[j+n] + bs[j+n]
        bls12_381_Fr_mont_add( SRC1(j+n) , QUOT(j+n) , QUOT(j) );
      }
      else {
        // bs[j+n] is zero
        bls12_381_Fr_mont_copy( SRC1(j+n) , QUOT(j) );
      }
    }
    if (rem) {
      for(int j=0; j<n; j++) {
        if (j <= deg_p-n) {
          // as[j] + bs[j]
          bls12_381_Fr_mont_add( SRC1(j) , QUOT(j) , REM(j) );
        }
        else {
          // bs[j] is zero
          bls12_381_Fr_mont_copy( SRC1(j) , REM(j) );
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
        bls12_381_Fr_mont_mul( QUOT(j+n) , eta , tmp );
        bls12_381_Fr_mont_add( SRC1(j+n) , tmp , QUOT(j) );
      }
      else {
        // bs[j+n] is zero
        bls12_381_Fr_mont_copy( SRC1(j+n) , QUOT(j) );
      }
    }
    if (rem) {
      for(int j=0; j<n; j++) {
        if (j <= deg_p-n) {
          uint64_t tmp[NLIMBS];
          // as[j] + eta * bs[j]
          bls12_381_Fr_mont_mul( QUOT(j) , eta , tmp );
          bls12_381_Fr_mont_add( SRC1(j) , tmp , REM(j) );
        }
        else {
          // bs[j] is zero
          bls12_381_Fr_mont_copy( SRC1(j) , REM(j) );
        }
      }
    }
  }
}

// divide by the vanishing polynomial `x^n - eta`
// returns True if the remainder is zero
// TODO: this could be implemented with no allocation, but i don't want to copy-paste the whole code right now
uint8_t bls12_381_poly_mont_quot_by_vanishing( int n1, const uint64_t *src1, int expo_n, const uint64_t *eta, int nquot, uint64_t *quot ) {
  int nrem = expo_n;
  uint64_t *rem = malloc( 8*NLIMBS*nrem );
  assert( rem != 0 );
  bls12_381_poly_mont_div_by_vanishing( n1, src1, expo_n, eta, nquot, quot, nrem, rem );
  int ok = 1;
  for(int j=0; j<nrem; j++) {
    if (!bls12_381_Fr_mont_is_zero(REM(j))) { ok = 0; break; }
  }
  free(rem);
  return ok;
}


// -----------------------------------------------------------------------------

void bls12_381_poly_mont_ntt_forward_noalloc(int m, int src_stride, const uint64_t *gpows, const uint64_t *src, uint64_t *buf, uint64_t *tgt) {

  if (m==0) {
    bls12_381_Fr_mont_copy( src, tgt );
    return;
  }

  if (m==1) {
    // N = 2
    bls12_381_Fr_mont_add( src , src + src_stride*NLIMBS , tgt          );    // x + y
    bls12_381_Fr_mont_sub( src , src + src_stride*NLIMBS , tgt + NLIMBS );    // x - y
    return;
  }

  else {
    int N     = (1<< m   );
    int halfN = (1<<(m-1));

    bls12_381_poly_mont_ntt_forward_noalloc( m-1 , src_stride<<1 , gpows , src                     , buf + N*NLIMBS , buf                );
    bls12_381_poly_mont_ntt_forward_noalloc( m-1 , src_stride<<1 , gpows , src + src_stride*NLIMBS , buf + N*NLIMBS , buf + halfN*NLIMBS );

    for(int j=0; j<halfN; j++) {
      const uint64_t *gpow = gpows + j*src_stride*NLIMBS;
      bls12_381_Fr_mont_mul ( buf + (j+halfN)*NLIMBS , gpow , tgt +  j       *NLIMBS );  //   g*v[k]
      bls12_381_Fr_mont_neg ( tgt +  j       *NLIMBS ,        tgt + (j+halfN)*NLIMBS );  // - g*v[k]
      bls12_381_Fr_mont_add_inplace( tgt +  j       *NLIMBS , buf + j*NLIMBS );          // u[k] + g*v[k]
      bls12_381_Fr_mont_add_inplace( tgt + (j+halfN)*NLIMBS , buf + j*NLIMBS );          // u[k] - g*v[k]
    }
  }
}

// forward number-theoretical transform (evaluation of a polynomial)
// `src` and `tgt` should be `N = 2^m` sized arrays of field elements
// `gen` should be the generator of the multiplicative subgroup sized `N`
void bls12_381_poly_mont_ntt_forward(int m, const uint64_t *gen, const uint64_t *src, uint64_t *tgt) {
  int N     = (1<<m);
  int halfN = (N>>1);
  
  // precalculate [1,g,g^2,g^3...]
  uint64_t *gpows = malloc( 8*NLIMBS * halfN );
  assert( gpows != 0 );
  uint64_t x[NLIMBS];
  bls12_381_Fr_mont_set_one( gpows );
  bls12_381_Fr_mont_copy( gen, gpows + NLIMBS );
  bls12_381_Fr_mont_copy( gen, x );
  for(int i=2; i<halfN; i++) {
    bls12_381_Fr_mont_mul_inplace(x, gen);
    bls12_381_Fr_mont_copy( x , gpows + (i*NLIMBS) );
  }
  
  uint64_t *buf = malloc( 8*NLIMBS * (2*N) );
  assert( buf != 0 );
  bls12_381_poly_mont_ntt_forward_noalloc( m, 1, gpows, src, buf, tgt);
  free(buf);
  free(gpows);
}

// it's like `ntt_forward` but we pre-multiply the coefficients with `eta^k`
// resulting in evaluating f(eta*x) instead of f(x)
void bls12_381_poly_mont_ntt_forward_shifted(const uint64_t *eta, int m, const uint64_t *gen, const uint64_t *src, uint64_t *tgt) {
  int N = (1<<m);
  uint64_t *shifted = malloc( 8*NLIMBS*N );
  assert( shifted != 0 );
  uint64_t x[NLIMBS];
  bls12_381_Fr_mont_set_one( x );
  for(int i=0; i<N; i++) {
    bls12_381_Fr_mont_mul( src + i*NLIMBS, x, shifted + i*NLIMBS );
    bls12_381_Fr_mont_mul_inplace(x, eta);
  }
  bls12_381_poly_mont_ntt_forward( m, gen, shifted, tgt );
  free(shifted);
}

// it's like `ntt_forward` but asymmetric, evaluating on a larger target subgroup
void bls12_381_poly_mont_ntt_forward_asymmetric(int m_src, int m_tgt, const uint64_t *gen_src, const uint64_t *gen_tgt, const uint64_t *src, uint64_t *tgt) {
  assert( m_tgt >= m_src );
  int N_src = (1 << m_src);
  int N_tgt = (1 << m_tgt);
  int halfN_src = (N_src >> 1);
  int K = (1 << (m_tgt - m_src));
  
  // precalculate [1,g,g^2,g^3...]
  uint64_t *gpows = malloc( 8*NLIMBS * halfN_src );
  assert( gpows != 0 );
  uint64_t x[NLIMBS];
  bls12_381_Fr_mont_set_one( gpows );
  bls12_381_Fr_mont_copy( gen_src, gpows + NLIMBS );
  bls12_381_Fr_mont_copy( gen_src, x );
  for(int i=2; i<halfN_src; i++) {
    bls12_381_Fr_mont_mul_inplace(x, gen_src);
    bls12_381_Fr_mont_copy( x , gpows + (i*NLIMBS) );
  }
  
  uint64_t *shifted = malloc( 8*NLIMBS * N_src );
  assert( shifted != 0 );
  
  uint64_t *buf = malloc( 8*NLIMBS * (2*N_src) );
  assert( buf != 0 );
  
  // temporary target buffer (we could replace this by adding `tgt_stride`)
  uint64_t *tgt_small = malloc( 8*NLIMBS * N_src );
  assert( tgt_small != 0 );
  
  // eta will be the shift
  uint64_t eta[NLIMBS];
  bls12_381_Fr_mont_set_one( eta );
  
  for(int k=0; k<K; k++) {
    if (k==0) {
      memcpy( shifted, src, N_src * NLIMBS * 8 );
    }
    else {
      bls12_381_Fr_mont_mul_inplace( eta , gen_tgt );
      
      uint64_t x[NLIMBS];
      bls12_381_Fr_mont_set_one( x );
      for(int i=0; i<N_src; i++) {
        bls12_381_Fr_mont_mul( src + i*NLIMBS, x, shifted + i*NLIMBS );
        bls12_381_Fr_mont_mul_inplace(x, eta);
      }
    }
    
    bls12_381_poly_mont_ntt_forward_noalloc( m_src, 1, gpows, shifted, buf, tgt_small);
    
    uint64_t *p = tgt_small;
    uint64_t *q = tgt + k*NLIMBS;
    int tgt_stride = NLIMBS*K;
    for(int i=0; i<N_src; i++) {
      memcpy( q, p, NLIMBS*8 );
      p += NLIMBS;
      q += tgt_stride;
    }
    
  }
  
  free(tgt_small);
  free(buf);
  free(gpows);
  free(shifted);
}


// -----------------------------------------------------------------------------
 
// inverse of 2
const uint64_t bls12_381_poly_mont_oneHalf[4] = { 0x00000000ffffffff, 0xac425bfd0001a401, 0xccc627f7f65e27fa, 0x0c1258acd66282b7 };

void bls12_381_poly_mont_ntt_inverse_noalloc(int m, int tgt_stride, const uint64_t *gpows, const uint64_t *src, uint64_t *buf, uint64_t *tgt) {

  if (m==0) {
    bls12_381_Fr_mont_copy( src, tgt );
    return;
  }

  if (m==1) {
    // N = 2
    bls12_381_Fr_mont_add( src , src + NLIMBS , tgt                     );   // x + y
    bls12_381_Fr_mont_sub( src , src + NLIMBS , tgt + tgt_stride*NLIMBS );   // x - y
    bls12_381_Fr_mont_div_by_2_inplace( tgt                    );      // (x + y)/2
    bls12_381_Fr_mont_div_by_2_inplace( tgt + tgt_stride*NLIMBS);      // (x - y)/2
    return;
  }

  else {
    int N     = (1<< m   );
    int halfN = (1<<(m-1));

    for(int j=0; j<halfN; j++) {
      const uint64_t *gpow = gpows + j*tgt_stride*NLIMBS;
      bls12_381_Fr_mont_add( src +  j* NLIMBS , src + (j+halfN)*NLIMBS , buf + j        *NLIMBS  );    // x + y
      bls12_381_Fr_mont_sub( src +  j* NLIMBS , src + (j+halfN)*NLIMBS , buf + (j+halfN)*NLIMBS  );    // x - y
      bls12_381_Fr_mont_div_by_2_inplace( buf + j        *NLIMBS );           // (x + y) /  2
      bls12_381_Fr_mont_mul_inplace(      buf + (j+halfN)*NLIMBS , gpow );    // (x - y) / (2*g^k)
    }

    bls12_381_poly_mont_ntt_inverse_noalloc( m-1 , tgt_stride<<1 , gpows , buf                , buf + N*NLIMBS , tgt                     );
    bls12_381_poly_mont_ntt_inverse_noalloc( m-1 , tgt_stride<<1 , gpows , buf + halfN*NLIMBS , buf + N*NLIMBS , tgt + tgt_stride*NLIMBS );
  }
}

// inverse number-theoretical transform (interpolation of a polynomial)
// `src` and `tgt` should be `N = 2^m` sized arrays of field elements
// `gen` should be the generator of the multiplicative subgroup sized `N`
void bls12_381_poly_mont_ntt_inverse(int m, const uint64_t *gen, const uint64_t *src, uint64_t *tgt) {
  int N = (1<<m);
  int halfN = (N>>1);
  
  // precalculate [1/2,g^{-1}/2,g^{-2}/2,g^{-3}/2...]
  uint64_t *gpows = malloc( 8*NLIMBS * halfN );
  assert( gpows != 0 );
  uint64_t x[NLIMBS];
  uint64_t ginv[NLIMBS];
  bls12_381_Fr_mont_inv( gen , ginv );                    // gen^-1
  bls12_381_Fr_mont_copy(bls12_381_poly_mont_oneHalf , x);    // = 1/2
  for(int i=0; i<halfN; i++) {
    bls12_381_Fr_mont_copy( x , gpows + (i*NLIMBS) );
    if (i < halfN-1) bls12_381_Fr_mont_mul_inplace(x, ginv);
  }
  
  uint64_t *buf = malloc( 8*NLIMBS * (2*N) );
  assert( buf !=0 );
  bls12_381_poly_mont_ntt_inverse_noalloc( m, 1, gpows, src, buf, tgt );
  free(buf);
  free(gpows);
}

// it's like `ntt_inverse` but we post-multiply the resulting coefficients with `eta^k`
// resulting in interpolating an f such that f(eta^-1 * omega^k) = y_k
void bls12_381_poly_mont_ntt_inverse_shifted(const uint64_t *eta, int m, const uint64_t *gen, const uint64_t *src, uint64_t *tgt) {
  int N = (1<<m);
  uint64_t *unshifted = malloc( 8*NLIMBS*N );
  assert( unshifted != 0 );
  bls12_381_poly_mont_ntt_inverse( m, gen, src, unshifted );
  uint64_t x[NLIMBS];
  bls12_381_Fr_mont_set_one( x );
  for(int i=0; i<N; i++) {
    bls12_381_Fr_mont_mul( unshifted + i*NLIMBS, x, tgt + i*NLIMBS );
    bls12_381_Fr_mont_mul_inplace(x, eta);
  }
  free(unshifted);
}

// -----------------------------------------------------------------------------



// -----------------------------------------------------------------------------
// RADIX-4 version of forward NTT

// helper function computing (+-A+-B+-C+-D)
void bls12_381_Fr_mont_signed_sum_4( int signbits, const uint64_t *A, const uint64_t *B, const uint64_t *C, const uint64_t *D, uint64_t *tgt) {
  if (signbits & 0x01) bls12_381_Fr_mont_neg ( A , tgt );
                  else bls12_381_Fr_mont_copy( A , tgt );
  if (signbits & 0x02) bls12_381_Fr_mont_sub_inplace( tgt , B );
                  else bls12_381_Fr_mont_add_inplace( tgt , B );
  if (signbits & 0x04) bls12_381_Fr_mont_sub_inplace( tgt , C );
                  else bls12_381_Fr_mont_add_inplace( tgt , C );
  if (signbits & 0x08) bls12_381_Fr_mont_sub_inplace( tgt , D );
                  else bls12_381_Fr_mont_add_inplace( tgt , D );
}

void bls12_381_poly_mont_ntt_forward_radix4_noalloc(int m, int src_stride, const uint64_t *complex_i, const uint64_t *gpows, const uint64_t *src, uint64_t *buf, uint64_t *tgt) {

  if (m==0) {
    bls12_381_Fr_mont_copy( src, tgt );
    return;
  }

  if (m==1) {
    // N = 2
    // omega = -1
    bls12_381_Fr_mont_add( src , src + src_stride*NLIMBS , tgt          );    // x + y
    bls12_381_Fr_mont_sub( src , src + src_stride*NLIMBS , tgt + NLIMBS );    // x - y
    return;
  }

  if (m==2) {
    // N = 4
    // omega = i := sqrt(-1)
    // tgt[0] = + a +   b + c +   d
    // tgt[1] = + a + i*b - c - i*d
    // tgt[2] = + a -   b + c -   d
    // tgt[3] = + a - i*b - c + i*d
    const uint64_t *A = src;
    const uint64_t *B = src + src_stride  *NLIMBS;
    const uint64_t *C = src + src_stride*2*NLIMBS;
    const uint64_t *D = src + src_stride*3*NLIMBS;
    uint64_t iB[NLIMBS];
    uint64_t iD[NLIMBS];
    bls12_381_Fr_mont_mul( B , complex_i , iB );
    bls12_381_Fr_mont_mul( D , complex_i , iD );
    bls12_381_Fr_mont_signed_sum_4( 0b0000 , A ,  B , C ,  D  , tgt            );    // + a +   b + c +   d
    bls12_381_Fr_mont_signed_sum_4( 0b1100 , A , iB , C , iD  , tgt +   NLIMBS );    // + a + i*b - c - i*d
    bls12_381_Fr_mont_signed_sum_4( 0b1010 , A ,  B , C ,  D  , tgt + 2*NLIMBS );    // + a -   b + c -   d
    bls12_381_Fr_mont_signed_sum_4( 0b0110 , A , iB , C , iD  , tgt + 3*NLIMBS );    // + a - i*b - c + i*d
    return;
  }

  else {
    int N      = (1<< m   );
    int quartN = (1<<(m-2));
    
    bls12_381_poly_mont_ntt_forward_radix4_noalloc( m-2 , src_stride<<2 , complex_i , gpows , src                       , buf + N*NLIMBS , buf                   );
    bls12_381_poly_mont_ntt_forward_radix4_noalloc( m-2 , src_stride<<2 , complex_i , gpows , src + src_stride  *NLIMBS , buf + N*NLIMBS , buf + quartN  *NLIMBS );
    bls12_381_poly_mont_ntt_forward_radix4_noalloc( m-2 , src_stride<<2 , complex_i , gpows , src + src_stride*2*NLIMBS , buf + N*NLIMBS , buf + quartN*2*NLIMBS );
    bls12_381_poly_mont_ntt_forward_radix4_noalloc( m-2 , src_stride<<2 , complex_i , gpows , src + src_stride*3*NLIMBS , buf + N*NLIMBS , buf + quartN*3*NLIMBS );
    
    for(int j=0; j<quartN; j++) {
      const uint64_t *A;
      uint64_t  B[NLIMBS];
      uint64_t  C[NLIMBS];
      uint64_t  D[NLIMBS];
      uint64_t  iB[NLIMBS];
      uint64_t  iD[NLIMBS];
      A =                    buf +  j          *NLIMBS;
      bls12_381_Fr_mont_mul( buf + (j+  quartN)*NLIMBS , gpows +   j*src_stride*NLIMBS , B );  
      bls12_381_Fr_mont_mul( buf + (j+2*quartN)*NLIMBS , gpows + 2*j*src_stride*NLIMBS , C );  
      bls12_381_Fr_mont_mul( buf + (j+3*quartN)*NLIMBS , gpows + 3*j*src_stride*NLIMBS , D );  
      bls12_381_Fr_mont_mul( B , complex_i , iB );
      bls12_381_Fr_mont_mul( D , complex_i , iD );
      bls12_381_Fr_mont_signed_sum_4( 0b0000 , A ,  B , C ,  D , tgt + (j         )*NLIMBS );    // + A +   B + C +   D
      bls12_381_Fr_mont_signed_sum_4( 0b1100 , A , iB , C , iD , tgt + (j+  quartN)*NLIMBS );    // + A + i*B - C - i*D
      bls12_381_Fr_mont_signed_sum_4( 0b1010 , A ,  B , C ,  D , tgt + (j+2*quartN)*NLIMBS );    // + A -   B + C -   D
      bls12_381_Fr_mont_signed_sum_4( 0b0110 , A , iB , C , iD , tgt + (j+3*quartN)*NLIMBS );    // + A - i*B - C + i*D
    }
  }
}

// forward number-theoretical transform (evaluation of a polynomial)
// `src` and `tgt` should be `N = 2^m` sized arrays of field elements
// `gen` should be the generator of the multiplicative subgroup sized `N`
void bls12_381_poly_mont_ntt_forward_radix4(int m, const uint64_t *gen, const uint64_t *src, uint64_t *tgt) {
  int N       = (1<<m);
  int halfN   = (N>>1);
  int quartN  = (N>>2);
  
  // precalculate [1,g,g^2,g^3...g^(N-1)]
  uint64_t *gpows = malloc( 8*NLIMBS * N );
  assert( gpows != 0 );
  uint64_t x[NLIMBS];
  bls12_381_Fr_mont_set_one( x );
  for(int i=0; i<halfN; i++) {
    bls12_381_Fr_mont_copy( x , gpows + ( i       *NLIMBS) );
    bls12_381_Fr_mont_neg ( x , gpows + ((i+halfN)*NLIMBS) );
    bls12_381_Fr_mont_mul_inplace(x, gen);
  }
  uint64_t *complex_i = gpows + quartN*NLIMBS;
  
  uint64_t *buf = malloc( 8*NLIMBS * (2*N) );
  assert( buf != 0 );
  bls12_381_poly_mont_ntt_forward_radix4_noalloc( m, 1, complex_i, gpows, src, buf, tgt);
  free(buf);
  free(gpows);
}


/*
void bls12_381_poly_mont_signed_rotation_to_right( int N, const uint64_t *src, uint64_t *tgt) {
  memcpy( tgt + NLIMBS , src , (N-1)*NLIMBS*8 );
  bls12_381_Fr_mont_neg( src + (N-1)*NLIMBS , tgt );
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
void bls12_381_poly_mont_negacyclic_conv_Karatsuba_CRT( int logN, int logStrideIn, const uint64_t *src1, const uint64_t *src2, uint64_t *tgt) {
  if (logN==0) {
    bls12_381_Fr_mont_mul( src1 , src2 , tgt );
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
    
    bls12_381_poly_mont_negacyclic_conv_Karatsuba_CRT( logN-1 , logStrideIn+1 , src1        , src2        , p0 );  // evens
    bls12_381_poly_mont_negacyclic_conv_Karatsuba_CRT( logN-1 , logStrideIn+1 , src1 + step , src2 + step , p1 );  // odds 
    
    for(int i=0; i<halfN; i++) {
      bls12_381_Fr_mont_add( src1 + (2*i)*step , src1 + (2*i+1)*step , tmp1 + i*NLIMBS );   // f_even + f_odd
      bls12_381_Fr_mont_add( src2 + (2*i)*step , src2 + (2*i+1)*step , tmp2 + i*NLIMBS );   // g_even + g_odd
    }
    bls12_381_poly_mont_negacyclic_conv_Karatsuba_CRT( logN-1 , 0 , tmp1 , tmp2 , p2 );  // mixed
    
    // out_evens := p0 + signedRotRight(p1)
    bls12_381_Fr_mont_sub( p0 , p1 + (halfN-1)*NLIMBS , tgt );
    for(int i=1; i<halfN; i++) {
      bls12_381_Fr_mont_add( p0 + i*NLIMBS , p1 + (i-1)*NLIMBS , tgt + (2*i)*NLIMBS );
    }
    // out_odds := p2 - p0 - p1
    for(int i=0; i<halfN; i++) {
      uint64_t p0_plus_p1[NLIMBS];
      bls12_381_Fr_mont_add( p0 + i*NLIMBS , p1 + i*NLIMBS , p0_plus_p1 );
      bls12_381_Fr_mont_sub( p2 + i*NLIMBS , p0_plus_p1 , tgt + (2*i+1)*NLIMBS );
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
void bls12_381_poly_mont_cyclic_conv_Karatsuba_CRT(int logN, const uint64_t *src1, const uint64_t *src2, uint64_t *tgt) {
  if (logN==0) {
    bls12_381_Fr_mont_mul( src1 , src2 , tgt );
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
      bls12_381_Fr_mont_add( src1 + i*NLIMBS , src1 + (i+halfN)*NLIMBS , tmp1 + i*NLIMBS );   // (flo .+. fhi)
      bls12_381_Fr_mont_add( src2 + i*NLIMBS , src2 + (i+halfN)*NLIMBS , tmp2 + i*NLIMBS );   // (glo .+. ghi)
    }
    bls12_381_poly_mont_cyclic_conv_Karatsuba_CRT( logN-1 , tmp1 , tmp2 , posi );
    
    for(int i=0; i<halfN; i++) {
      bls12_381_Fr_mont_sub( src1 + i*NLIMBS , src1 + (i+halfN)*NLIMBS , tmp1 + i*NLIMBS );   // (flo .-. fhi)
      bls12_381_Fr_mont_sub( src2 + i*NLIMBS , src2 + (i+halfN)*NLIMBS , tmp2 + i*NLIMBS );   // (glo .-. ghi)
    }
    bls12_381_poly_mont_negacyclic_conv_Karatsuba_CRT( logN-1 , 0 , tmp1 , tmp2 , nega );
    
    for(int i=0; i<halfN; i++) {
      bls12_381_Fr_mont_add( posi + i*NLIMBS , nega + i*NLIMBS , tgt +  i       *NLIMBS );
      bls12_381_Fr_mont_sub( posi + i*NLIMBS , nega + i*NLIMBS , tgt + (i+halfN)*NLIMBS );
      bls12_381_Fr_mont_div_by_2_inplace( tgt +  i       *NLIMBS );
      bls12_381_Fr_mont_div_by_2_inplace( tgt + (i+halfN)*NLIMBS );
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
void bls12_381_poly_mont_make_Rader_perm( int N , int64_t mgen , int64_t *tgt ) {
  int64_t a = 1;
  for(int i=0; i<N-1; i++) {
    tgt[i] = a;
    a = ((a * mgen) % N);
  }
}

void bls12_381_poly_mont_prepare_for_Rader( int N, const int64_t *perm, const uint64_t *omega, const uint64_t *src, uint64_t *tgt_Y0, uint64_t *tgt_XS, uint64_t *tgt_HS ) {

  uint64_t *omegaPows = (uint64_t*) malloc( 8 * N * NLIMBS );
  assert( omegaPows != 0);

  uint64_t acc[NLIMBS];
  bls12_381_Fr_mont_set_one( acc );
  for(int i=0; i<N; i++) {
    bls12_381_Fr_mont_copy( acc , omegaPows + i*NLIMBS );
    bls12_381_Fr_mont_mul_inplace( acc , omega );
  }
  
  for(int i=1; i<N; i++) {
     int64_t j = perm[i-1];
     bls12_381_Fr_mont_copy( omegaPows + j*NLIMBS , tgt_HS + (i-1)*NLIMBS );
     int64_t k;
     if (i==1) { k=1; } else { k=perm[N-i]; }
     bls12_381_Fr_mont_copy( src + k*NLIMBS , tgt_XS + (i-1)*NLIMBS );
  }
  
  // y0 = (a0 + sum xs)
  bls12_381_Fr_mont_copy( src , tgt_Y0 );
  for(int i=1; i<N; i++) {
  bls12_381_Fr_mont_add_inplace( tgt_Y0 , tgt_XS + (i-1)*NLIMBS );
  }
  
  free(omegaPows);
}

void bls12_381_poly_mont_postprocess_Rader( int N, const int64_t *perm, const uint64_t *Y0, const uint64_t *src, uint64_t *YS, uint64_t *tgt) {
  for(int i=1; i<N; i++) {
    int64_t j = perm[i-1];
    bls12_381_Fr_mont_copy( YS + (i-1)*NLIMBS , tgt + j*NLIMBS );  // scatter
    bls12_381_Fr_mont_add_inplace( tgt + j*NLIMBS , src );         // += a0
  }
  bls12_381_Fr_mont_copy( Y0 , tgt );
}
