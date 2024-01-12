
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

void bls12_381_poly_mont_ntt_forward_noalloc(int m, int src_stride, const uint64_t *gen, const uint64_t *src, uint64_t *buf, uint64_t *tgt) {

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

    uint64_t gpow[NLIMBS];
    bls12_381_Fr_mont_sqr( gen, gpow );  // gen^2
    
    bls12_381_poly_mont_ntt_forward_noalloc( m-1 , src_stride<<1 , gpow , src                     , buf + N*NLIMBS , buf                );
    bls12_381_poly_mont_ntt_forward_noalloc( m-1 , src_stride<<1 , gpow , src + src_stride*NLIMBS , buf + N*NLIMBS , buf + halfN*NLIMBS );

    bls12_381_Fr_mont_set_one(gpow);
    for(int j=0; j<halfN; j++) {
      bls12_381_Fr_mont_mul ( buf + (j+halfN)*NLIMBS , gpow , tgt +  j       *NLIMBS );  //   g*v[k]
      bls12_381_Fr_mont_neg ( tgt +  j       *NLIMBS ,        tgt + (j+halfN)*NLIMBS );  // - g*v[k]
      bls12_381_Fr_mont_add_inplace( tgt +  j       *NLIMBS , buf + j*NLIMBS );          // u[k] + g*v[k]
      bls12_381_Fr_mont_add_inplace( tgt + (j+halfN)*NLIMBS , buf + j*NLIMBS );          // u[k] - g*v[k]
      bls12_381_Fr_mont_mul_inplace( gpow , gen );      
    }
  }
}

// forward number-theoretical transform (evaluation of a polynomial)
// `src` and `tgt` should be `N = 2^m` sized arrays of field elements
// `gen` should be the generator of the multiplicative subgroup sized `N`
void bls12_381_poly_mont_ntt_forward(int m, const uint64_t *gen, const uint64_t *src, uint64_t *tgt) {
  int N = (1<<m);
  uint64_t *buf = malloc( 8*NLIMBS * (2*N) );
  assert( buf !=0 );
  bls12_381_poly_mont_ntt_forward_noalloc( m, 1, gen, src, buf, tgt);
  free(buf);
}



// -----------------------------------------------------------------------------
 
// inverse of 2
const uint64_t bls12_381_poly_mont_oneHalf[4] = { 0x00000000ffffffff, 0xac425bfd0001a401, 0xccc627f7f65e27fa, 0x0c1258acd66282b7 };

void bls12_381_poly_mont_ntt_inverse_noalloc(int m, int tgt_stride, const uint64_t *gen, const uint64_t *src, uint64_t *buf, uint64_t *tgt) {

  if (m==0) {
    bls12_381_Fr_mont_copy( src, tgt );
    return;
  }

  if (m==1) {
    // N = 2
    bls12_381_Fr_mont_add( src , src + NLIMBS , tgt                     );   // x + y
    bls12_381_Fr_mont_sub( src , src + NLIMBS , tgt + tgt_stride*NLIMBS );   // x - y
    bls12_381_Fr_mont_mul_inplace( tgt                     , bls12_381_poly_mont_oneHalf );      // (x + y)/2
    bls12_381_Fr_mont_mul_inplace( tgt + tgt_stride*NLIMBS , bls12_381_poly_mont_oneHalf );      // (x - y)/2
    return;
  }

  else {
  
    int N     = (1<< m   );
    int halfN = (1<<(m-1));

    uint64_t ginv[NLIMBS];
    bls12_381_Fr_mont_inv( gen , ginv );  // gen^-1

    uint64_t gpow[NLIMBS];    
    bls12_381_Fr_mont_copy(bls12_381_poly_mont_oneHalf , gpow);  // 1/2
    for(int j=0; j<halfN; j++) {
      bls12_381_Fr_mont_add( src +  j* NLIMBS , src + (j+halfN)*NLIMBS , buf + j        *NLIMBS  );    // x + y
      bls12_381_Fr_mont_sub( src +  j* NLIMBS , src + (j+halfN)*NLIMBS , buf + (j+halfN)*NLIMBS  );    // x - y
      bls12_381_Fr_mont_mul_inplace( buf + j        *NLIMBS , bls12_381_poly_mont_oneHalf  );    // (x + y) /  2
      bls12_381_Fr_mont_mul_inplace( buf + (j+halfN)*NLIMBS , gpow     );    // (x - y) / (2*g^k)
      bls12_381_Fr_mont_mul_inplace( gpow , ginv );      
    }

    bls12_381_Fr_mont_sqr( gen, gpow );  // gen^2
    bls12_381_poly_mont_ntt_inverse_noalloc( m-1 , tgt_stride<<1 , gpow , buf                , buf + N*NLIMBS , tgt                     );
    bls12_381_poly_mont_ntt_inverse_noalloc( m-1 , tgt_stride<<1 , gpow , buf + halfN*NLIMBS , buf + N*NLIMBS , tgt + tgt_stride*NLIMBS );

  }
}

// inverse number-theoretical transform (interpolation of a polynomial)
// `src` and `tgt` should be `N = 2^m` sized arrays of field elements
// `gen` should be the generator of the multiplicative subgroup sized `N`
void bls12_381_poly_mont_ntt_inverse(int m, const uint64_t *gen, const uint64_t *src, uint64_t *tgt) {
  int N = (1<<m);
  uint64_t *buf = malloc( 8*NLIMBS * (2*N) );
  assert( buf !=0 );
  bls12_381_poly_mont_ntt_inverse_noalloc( m, 1, gen, src, buf, tgt );
  free(buf);
}

// -----------------------------------------------------------------------------
 
