
// vectors of `Fr` elements
//
// NOTE: generated code, do not edit!

#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>

#include "bls12_381_arr_mont.h"
#include "bls12_381_Fr_mont.h"

#define ELEM_NWORDS 4

#define SRC1(i) ((src1) + (i)*ELEM_NWORDS)
#define SRC2(i) ((src2) + (i)*ELEM_NWORDS)
#define SRC3(i) ((src3) + (i)*ELEM_NWORDS)
#define TGT(i)  (( tgt) + (i)*ELEM_NWORDS)
#define TMP(i)  (( tmp) + (i)*ELEM_NWORDS)

//------------------------------------------------------------------------------


uint8_t bls12_381_arr_mont_is_valid ( int n, const uint64_t *src1 ) {
  uint8_t ok = 1;
  for(int i=0; i<n; i++) {
    if (!bls12_381_Fr_mont_is_valid( SRC1(i) )) { ok=0; break; }
  }
  return ok;
}

uint8_t bls12_381_arr_mont_is_zero  ( int n, const uint64_t *src1 ) {
  uint8_t ok = 1;
  for(int i=0; i<n; i++) {
    if (!bls12_381_Fr_mont_is_zero( SRC1(i) )) { ok=0; break; }
  }
  return ok;
}

// not that this looks very useful, whatever
uint8_t bls12_381_arr_mont_is_one  ( int n, const uint64_t *src1 ) {
  uint8_t ok = 1;
  for(int i=0; i<n; i++) {
    if (!bls12_381_Fr_mont_is_one( SRC1(i) )) { ok=0; break; }
  }
  return ok;
}

uint8_t bls12_381_arr_mont_is_equal ( int n, const uint64_t *src1, const uint64_t *src2 ) {
  uint8_t ok = 1;
  for(int i=0; i<n; i++) {
    if (!bls12_381_Fr_mont_is_equal( SRC1(i) , SRC2(i) )) { ok=0; break; }
  }
  return ok;
}

void bls12_381_arr_mont_set_zero ( int n, uint64_t *tgt ) {
  for(int i=0; i<n; i++) bls12_381_Fr_mont_set_zero( TGT(i) ); 
}

void bls12_381_arr_mont_set_one  ( int n, uint64_t *tgt ) {
  for(int i=0; i<n; i++) bls12_381_Fr_mont_set_one( TGT(i) ); 
}

void bls12_381_arr_mont_set_const( int n, const uint64_t *src , uint64_t *tgt ) {
  for(int i=0; i<n; i++) bls12_381_Fr_mont_copy( src , TGT(i) ); 
}

void bls12_381_arr_mont_copy ( int n, const uint64_t *src , uint64_t *tgt ) {
  memcpy( tgt, src, (8*ELEM_NWORDS)*n );
}


void bls12_381_arr_mont_append( int n1, int n2, const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  int N1 = n1 * ELEM_NWORDS;
  int N2 = n2 * ELEM_NWORDS;
  memcpy( tgt    , src1 , 8*N1 );
  memcpy( tgt+N1 , src2 , 8*N2 );
}

void bls12_381_arr_mont_from_std ( int n, const uint64_t *src1 , uint64_t *tgt ) {
  for(int i=0; i<n; i++) bls12_381_Fr_mont_from_std ( SRC1(i), TGT(i) ); 
}

void bls12_381_arr_mont_to_std ( int n, const uint64_t *src1 , uint64_t *tgt ) {
  for(int i=0; i<n; i++) bls12_381_Fr_mont_to_std ( SRC1(i), TGT(i) ); 
}


void bls12_381_arr_mont_neg ( int n, const uint64_t *src1, uint64_t *tgt ) {
  for(int i=0; i<n; i++) bls12_381_Fr_mont_neg( SRC1(i), TGT(i) ); 
}

void bls12_381_arr_mont_add ( int n, const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  for(int i=0; i<n; i++) bls12_381_Fr_mont_add( SRC1(i), SRC2(i), TGT(i) ); 
}

void bls12_381_arr_mont_sub ( int n, const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  for(int i=0; i<n; i++) bls12_381_Fr_mont_sub( SRC1(i), SRC2(i), TGT(i) ); 
}

void bls12_381_arr_mont_sqr ( int n, const uint64_t *src1, uint64_t *tgt ) {
  for(int i=0; i<n; i++) bls12_381_Fr_mont_sqr( SRC1(i), TGT(i) ); 
}

void bls12_381_arr_mont_mul ( int n, const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  for(int i=0; i<n; i++) bls12_381_Fr_mont_mul( SRC1(i), SRC2(i), TGT(i) ); 
}

void bls12_381_arr_mont_inv ( int n, const uint64_t *src1, uint64_t *tgt ) {
  bls12_381_Fr_mont_batch_inv( n, src1, tgt );
}

void bls12_381_arr_mont_div ( int n, const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  uint64_t *tmp = (uint64_t*) malloc( n*(8*ELEM_NWORDS) );
  assert( tmp != 0);
  bls12_381_Fr_mont_batch_inv( n, src2, tmp );
  for(int i=0; i<n; i++) bls12_381_Fr_mont_mul( SRC1(i), TMP(i), TGT(i) ); 
  free(tmp);
}

// computes the vector `A*B+C`
void bls12_381_arr_mont_mul_add ( int n, const uint64_t *src1, const uint64_t *src2, const uint64_t *src3, uint64_t *tgt ) {
  for(int i=0; i<n; i++) {
    bls12_381_Fr_mont_mul( SRC1(i), SRC2(i), TGT(i) ); 
    bls12_381_Fr_mont_add_inplace( TGT(i) , SRC3(i) ); 
  }
}

// computes the vector `A*B-C`
void bls12_381_arr_mont_mul_sub ( int n, const uint64_t *src1, const uint64_t *src2, const uint64_t *src3, uint64_t *tgt ) {
  for(int i=0; i<n; i++) {
    bls12_381_Fr_mont_mul( SRC1(i), SRC2(i), TGT(i) ); 
    bls12_381_Fr_mont_sub_inplace( TGT(i) , SRC3(i) ); 
  }
}

void bls12_381_arr_mont_neg_inplace ( int n, uint64_t *tgt ) {
  for(int i=0; i<n; i++) bls12_381_Fr_mont_neg_inplace( TGT(i) ); 
}

void bls12_381_arr_mont_add_inplace ( int n, uint64_t *tgt , const uint64_t *src2 ) {
  for(int i=0; i<n; i++) bls12_381_Fr_mont_add_inplace( TGT(i) , SRC2(i) ); 
}

void bls12_381_arr_mont_sub_inplace ( int n, uint64_t *tgt , const uint64_t *src2 ) {
  for(int i=0; i<n; i++) bls12_381_Fr_mont_sub_inplace( TGT(i) , SRC2(i) ); 
}

void bls12_381_arr_mont_sub_inplace_reverse ( int n, uint64_t *tgt , const uint64_t *src1 ) {
  for(int i=0; i<n; i++) bls12_381_Fr_mont_sub_inplace_reverse( TGT(i) , SRC1(i) ); 
}

void bls12_381_arr_mont_sqr_inplace ( int n, uint64_t *tgt ) {
  for(int i=0; i<n; i++) bls12_381_Fr_mont_sqr_inplace( TGT(i) );
}

void bls12_381_arr_mont_mul_inplace ( int n, uint64_t *tgt , const uint64_t *src2) {
  for(int i=0; i<n; i++) bls12_381_Fr_mont_mul_inplace( TGT(i) , SRC2(i) ); 
}

void bls12_381_arr_mont_inv_inplace ( int n, uint64_t *tgt ) {
  bls12_381_Fr_mont_batch_inv( n, tgt, tgt );   // batch_inv is inplace-safe
}

void bls12_381_arr_mont_div_inplace ( int n, uint64_t *tgt , const uint64_t *src2 ) {
  uint64_t *tmp = malloc( n*(8*ELEM_NWORDS) );
  bls12_381_Fr_mont_batch_inv( n, src2, tmp );
  for(int i=0; i<n; i++) bls12_381_Fr_mont_mul_inplace( TGT(i) , TMP(i) ); 
  free(tmp);
}


void bls12_381_arr_mont_dot_prod  ( int n, const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  uint64_t tmp[ELEM_NWORDS];
  uint64_t acc[ELEM_NWORDS];
  bls12_381_Fr_mont_set_zero( acc );
  for(int i=0; i<n; i++) {
    bls12_381_Fr_mont_mul( SRC1(i) , SRC2(i) , tmp );
    bls12_381_Fr_mont_add_inplace( acc, tmp );
  }
  bls12_381_Fr_mont_copy( acc, tgt );
}

// generate the vector `[ a*b^i | i<-[0..n-1] ]`
void bls12_381_arr_mont_powers ( int n, const uint64_t *coeffA, const uint64_t *coeffB, uint64_t *tgt ) {
  if (n==0) return;
  bls12_381_Fr_mont_copy( coeffA, TGT(0) );
  for(int i=1; i<n; i++) {
  bls12_381_Fr_mont_mul( TGT(i-1), coeffB, TGT(i) );
  }
}


void bls12_381_arr_mont_scale ( int n, const uint64_t *coeff, const uint64_t *src2, uint64_t *tgt ) {
  for(int i=0; i<n; i++) bls12_381_Fr_mont_mul( coeff, SRC2(i), TGT(i) ); 
}

void bls12_381_arr_mont_scale_inplace  ( int n, const uint64_t *coeff, uint64_t *tgt ) {
  for(int i=0; i<n; i++) bls12_381_Fr_mont_mul_inplace( TGT(i) , coeff ); 
}

void bls12_381_arr_mont_Ax_plus_y ( int n, const uint64_t *coeffA, const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  for(int i=0; i<n; i++) {
    bls12_381_Fr_mont_mul( coeffA, SRC1(i), TGT(i) ); 
    bls12_381_Fr_mont_add_inplace( TGT(i) , SRC2(i) ); 
  }
}

void bls12_381_arr_mont_Ax_plus_y_inplace ( int n, const uint64_t *coeffA, uint64_t *tgt , const uint64_t *src2 ) {
  for(int i=0; i<n; i++) {
    bls12_381_Fr_mont_mul_inplace( TGT(i) , coeffA  ); 
    bls12_381_Fr_mont_add_inplace( TGT(i) , SRC2(i) ); 
  }
}

void bls12_381_arr_mont_Ax_plus_By ( int n, const uint64_t *coeffA, const uint64_t *coeffB, const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  uint64_t tmp1[ELEM_NWORDS];
  uint64_t tmp2[ELEM_NWORDS];
  for(int i=0; i<n; i++) {
    bls12_381_Fr_mont_mul( coeffA , SRC1(i) , tmp1 );
    bls12_381_Fr_mont_mul( coeffB , SRC2(i) , tmp2 );
    bls12_381_Fr_mont_add( tmp1, tmp2, TGT(i) ); 
  }
}

void bls12_381_arr_mont_Ax_plus_By_inplace ( int n, const uint64_t *coeffA, const uint64_t *coeffB, uint64_t *tgt , const uint64_t *src2 ) {
  uint64_t tmp[ELEM_NWORDS];
  for(int i=0; i<n; i++) {
    bls12_381_Fr_mont_mul_inplace( TGT(i), coeffA  );
    bls12_381_Fr_mont_mul( coeffB , SRC2(i) , tmp );
    bls12_381_Fr_mont_add_inplace( TGT(i) , tmp );
  }
}
