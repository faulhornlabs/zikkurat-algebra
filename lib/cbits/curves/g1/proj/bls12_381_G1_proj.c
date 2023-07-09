
// elliptic curve "BLS12-381" in projective coordinates, Montgomery field representation
//
// NOTE: generated code, do not edit!

#include <string.h>
#include <stdint.h>
#include <x86intrin.h>

#include "bls12_381_G1_proj.h"
#include "bls12_381_G1_affine.h"
#include "bls12_381_p_mont.h"
#include "bls12_381_r_mont.h"

#define NLIMBS_P 6
#define NLIMBS_R 4

#define X1 (src1)
#define Y1 (src1 + 6)
#define Z1 (src1 + 12)

#define X2 (src2)
#define Y2 (src2 + 6)
#define Z2 (src2 + 12)

#define X3 (tgt)
#define Y3 (tgt + 6)
#define Z3 (tgt + 12)

// the generator of the subgroup G1
const uint64_t bls12_381_G1_proj_gen_G1[18] = { 0x5cb38790fd530c16, 0x7817fc679976fff5, 0x154f95c7143ba1c1, 0xf0ae6acdf3d0e747, 0xedce6ecc21dbf440, 0x120177419e0bfb75, 0xbaac93d50ce72271, 0x8c22631a7918fd8e, 0xdd595f13570725ce, 0x51ac582950405194, 0x0e1c8c3fad0059c0, 0x0bbc3efc5008a26a, 0x760900000002fffd, 0xebf4000bc40c0002, 0x5f48985753c758ba, 0x77ce585370525745, 0x5c071a97a256ec6d, 0x15f65ec3fa80e493 };

// the cofactor of the curve subgroup = 76329603384216526031706109802092473003
const uint64_t bls12_381_G1_proj_cofactor[6] = { 0x8c00aaab0000aaab, 0x396c8c005555e156, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000 };

//------------------------------------------------------------------------------

// scale a field element by A = 0
void bls12_381_G1_proj_scale_by_A(const uint64_t *src, uint64_t *tgt ) {
  memset( tgt, 0, 48 );
}

void bls12_381_G1_proj_scale_by_A_inplace( uint64_t *tgt ) {
  memset( tgt, 0, 48 );
}

// scale a field element by B = 4
void bls12_381_G1_proj_scale_by_B(const uint64_t *src, uint64_t *tgt ) {
  uint64_t tmp[6];
  bls12_381_p_mont_add( src, src, tmp );
  bls12_381_p_mont_add( tmp, tmp, tgt );
}

void bls12_381_G1_proj_scale_by_B_inplace( uint64_t *tgt ) {
  uint64_t tmp[6];
  bls12_381_p_mont_add( tgt, tgt, tmp );
  bls12_381_p_mont_add( tmp, tmp, tgt );
}

// scale a field element by (3*B) = 12
void bls12_381_G1_proj_scale_by_3B(const uint64_t *src, uint64_t *tgt ) {
  uint64_t tmp [NLIMBS_P];
  uint64_t tmp2[NLIMBS_P];
  bls12_381_p_mont_add( src, src, tmp );       // 2*B
  bls12_381_p_mont_add_inplace( tmp, tmp );    // 4*B
  bls12_381_p_mont_add( tmp, tmp, tmp2);       // 8*B
  bls12_381_p_mont_add( tmp, tmp2, tgt );      // 12*B
}

void bls12_381_G1_proj_scale_by_3B_inplace( uint64_t *tgt ) {
  bls12_381_G1_proj_scale_by_3B( tgt , tgt );
}

void bls12_381_G1_proj_normalize( const uint64_t *src1, uint64_t *tgt ) {
  uint64_t zinv[6];
  if (bls12_381_p_mont_is_zero( Z1 ) ) {
    // Z == 0, it must be the point at infinity
    memset( tgt, 0, 144 );
    bls12_381_p_mont_set_one( Y3 );
  }
  else {
    if (bls12_381_p_mont_is_one( Z1 )) {
      // already normalized
      if (tgt != src1) { memcpy( tgt, src1, 144 ); }
    }
    else {
      bls12_381_p_mont_inv( Z1, zinv );
      bls12_381_p_mont_mul( X1, zinv, X3 );
      bls12_381_p_mont_mul( Y1, zinv, Y3 );
      bls12_381_p_mont_set_one( Z3 );
    }
  }
}

void bls12_381_G1_proj_normalize_inplace( uint64_t *tgt ) {
  bls12_381_G1_proj_normalize( tgt, tgt );
}

// checks whether the underlying representation (projective coordinates) are the same
uint8_t bls12_381_G1_proj_is_same( const uint64_t *src1, const uint64_t *src2 ) {
  return ( bls12_381_p_mont_is_equal( X1, X2 ) &&
           bls12_381_p_mont_is_equal( Y1, Y2 ) &&
           bls12_381_p_mont_is_equal( Z1, Z2 ) );
}

// checks whether two curve points are equal
uint8_t bls12_381_G1_proj_is_equal( const uint64_t *src1, const uint64_t *src2 ) {
  uint64_t tmp1[18];
  uint64_t tmp2[18];
  bls12_381_G1_proj_normalize( src1, tmp1 );
  bls12_381_G1_proj_normalize( src2, tmp2 );
  return bls12_381_G1_proj_is_same( tmp1, tmp2 );
}

// converts from affine coordinates
void bls12_381_G1_proj_from_affine( const uint64_t *src1 , uint64_t *tgt ) {
  memcpy( tgt, src1, 96 );
  if (bls12_381_G1_affine_is_infinity( src1 )) {
    bls12_381_G1_proj_set_infinity( tgt );
  }
  else {
    bls12_381_p_mont_set_one( Z3 );
  }
}

// converts to affine coordinates
// remark: the point at infinity will result in the special string `0xffff...ffff`
void bls12_381_G1_proj_to_affine( const uint64_t *src1 , uint64_t *tgt ) {
  if (bls12_381_p_mont_is_zero( Z1 )) {
    // in the affine coordinate system, the point at infinity is represented by a hack
    // consisting all 0xff bytes (note that that's an invalid value for prime fields)
    memset( tgt, 0xff, 96 );
  }
  else {
    uint64_t zinv[6];
    bls12_381_p_mont_inv( Z1, zinv );
    bls12_381_p_mont_mul( X1, zinv, X3 );
    bls12_381_p_mont_mul( Y1, zinv, Y3 );
  }
}

void bls12_381_G1_proj_copy( const uint64_t *src1 , uint64_t *tgt ) {
  if (tgt != src1) { memcpy( tgt, src1, 144 ); }
}

uint8_t bls12_381_G1_proj_is_infinity ( const uint64_t *src1 ) {
  return ( ( bls12_381_p_mont_is_zero( Z1 )) &&
           (!bls12_381_p_mont_is_zero( Y1 )) &&
           ( bls12_381_p_mont_is_zero( X1 )) );
}

void bls12_381_G1_proj_set_infinity ( uint64_t *tgt ) {
  bls12_381_p_mont_set_zero( X3 );
  bls12_381_p_mont_set_one ( Y3 );
  bls12_381_p_mont_set_zero( Z3 );
}

// checks the curve equation
//   y^2*z == x^3 + A*x*z^2 + B*z^3
uint8_t bls12_381_G1_proj_is_on_curve ( const uint64_t *src1 ) {
  uint64_t  ZZ[6];
  uint64_t acc[6];
  uint64_t tmp[6];
  bls12_381_p_mont_sqr( Y1, acc );             // Y^2
  bls12_381_p_mont_mul_inplace( acc, Z1 );     // Y^2*Z
  bls12_381_p_mont_neg_inplace( acc );         // -Y^2*Z
  bls12_381_p_mont_sqr( X1, tmp );             // X^2
  bls12_381_p_mont_mul_inplace( tmp, X1 );     // X^3
  bls12_381_p_mont_add_inplace( acc, tmp );    // - Y^2*Z + X^3
  bls12_381_p_mont_sqr( Z1, ZZ );              // Z^2
  bls12_381_p_mont_mul( Z1, ZZ, tmp );          // Z^3
  bls12_381_G1_proj_scale_by_B_inplace( tmp );   // B*Z^3
  bls12_381_p_mont_add_inplace( acc, tmp );     // - Y^2*Z + X^3 + A*X*Z^2 + B*Z^3
  return (bls12_381_p_mont_is_zero( acc ) &&
           ( (!bls12_381_p_mont_is_zero( Z1 )) || 
             (!bls12_381_p_mont_is_zero( Y1 )) ) );
}

// checks whether the given point is in the subgroup G1
uint8_t bls12_381_G1_proj_is_in_subgroup ( const uint64_t *src1 ) {
  uint64_t tmp[18];
  if (!bls12_381_G1_proj_is_on_curve(src1)) {
    return 0;
  }
  else {
    bls12_381_G1_proj_scl_Fr( bls12_381_G1_proj_cofactor , src1 , tmp );
    return bls12_381_G1_proj_is_infinity( tmp );
  }
}

// negates an elliptic curve point
void bls12_381_G1_proj_neg( const uint64_t *src, uint64_t *tgt ) {
  if (tgt != src) { memcpy( tgt, src, 144 ); }
  bls12_381_p_mont_neg_inplace( Y3 );
}

// negates an elliptic curve point
void bls12_381_G1_proj_neg_inplace( uint64_t *tgt ) {
  bls12_381_p_mont_neg_inplace( Y3 );
}

// doubles an elliptic curve point
// https://hyperelliptic.org/EFD/g1p/auto-shortw-projective.html#doubling-dbl-2007-bl
void bls12_381_G1_proj_dbl( const uint64_t *src1, uint64_t *tgt ) {
  uint64_t  XX[6];
  uint64_t  ZZ[6];
  uint64_t   w[6];
  uint64_t   s[6];
  uint64_t  ss[6];
  uint64_t sss[6];
  uint64_t   R[6];
  uint64_t  RR[6];
  uint64_t   B[6];
  uint64_t   h[6];
  bls12_381_p_mont_sqr( X1 , XX );         // XX = X1^2
  bls12_381_p_mont_sqr( Z1 , ZZ );         // ZZ = Z1^2
  bls12_381_p_mont_add( XX, XX, w );       // w  = 2*XX
  bls12_381_p_mont_add_inplace( w, XX );   // w  = 3*XX
  bls12_381_p_mont_mul( Y1 , Z1 , s );         // s   = Y1*Z1
  bls12_381_p_mont_add_inplace( s, s );        // s   = 2*Y1*Z1
  bls12_381_p_mont_sqr( s , ss );              // ss  = s^2
  bls12_381_p_mont_mul( Y1 , s , R);           // R   = Y1*s
  bls12_381_p_mont_sqr( R , RR );              // RR  = R^2
  bls12_381_p_mont_add( X1 , R , B);           // B   = (X1+R)
  bls12_381_p_mont_sqr_inplace( B );           // B   = (X1+R)^2
  bls12_381_p_mont_sub_inplace( B , XX );      // B   = (X1+R)^2 - XX
  bls12_381_p_mont_sub_inplace( B , RR );      // B   = (X1+R)^2 - XX - RR
  bls12_381_p_mont_sqr( w , h );               // h   = w^2
  bls12_381_p_mont_sub_inplace( h , B );       // h   = w^2 - B
  bls12_381_p_mont_sub_inplace( h , B );       // h   = w^2 - 2*B
  bls12_381_p_mont_mul( s , ss , Z3 );         // Z3  = s^3
  bls12_381_p_mont_mul( h , s , X3 );          // X3  = h*s
  bls12_381_p_mont_sub( B , h , Y3 );          // Y3  = B-h
  bls12_381_p_mont_mul_inplace( Y3 , w  );     // Y3  = w*(B-h)
  bls12_381_p_mont_sub_inplace( Y3 , RR );     // Y3  = w*(B-h) - RR
  bls12_381_p_mont_sub_inplace( Y3 , RR );     // Y3  = w*(B-h) - 2*RR
}

// doubles an elliptic curve point
void bls12_381_G1_proj_dbl_inplace( uint64_t *tgt ) {
  bls12_381_G1_proj_dbl( tgt , tgt );
}

// adds two elliptic curve points, assuming A = 0
// https://hyperelliptic.org/EFD/g1p/auto-shortw-projective.html#addition-add-2015-rcb
void bls12_381_G1_proj_add( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  uint64_t t0[6];
  uint64_t t1[6];
  uint64_t t2[6];
  uint64_t t3[6];
  uint64_t t4[6];
  uint64_t t5[6];
  bls12_381_p_mont_mul( X1, X2, t0 );              // t0 = X1*X2
  bls12_381_p_mont_mul( Y1, Y2, t1 );              // t1 = Y1*Y2
  bls12_381_p_mont_mul( Z1, Z2, t2 );              // t2 = Z1*Z2
  bls12_381_p_mont_add( X1, Y1, t3 );              // t3 = X1+Y1
  bls12_381_p_mont_add( X2, Y2, t4 );              // t4 = X2+Y2
  bls12_381_p_mont_mul_inplace( t3, t4 );          // t3 = t3*t4
  bls12_381_p_mont_add( t0, t1, t4 );              // t4 = t0+t1
  bls12_381_p_mont_sub_inplace( t3 , t4 );         // t3 = t3-t4
  bls12_381_p_mont_add( X1, Z1, t4 );              // t4 = X1+Z1
  bls12_381_p_mont_add( X2, Z2, t5 );              // t5 = X2+Z2
  bls12_381_p_mont_mul_inplace( t4, t5 );          // t4 = t4*t5
  bls12_381_p_mont_add( t0, t2, t5 );              // t5 = t0+t2
  bls12_381_p_mont_sub_inplace( t4, t5 );          // t4 = t4-t5
  bls12_381_p_mont_add( Y1, Z1, t5 );              // t5 = Y1+Z1
  bls12_381_p_mont_add( Y2, Z2, X3 );              // X3 = Y2+Z2
  bls12_381_p_mont_mul_inplace( t5, X3 );          // t5 = t5*X3
  bls12_381_p_mont_add( t1, t2, X3 );              // X3 = t1+t2
  bls12_381_p_mont_sub_inplace( t5, X3 );          // t5 = t5-X3
  bls12_381_G1_proj_scale_by_3B( t2, X3 );          // X3 = b3*t2
  bls12_381_p_mont_copy( X3, Z3 );                 // Z3 = X3
  bls12_381_p_mont_sub( t1, Z3, X3 );              // X3 = t1-Z3
  bls12_381_p_mont_add_inplace( Z3, t1 );          // Z3 = t1+Z3
  bls12_381_p_mont_mul( X3, Z3, Y3 );              // Y3 = X3*Z3
  bls12_381_p_mont_add( t0, t0, t1 );              // t1 = t0+t0
  bls12_381_p_mont_add_inplace( t1, t0 );          // t1 = t1+t0
  bls12_381_G1_proj_scale_by_3B_inplace( t4 );      // t4 = b3*t4
  bls12_381_p_mont_mul( t1, t4, t0 );              // t0 = t1*t4
  bls12_381_p_mont_add_inplace( Y3, t0 );          // Y3 = Y3+t0
  bls12_381_p_mont_mul( t4, t5, t0 );              // t0 = t5*t4
  bls12_381_p_mont_mul_inplace( X3, t3 );          // X3 = t3*X3
  bls12_381_p_mont_sub_inplace( X3, t0 );          // X3 = X3-t0
  bls12_381_p_mont_mul( t1, t3, t0 );              // t0 = t3*t1
  bls12_381_p_mont_mul_inplace( Z3, t5 );          // Z3 = t5*Z3
  bls12_381_p_mont_add_inplace( Z3, t0 );          // Z3 = Z3+t0
}

void bls12_381_G1_proj_add_inplace( uint64_t *tgt, const uint64_t *src2 ) {
  bls12_381_G1_proj_add( tgt, src2, tgt);
}

void bls12_381_G1_proj_sub( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  uint64_t tmp[18];
  bls12_381_G1_proj_neg( src2, tmp );
  bls12_381_G1_proj_add( src1, tmp, tgt );
}

void bls12_381_G1_proj_sub_inplace( uint64_t *tgt, const uint64_t *src2 ) {
  uint64_t tmp[18];
  bls12_381_G1_proj_neg( src2, tmp );
  bls12_381_G1_proj_add( tgt , tmp, tgt );
}

// adds a projective point (src1) to an affine point (src2)
// https://hyperelliptic.org/EFD/g1p/auto-shortw-projective.html#addition-madd-1998-cmo
void bls12_381_G1_proj_madd_proj_aff( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  if (bls12_381_G1_proj_is_infinity( src1 )) {
    // the formula is not valid for this case
    bls12_381_G1_proj_from_affine( src2 , tgt );
    return;
  }
  if (bls12_381_G1_affine_is_infinity( src2 )) {
    bls12_381_G1_proj_copy( src1 , tgt );
    return;
  }
  uint64_t   u[6];
  uint64_t  uu[6];
  uint64_t   v[6];
  uint64_t  vv[6];
  uint64_t vvv[6];
  uint64_t   R[6];
  uint64_t   A[6];
  bls12_381_p_mont_mul( Y2, Z1, u );              //  u = Y2*Z1     
  bls12_381_p_mont_sub_inplace( u , Y1 );         //  u = Y2*Z1-Y1  
  bls12_381_p_mont_mul( X2, Z1, v );              //  v = X2*Z1     
  bls12_381_p_mont_sub_inplace( v , X1 );         //  v = X2*Z1-X1  
  if (bls12_381_p_mont_is_zero(u) && bls12_381_p_mont_is_zero(v) ) {
    // it's doubling, the naive result would be (0,0,0)
    bls12_381_G1_proj_dbl( src1 , tgt );
    return;
  }
  bls12_381_p_mont_sqr( u , uu );                 //  uu = u2       
  bls12_381_p_mont_sqr( v, vv );                  //  vv = v2       
  bls12_381_p_mont_mul( v, vv, vvv );             //  vvv = v*vv    
  bls12_381_p_mont_mul( vv, X1, R );              //  R = vv*X1     
  bls12_381_p_mont_mul( uu, Z1, A );              //  A = uu*Z1     
  bls12_381_p_mont_sub_inplace( A, vvv );         //  A = uu*Z1-vvv 
  bls12_381_p_mont_sub_inplace( A, R );           //  A = uu*Z1-vvv-2  
  bls12_381_p_mont_sub_inplace( A, R );           //  A = uu*Z1-vvv-2*R
  bls12_381_p_mont_mul( v, A, X3 );               //  X3 = v*A      
  bls12_381_p_mont_mul( Z1, vvv, Z3 );            //  Z3 = vvv*Z1   
  bls12_381_p_mont_sub_inplace( R , A );          //  R' =  R-A     
  bls12_381_p_mont_mul_inplace( vvv , Y1 );       //  vvv' = vvv*Y1 
  bls12_381_p_mont_mul( u , R , Y3 );             //  Y3 = u*(R-A)  
  bls12_381_p_mont_sub_inplace( Y3, vvv );        //  Y3 = u*(R-A)-vvv*Y1
}

// adds an affine point (src1) to a projective one (src2)
// https://hyperelliptic.org/EFD/g1p/auto-shortw-projective.html#addition-add-2015-rcb
void bls12_381_G1_proj_madd_aff_proj( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  bls12_381_G1_proj_madd_proj_aff( src2, src1, tgt );
}

// adds to a projective point (tgt) an affine point (src2), in place
void bls12_381_G1_proj_madd_inplace( uint64_t *tgt, const uint64_t *src2 ) {
  bls12_381_G1_proj_madd_proj_aff( tgt, src2, tgt );
}

// computes `expo*grp` (or `grp^expo` in multiplicative notation)
// where `grp` is a group element in G1, and `expo` is a (non-negative) bigint
// naive algorithm
void bls12_381_G1_proj_scl_naive(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt, int expo_len) {

  uint64_t dbl[3*NLIMBS_P];
  bls12_381_G1_proj_copy( grp, dbl );              // dbl := grp
  bls12_381_G1_proj_set_infinity( tgt );           // tgt := infinity

  int s = expo_len - 1;
  while( (s>0) && (expo[s] == 0) ) { s--; }      // skip the unneeded largest powers

  for(int i=0; i<=s; i++) {
    uint64_t e = expo[i];
    for(int j=0; j<64; j++) {
      if (e & 1) { 
        bls12_381_G1_proj_add( tgt, dbl, tgt ); 
      }
      bls12_381_G1_proj_dbl( dbl, dbl );
      e = e >> 1;
    }
  }
}

#define TBL(k) (table + (k-1)*3*NLIMBS_P)

// precalculate [ k*g | k <- [1..15] ]
void bls12_381_G1_proj_precalc_expos_window_16( const uint64_t *grp, uint64_t *table ) {
  bls12_381_G1_proj_copy( grp              , TBL( 1) );           //  1*g
  bls12_381_G1_proj_dbl ( TBL(1)           , TBL( 2) );           //  2*g
  bls12_381_G1_proj_dbl ( TBL(2)           , TBL( 4) );           //  4*g
  bls12_381_G1_proj_dbl ( TBL(4)           , TBL( 8) );           //  8*g
  bls12_381_G1_proj_add ( TBL(1) , TBL( 2) , TBL( 3) );           //  3*g
  bls12_381_G1_proj_dbl ( TBL(3) ,           TBL( 6) );           //  6*g
  bls12_381_G1_proj_dbl ( TBL(6) ,           TBL(12) );           // 12*g
  bls12_381_G1_proj_add ( TBL(1) , TBL( 4) , TBL( 5) );           //  5*g
  bls12_381_G1_proj_dbl ( TBL(5) ,           TBL(10) );           // 10*g
  bls12_381_G1_proj_add ( TBL(1) , TBL( 6) , TBL( 7) );           //  7*g
  bls12_381_G1_proj_dbl ( TBL(7) ,           TBL(14) );           // 14*g
  bls12_381_G1_proj_add ( TBL(1) , TBL( 8) , TBL( 9) );           //  9*g
  bls12_381_G1_proj_add ( TBL(1) , TBL(10) , TBL(11) );           // 11*g
  bls12_381_G1_proj_add ( TBL(1) , TBL(12) , TBL(13) );           // 13*g
  bls12_381_G1_proj_add ( TBL(1) , TBL(14) , TBL(15) );           // 15*g
}

// computes `expo*grp` (or `grp^expo` in multiplicative notation)
// where `grp` is a group element in G1, and `expo` is a (non-negative) bigint
// generic windowed algo, 4-bit windows
void bls12_381_G1_proj_scl_windowed(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt, int expo_len) {

  // precalculate [ k*g | k <- [1..15] ]
  uint64_t table[15*3*NLIMBS_P];
  bls12_381_G1_proj_precalc_expos_window_16( grp, table );

  bls12_381_G1_proj_set_infinity( tgt );           // tgt := infinity

  int s = expo_len - 1;
  while( (s>0) && (expo[s] == 0) ) { s--; }      // skip the unneeded largest powers

  for(int i=s; i>=0; i--) {
    uint64_t e = expo[i];
    for(int j=0; j<16; j++) {
      // we can skip doubling when infinity
      if (!bls12_381_p_mont_is_zero(tgt+2*NLIMBS_P)) {
        bls12_381_G1_proj_dbl_inplace( tgt );
        bls12_381_G1_proj_dbl_inplace( tgt );
        bls12_381_G1_proj_dbl_inplace( tgt );
        bls12_381_G1_proj_dbl_inplace( tgt );
      }
      int k = (e >> 60);
      if (k) { 
        bls12_381_G1_proj_add_inplace( tgt, TBL(k) ); 
      }
      e = e << 4;
    }
  }
}

// computes `expo*grp` (or `grp^expo` in multiplicative notation)
// where `grp` is a group element in G1, and `expo` is in Fr
void bls12_381_G1_proj_scl_generic(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt, int nlimbs) {
  bls12_381_G1_proj_scl_windowed(expo, grp, tgt, nlimbs);
}

// computes `expo*grp` (or `grp^expo` in multiplicative notation)
// where `grp` is a group element in G1, and `expo` is in Fr
void bls12_381_G1_proj_scl_Fr(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt) {
  bls12_381_G1_proj_scl_generic(expo, grp, tgt, NLIMBS_R);
}

// computes `expo*grp` (or `grp^expo` in multiplicative notation)
// where `grp` is a group element in G1, and `expo` is the same size as Fp
void bls12_381_G1_proj_scl_big(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt) {
  bls12_381_G1_proj_scl_generic(expo, grp, tgt, NLIMBS_P);
}

// computes `expo*grp` (or `grp^expo` in multiplicative notation)
// where `grp` is a group element in G1, and `expo` is a 64 bit (unsigned!) word
void bls12_381_G1_proj_scl_small(uint64_t expo, const uint64_t *grp, uint64_t *tgt) {
  uint64_t expo_vec[1];
  expo_vec[0] = expo;
  bls12_381_G1_proj_scl_generic(expo_vec, grp, tgt, 1);
}
