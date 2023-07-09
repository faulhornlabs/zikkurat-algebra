
// elliptic curve "BLS12-381" in projective coordinates, Montgomery field representation
//
// NOTE: generated code, do not edit!

#include <string.h>
#include <stdint.h>
#include <x86intrin.h>

#include "bls12_381_G1_jac.h"
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
const uint64_t bls12_381_G1_jac_gen_G1[18] = { 0x5cb38790fd530c16, 0x7817fc679976fff5, 0x154f95c7143ba1c1, 0xf0ae6acdf3d0e747, 0xedce6ecc21dbf440, 0x120177419e0bfb75, 0xbaac93d50ce72271, 0x8c22631a7918fd8e, 0xdd595f13570725ce, 0x51ac582950405194, 0x0e1c8c3fad0059c0, 0x0bbc3efc5008a26a, 0x760900000002fffd, 0xebf4000bc40c0002, 0x5f48985753c758ba, 0x77ce585370525745, 0x5c071a97a256ec6d, 0x15f65ec3fa80e493 };

// the cofactor of the curve subgroup = 76329603384216526031706109802092473003
const uint64_t bls12_381_G1_jac_cofactor[6] = { 0x8c00aaab0000aaab, 0x396c8c005555e156, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000 };

//------------------------------------------------------------------------------

// scale a field element by A = 0
void bls12_381_G1_jac_scale_by_A(const uint64_t *src, uint64_t *tgt ) {
  memset( tgt, 0, 48 );
}

void bls12_381_G1_jac_scale_by_A_inplace( uint64_t *tgt ) {
  memset( tgt, 0, 48 );
}

// scale a field element by B = 4
void bls12_381_G1_jac_scale_by_B(const uint64_t *src, uint64_t *tgt ) {
  uint64_t tmp[6];
  bls12_381_p_mont_add( src, src, tmp );
  bls12_381_p_mont_add( tmp, tmp, tgt );
}

void bls12_381_G1_jac_scale_by_B_inplace( uint64_t *tgt ) {
  uint64_t tmp[6];
  bls12_381_p_mont_add( tgt, tgt, tmp );
  bls12_381_p_mont_add( tmp, tmp, tgt );
}

void bls12_381_G1_jac_normalize( const uint64_t *src1, uint64_t *tgt ) {
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
      uint64_t zinv [6];
      uint64_t zinv2[6];
      uint64_t zinv3[6];
      bls12_381_p_mont_inv( Z1, zinv );
      bls12_381_p_mont_sqr( zinv, zinv2 );
      bls12_381_p_mont_mul( zinv, zinv2, zinv3 );
      bls12_381_p_mont_mul( X1, zinv2, X3 );
      bls12_381_p_mont_mul( Y1, zinv3, Y3 );
      bls12_381_p_mont_set_one( Z3 );
    }
  }
}

void bls12_381_G1_jac_normalize_inplace( uint64_t *tgt ) {
  bls12_381_G1_jac_normalize( tgt, tgt );
}

// checks whether the underlying representation (projective coordinates) are the same
uint8_t bls12_381_G1_jac_is_same( const uint64_t *src1, const uint64_t *src2 ) {
  return ( bls12_381_p_mont_is_equal( X1, X2 ) &&
           bls12_381_p_mont_is_equal( Y1, Y2 ) &&
           bls12_381_p_mont_is_equal( Z1, Z2 ) );
}

// checks whether two curve points are equal
uint8_t bls12_381_G1_jac_is_equal( const uint64_t *src1, const uint64_t *src2 ) {
  uint64_t tmp1[18];
  uint64_t tmp2[18];
  bls12_381_G1_jac_normalize( src1, tmp1 );
  bls12_381_G1_jac_normalize( src2, tmp2 );
  return bls12_381_G1_jac_is_same( tmp1, tmp2 );
}

// converts from affine coordinates
void bls12_381_G1_jac_from_affine( const uint64_t *src1 , uint64_t *tgt ) {
  memcpy( tgt, src1, 96 );
  if (bls12_381_G1_affine_is_infinity( src1 )) {
    bls12_381_G1_jac_set_infinity( tgt );
  }
  else {
    bls12_381_p_mont_set_one( Z3 );
  }
}

// converts to affine coordinates
// remark: the point at infinity will result in the special string `0xffff...ffff`
void bls12_381_G1_jac_to_affine( const uint64_t *src1 , uint64_t *tgt ) {
  if (bls12_381_p_mont_is_zero( Z1 )) {
    // in the affine coordinate system, the point at infinity is represented by a hack
    // consisting all 0xff bytes (note that that's an invalid value for prime fields)
    memset( tgt, 0xff, 96 );
  }
  else {
    uint64_t zinv [6];
    uint64_t zinv2[6];
    uint64_t zinv3[6];
    bls12_381_p_mont_inv( Z1, zinv );
    bls12_381_p_mont_mul( zinv, zinv , zinv2 );
    bls12_381_p_mont_mul( zinv, zinv2, zinv3 );
    bls12_381_p_mont_mul( X1, zinv2, X3 );
    bls12_381_p_mont_mul( Y1, zinv3, Y3 );
  }
}

void bls12_381_G1_jac_copy( const uint64_t *src1 , uint64_t *tgt ) {
  if (tgt != src1) { memcpy( tgt, src1, 144 ); }
}

uint8_t bls12_381_G1_jac_is_infinity ( const uint64_t *src1 ) {
  if ( ( bls12_381_p_mont_is_zero( Z1 )) &&
       (!bls12_381_p_mont_is_zero( X1 )) &&
       (!bls12_381_p_mont_is_zero( Y1 )) ) {
    // for Z=0 we have the equation Y^2 = X^3
    uint64_t XX [6];
    uint64_t XXX[6];
    uint64_t YY [6];
    bls12_381_p_mont_sqr(X1, XX);
    bls12_381_p_mont_mul(X1, XX, XXX);
    bls12_381_p_mont_sqr(Y1, YY);
    return bls12_381_p_mont_is_equal( YY, XXX );
  }
  else {
    return 0;
  }
}

// note: In Jacobian coordinates, the point at infinity is [1:1:0]
void bls12_381_G1_jac_set_infinity ( uint64_t *tgt ) {
  bls12_381_p_mont_set_one ( X3 );
  bls12_381_p_mont_set_one ( Y3 );
  bls12_381_p_mont_set_zero( Z3 );
}

// checks the curve equation
//   y^2 == x^3 + A*x*z^4 + B*z^6
uint8_t bls12_381_G1_jac_is_on_curve ( const uint64_t *src1 ) {
  uint64_t ZZ2[6];
  uint64_t ZZ4[6];
  uint64_t acc[6];
  uint64_t tmp[6];
  bls12_381_p_mont_sqr( Y1, acc );             // Y^2
  bls12_381_p_mont_neg_inplace( acc );         // -Y^2
  bls12_381_p_mont_sqr( X1, tmp );             // X^2
  bls12_381_p_mont_mul_inplace( tmp, X1 );     // X^3
  bls12_381_p_mont_add_inplace( acc, tmp );    // - Y^2 + X^3
  bls12_381_p_mont_sqr( Z1 , ZZ2 );            // Z^2
  bls12_381_p_mont_sqr( ZZ2, ZZ4 );            // Z^4
  bls12_381_p_mont_mul( ZZ2, ZZ4, tmp );        // Z^6
  bls12_381_G1_jac_scale_by_B_inplace( tmp );   // B*Z^6
  bls12_381_p_mont_add_inplace( acc, tmp );     // - Y^2 + X^3 + A*X*Z^4 + B*Z^6
  return (bls12_381_p_mont_is_zero( acc ) &&
           ( (!bls12_381_p_mont_is_zero( Z1 )) || 
             (!bls12_381_p_mont_is_zero( Y1 )) ) );
}

// checks whether the given point is in the subgroup G1
uint8_t bls12_381_G1_jac_is_in_subgroup ( const uint64_t *src1 ) {
  uint64_t tmp[18];
  if (!bls12_381_G1_jac_is_on_curve(src1)) {
    return 0;
  }
  else {
    bls12_381_G1_jac_scl_Fr( bls12_381_G1_jac_cofactor , src1 , tmp );
    return bls12_381_G1_jac_is_infinity( tmp );
  }
}

// negates an elliptic curve point
void bls12_381_G1_jac_neg( const uint64_t *src, uint64_t *tgt ) {
  if (tgt != src) { memcpy( tgt, src, 144 ); }
  bls12_381_p_mont_neg_inplace( Y3 );
}

// negates an elliptic curve point
void bls12_381_G1_jac_neg_inplace( uint64_t *tgt ) {
  bls12_381_p_mont_neg_inplace( Y3 );
}

// doubles an elliptic curve point
// <https://hyperelliptic.org/EFD/g1p/auto-shortw-jacobian.html#doubling-dbl-2007-bl>
void bls12_381_G1_jac_dbl( const uint64_t *src1, uint64_t *tgt ) {
  uint64_t   XX[6];
  uint64_t   YY[6];
  uint64_t YYYY[6];
  uint64_t   ZZ[6];
  uint64_t    S[6];
  uint64_t    M[6];
  uint64_t    T[6];
  bls12_381_p_mont_sqr( X1, XX );           // XX = X1^2
  bls12_381_p_mont_sqr( Y1, YY );           // YY = Y1^2
  bls12_381_p_mont_sqr( YY, YYYY );         // YYYY = Y1^4
  bls12_381_p_mont_sqr( Z1, ZZ );           // ZZ = Z1^2
  bls12_381_p_mont_add( X1, YY , S );       // = (X1+YY)
  bls12_381_p_mont_sqr_inplace( S );        // = (X1+YY)^2
  bls12_381_p_mont_sub_inplace( S, XX );    // = (X1+YY)^2 - XX
  bls12_381_p_mont_sub_inplace( S, YYYY );  // = (X1+YY)^2 - XX -YYYY
  bls12_381_p_mont_add_inplace( S, S );     // = S = 2*((X1+YY)^2-XX-YYYY)
  bls12_381_p_mont_add( XX, XX, M );        // = 2*XX
  bls12_381_p_mont_add_inplace( M, XX );    // M = 3*XX
  bls12_381_p_mont_sqr( M, T );                // T = M^2
  bls12_381_p_mont_sub_inplace( T, S );        // T = M^2 - S
  bls12_381_p_mont_sub_inplace( T, S );        // T = M^2 - 2*S
  bls12_381_p_mont_copy( T, X3 );              // X3  = T
  bls12_381_p_mont_add( Z1, Y1, Z3 );          // Z3  = Y1+Z1
  bls12_381_p_mont_sqr_inplace( Z3 );          // Z3  = (Y1+Z1)^2
  bls12_381_p_mont_sub_inplace( Z3, YY );      // Z3  = (Y1+Z1)^2 - YY
  bls12_381_p_mont_sub_inplace( Z3, ZZ );      // Z3  = (Y1+Z1)^2 - YY - ZZ
  bls12_381_p_mont_sub( S, T, Y3 );            // Y3  = S - T
  bls12_381_p_mont_mul_inplace( Y3, M );       // Y3  = M*(S - T)
  bls12_381_p_mont_add_inplace( YYYY , YYYY ); // 2 * YYYY
  bls12_381_p_mont_add_inplace( YYYY , YYYY ); // 4 * YYYY
  bls12_381_p_mont_add_inplace( YYYY , YYYY ); // 8 * YYYY
  bls12_381_p_mont_sub_inplace( Y3 , YYYY);    // Y3  = M*(S - T) - 8*YYYY
}

// doubles an elliptic curve point
void bls12_381_G1_jac_dbl_inplace( uint64_t *tgt ) {
  bls12_381_G1_jac_dbl( tgt , tgt );
}

// adds two elliptic curve points
// <https://hyperelliptic.org/EFD/g1p/auto-shortw-jacobian.html#addition-add-2007-bl>
void bls12_381_G1_jac_add( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  if (bls12_381_G1_jac_is_infinity(src1)) {
    if (tgt != src2) { memcpy( tgt, src2, 144); }
    return;
  }
  if (bls12_381_G1_jac_is_infinity(src2)) {
    if (tgt != src1) { memcpy( tgt, src1, 144); }
    return;
  }
  uint64_t Z1Z1[6];
  uint64_t Z2Z2[6];
  uint64_t U1[6];
  uint64_t U2[6];
  uint64_t S1[6];
  uint64_t S2[6];
  uint64_t  H[6];
  uint64_t  I[6];
  uint64_t  J[6];
  uint64_t  r[6];
  uint64_t  V[6];
  bls12_381_p_mont_sqr( Z1, Z1Z1 );           // Z1Z1 = Z1^2
  bls12_381_p_mont_sqr( Z2, Z2Z2 );           // Z2Z2 = Z2^2
  bls12_381_p_mont_mul( X1, Z2Z2 , U1 );      // U1 = X1*Z2Z2
  bls12_381_p_mont_mul( X2, Z1Z1 , U2 );      // U2 = X2*Z1Z1
  bls12_381_p_mont_mul( Y1, Z2 , S1 );        //    = Y1 * Z2
  bls12_381_p_mont_mul_inplace(  S1, Z2Z2 );  // S1 = Y1 * Z2 * Z2Z2
  bls12_381_p_mont_mul( Y2, Z1 , S2 );        //    = Y2 * Z1
  bls12_381_p_mont_mul_inplace(  S2, Z1Z1 );  // S2 = Y2 * Z1 * Z1Z1
  bls12_381_p_mont_sub( U2, U1, H );          // H  = U2-U1
  if (bls12_381_p_mont_is_zero( H )) {
    // X1/Z1^2 == X2/Z2^2
    // so either Y1/Z1^3 == Y2/Z2^3, in which case it's a doubling
    // or not, in which case Y1/Z1^3 == - Y2/Z2^3 and the result is infinity
    if (bls12_381_p_mont_is_equal( S1, S2)) {
      // Y1/Z1^3 == Y2/Z2^3
      bls12_381_G1_jac_dbl( src1, tgt );
      return;
    }
    else {
      // Y1/Z1^3 != Y2/Z2^3
      bls12_381_G1_jac_set_infinity( tgt );
      return;
    }
  }
  bls12_381_p_mont_add( H, H, I );            //    = 2*H
  bls12_381_p_mont_sqr_inplace( I );          // I  = (2*H)^2
  bls12_381_p_mont_mul( H, I, J );            // J  = H*I
  bls12_381_p_mont_sub( S2, S1, r );          //    = S2-S1
  bls12_381_p_mont_add_inplace( r, r );       // r  = 2*(S2-S1)
  bls12_381_p_mont_mul( U1, I, V );           // V  = U1*I
  bls12_381_p_mont_sqr( r, X3 );              //    = r^2
  bls12_381_p_mont_sub_inplace( X3, J );      //    = r^2 - J
  bls12_381_p_mont_sub_inplace( X3, V );      //    = r^2 - J - V
  bls12_381_p_mont_sub_inplace( X3, V );      // X3 = r^2 - J - 2*V
  bls12_381_p_mont_sub( V, X3, Y3 );          //    = V-X3
  bls12_381_p_mont_mul_inplace( Y3, r );      //    = r*(V-X3)
  bls12_381_p_mont_mul_inplace( J, S1 );      // J := S1*J
  bls12_381_p_mont_sub_inplace( Y3, J );      //    = r*(V-X3) - S1*J
  bls12_381_p_mont_sub_inplace( Y3, J );      // Y3 = r*(V-X3) - 2*S1*J
  bls12_381_p_mont_add( Z1, Z2, Z3 );         //    = Z1+Z2
  bls12_381_p_mont_sqr_inplace( Z3 );         //    = (Z1+Z2)^2
  bls12_381_p_mont_sub_inplace( Z3, Z1Z1 );   //    = (Z1+Z2)^2-Z1Z1
  bls12_381_p_mont_sub_inplace( Z3, Z2Z2 );   //    = (Z1+Z2)^2-Z1Z1-Z2Z2
  bls12_381_p_mont_mul_inplace( Z3, H );      // Z3 = ((Z1+Z2)^2-Z1Z1-Z2Z2)*H
}

void bls12_381_G1_jac_add_inplace( uint64_t *tgt, const uint64_t *src2 ) {
  bls12_381_G1_jac_add( tgt, src2, tgt);
}

void bls12_381_G1_jac_sub( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  uint64_t tmp[18];
  bls12_381_G1_jac_neg( src2, tmp );
  bls12_381_G1_jac_add( src1, tmp, tgt );
}

void bls12_381_G1_jac_sub_inplace( uint64_t *tgt, const uint64_t *src2 ) {
  uint64_t tmp[18];
  bls12_381_G1_jac_neg( src2, tmp );
  bls12_381_G1_jac_add( tgt , tmp, tgt );
}

// adds a Jacobian projective point (src1) to an affine point (src2)
// <https://hyperelliptic.org/EFD/g1p/auto-shortw-jacobian.html#addition-madd-2007-bl>
void bls12_381_G1_jac_madd_jac_aff( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  if (bls12_381_G1_jac_is_infinity( src1 )) {
    // the formula is not valid for this case
    bls12_381_G1_jac_from_affine( src2 , tgt );
    return;
  }
  if (bls12_381_G1_affine_is_infinity( src2 )) {
    bls12_381_G1_jac_copy( src1 , tgt );
    return;
  }
  uint64_t Z1Z1[6];
  uint64_t U2[6];
  uint64_t S2[6];
  uint64_t  H[6];
  uint64_t HH[6];
  uint64_t  I[6];
  uint64_t  J[6];
  uint64_t  r[6];
  uint64_t  V[6];
  bls12_381_p_mont_sqr( Z1, Z1Z1 );           // Z1Z1 = Z1^2
  bls12_381_p_mont_mul( X2, Z1Z1 , U2 );      // U2 = X2*Z1Z1
  bls12_381_p_mont_mul( Y2, Z1 , S2 );        //    = Y2 * Z1
  bls12_381_p_mont_mul_inplace( S2, Z1Z1 );   // S2 = Y2 * Z1 * Z1Z1
  bls12_381_p_mont_sub( U2, X1, H );          // H  = U2-X1
  bls12_381_p_mont_sqr( H, HH );              // HH = H^2
  bls12_381_p_mont_add( HH, HH, I );          //    = 2*HH
  bls12_381_p_mont_add_inplace( I, I );       // I  = 4*HH
  bls12_381_p_mont_mul( H, I, J );            // J  = H*I
  bls12_381_p_mont_sub( S2, Y1, r );          //    = S2-Y1
  if (bls12_381_p_mont_is_zero(H)) {
    // H=0  <==>  X1/Z1^2 = X2
    // either a doubling or the result is infinity
    if (bls12_381_p_mont_is_zero(r)) {
      // r=0  <==>  Y1/Z1^2 = Y2
      // it's a doubling
      bls12_381_G1_jac_dbl( src1, tgt );
      return;
    }
    else {
      // X1/Z1^2 = X2 but Y1/Z1^2 /= Y2
      // so the result must be infinity
      bls12_381_G1_jac_set_infinity( tgt );
      return;
    }
  }
  bls12_381_p_mont_add_inplace( r, r );       // r  = 2*(S2-Y1)
  bls12_381_p_mont_mul( X1, I, V );           // V  = X1*I
  bls12_381_p_mont_sqr( r, X3 );              //    = r^2
  bls12_381_p_mont_sub_inplace( X3, J );      //    = r^2 - J
  bls12_381_p_mont_sub_inplace( X3, V );      //    = r^2 - J - V
  bls12_381_p_mont_sub_inplace( X3, V );      // X3 = r^2 - J - 2*V
  bls12_381_p_mont_mul_inplace( J, Y1 );      // J := Y1*J - careful, in the next row we possibly overwrite Y1!
  bls12_381_p_mont_sub( V, X3, Y3 );          //    = V-X3
  bls12_381_p_mont_mul_inplace( Y3, r );      // Y3 = r*(V-X3)
  bls12_381_p_mont_sub_inplace( Y3, J );      //    = r*(V-X3) - Y1*J
  bls12_381_p_mont_sub_inplace( Y3, J );      // Y3 = r*(V-X3) - 2*Y1*J
  bls12_381_p_mont_add( Z1, H , Z3 );         //    = Z1+H
  bls12_381_p_mont_sqr_inplace( Z3 );         //    = (Z1+H)^2
  bls12_381_p_mont_sub_inplace( Z3, Z1Z1 );   //    = (Z1+H)^2-Z1Z1
  bls12_381_p_mont_sub_inplace( Z3, HH );     // Z3 = (Z1+H)^2-Z1Z1-HH
}

// adds an affine point (src1) to a projective one (src2)
// https://hyperelliptic.org/EFD/g1p/auto-shortw-projective.html#addition-add-2015-rcb
void bls12_381_G1_jac_madd_aff_jac( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  bls12_381_G1_jac_madd_jac_aff( src2, src1, tgt );
}

// adds to a projective point (tgt) an affine point (src2), in place
void bls12_381_G1_jac_madd_inplace( uint64_t *tgt, const uint64_t *src2 ) {
  bls12_381_G1_jac_madd_jac_aff( tgt, src2, tgt );
}

// computes `expo*grp` (or `grp^expo` in multiplicative notation)
// where `grp` is a group element in G1, and `expo` is a (non-negative) bigint
// naive algorithm
void bls12_381_G1_jac_scl_naive(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt, int expo_len) {

  uint64_t dbl[3*NLIMBS_P];
  bls12_381_G1_jac_copy( grp, dbl );              // dbl := grp
  bls12_381_G1_jac_set_infinity( tgt );           // tgt := infinity

  int s = expo_len - 1;
  while( (s>0) && (expo[s] == 0) ) { s--; }      // skip the unneeded largest powers

  for(int i=0; i<=s; i++) {
    uint64_t e = expo[i];
    for(int j=0; j<64; j++) {
      if (e & 1) { 
        bls12_381_G1_jac_add( tgt, dbl, tgt ); 
      }
      bls12_381_G1_jac_dbl( dbl, dbl );
      e = e >> 1;
    }
  }
}

#define TBL(k) (table + (k-1)*3*NLIMBS_P)

// precalculate [ k*g | k <- [1..15] ]
void bls12_381_G1_jac_precalc_expos_window_16( const uint64_t *grp, uint64_t *table ) {
  bls12_381_G1_jac_copy( grp              , TBL( 1) );           //  1*g
  bls12_381_G1_jac_dbl ( TBL(1)           , TBL( 2) );           //  2*g
  bls12_381_G1_jac_dbl ( TBL(2)           , TBL( 4) );           //  4*g
  bls12_381_G1_jac_dbl ( TBL(4)           , TBL( 8) );           //  8*g
  bls12_381_G1_jac_add ( TBL(1) , TBL( 2) , TBL( 3) );           //  3*g
  bls12_381_G1_jac_dbl ( TBL(3) ,           TBL( 6) );           //  6*g
  bls12_381_G1_jac_dbl ( TBL(6) ,           TBL(12) );           // 12*g
  bls12_381_G1_jac_add ( TBL(1) , TBL( 4) , TBL( 5) );           //  5*g
  bls12_381_G1_jac_dbl ( TBL(5) ,           TBL(10) );           // 10*g
  bls12_381_G1_jac_add ( TBL(1) , TBL( 6) , TBL( 7) );           //  7*g
  bls12_381_G1_jac_dbl ( TBL(7) ,           TBL(14) );           // 14*g
  bls12_381_G1_jac_add ( TBL(1) , TBL( 8) , TBL( 9) );           //  9*g
  bls12_381_G1_jac_add ( TBL(1) , TBL(10) , TBL(11) );           // 11*g
  bls12_381_G1_jac_add ( TBL(1) , TBL(12) , TBL(13) );           // 13*g
  bls12_381_G1_jac_add ( TBL(1) , TBL(14) , TBL(15) );           // 15*g
}

// computes `expo*grp` (or `grp^expo` in multiplicative notation)
// where `grp` is a group element in G1, and `expo` is a (non-negative) bigint
// generic windowed algo, 4-bit windows
void bls12_381_G1_jac_scl_windowed(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt, int expo_len) {

  // precalculate [ k*g | k <- [1..15] ]
  uint64_t table[15*3*NLIMBS_P];
  bls12_381_G1_jac_precalc_expos_window_16( grp, table );

  bls12_381_G1_jac_set_infinity( tgt );           // tgt := infinity

  int s = expo_len - 1;
  while( (s>0) && (expo[s] == 0) ) { s--; }      // skip the unneeded largest powers

  for(int i=s; i>=0; i--) {
    uint64_t e = expo[i];
    for(int j=0; j<16; j++) {
      // we can skip doubling when infinity
      if (!bls12_381_p_mont_is_zero(tgt+2*NLIMBS_P)) {
        bls12_381_G1_jac_dbl_inplace( tgt );
        bls12_381_G1_jac_dbl_inplace( tgt );
        bls12_381_G1_jac_dbl_inplace( tgt );
        bls12_381_G1_jac_dbl_inplace( tgt );
      }
      int k = (e >> 60);
      if (k) { 
        bls12_381_G1_jac_add_inplace( tgt, TBL(k) ); 
      }
      e = e << 4;
    }
  }
}

// computes `expo*grp` (or `grp^expo` in multiplicative notation)
// where `grp` is a group element in G1, and `expo` is in Fr
void bls12_381_G1_jac_scl_generic(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt, int nlimbs) {
  bls12_381_G1_jac_scl_windowed(expo, grp, tgt, nlimbs);
}

// computes `expo*grp` (or `grp^expo` in multiplicative notation)
// where `grp` is a group element in G1, and `expo` is in Fr
void bls12_381_G1_jac_scl_Fr(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt) {
  bls12_381_G1_jac_scl_generic(expo, grp, tgt, NLIMBS_R);
}

// computes `expo*grp` (or `grp^expo` in multiplicative notation)
// where `grp` is a group element in G1, and `expo` is the same size as Fp
void bls12_381_G1_jac_scl_big(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt) {
  bls12_381_G1_jac_scl_generic(expo, grp, tgt, NLIMBS_P);
}

// computes `expo*grp` (or `grp^expo` in multiplicative notation)
// where `grp` is a group element in G1, and `expo` is a 64 bit (unsigned!) word
void bls12_381_G1_jac_scl_small(uint64_t expo, const uint64_t *grp, uint64_t *tgt) {
  uint64_t expo_vec[1];
  expo_vec[0] = expo;
  bls12_381_G1_jac_scl_generic(expo_vec, grp, tgt, 1);
}
