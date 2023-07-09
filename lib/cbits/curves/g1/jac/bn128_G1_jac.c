
// elliptic curve "BN128" in projective coordinates, Montgomery field representation
//
// NOTE: generated code, do not edit!

#include <string.h>
#include <stdint.h>
#include <x86intrin.h>

#include "bn128_G1_jac.h"
#include "bn128_G1_affine.h"
#include "bn128_p_mont.h"
#include "bn128_r_mont.h"

#define NLIMBS_P 4
#define NLIMBS_R 4

#define X1 (src1)
#define Y1 (src1 + 4)
#define Z1 (src1 + 8)

#define X2 (src2)
#define Y2 (src2 + 4)
#define Z2 (src2 + 8)

#define X3 (tgt)
#define Y3 (tgt + 4)
#define Z3 (tgt + 8)

// the generator of the subgroup G1
const uint64_t bn128_G1_jac_gen_G1[12] = { 0xd35d438dc58f0d9d, 0x0a78eb28f5c70b3d, 0x666ea36f7879462c, 0x0e0a77c19a07df2f, 0xa6ba871b8b1e1b3a, 0x14f1d651eb8e167b, 0xccdd46def0f28c58, 0x1c14ef83340fbe5e, 0xd35d438dc58f0d9d, 0x0a78eb28f5c70b3d, 0x666ea36f7879462c, 0x0e0a77c19a07df2f };

// the cofactor of the curve subgroup = 1
const uint64_t bn128_G1_jac_cofactor[4] = { 0x0000000000000001, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000 };

//------------------------------------------------------------------------------

// scale a field element by A = 0
void bn128_G1_jac_scale_by_A(const uint64_t *src, uint64_t *tgt ) {
  memset( tgt, 0, 32 );
}

void bn128_G1_jac_scale_by_A_inplace( uint64_t *tgt ) {
  memset( tgt, 0, 32 );
}

// scale a field element by B = 3
void bn128_G1_jac_scale_by_B(const uint64_t *src, uint64_t *tgt ) {
  uint64_t tmp[4];
  bn128_p_mont_add( src, src, tmp );
  bn128_p_mont_add( src, tmp, tgt );
}

void bn128_G1_jac_scale_by_B_inplace( uint64_t *tgt ) {
  uint64_t tmp[4];
  bn128_p_mont_add( tgt, tgt, tmp );
  bn128_p_mont_add_inplace( tgt, tmp );
}

void bn128_G1_jac_normalize( const uint64_t *src1, uint64_t *tgt ) {
  if (bn128_p_mont_is_zero( Z1 ) ) {
    // Z == 0, it must be the point at infinity
    memset( tgt, 0, 96 );
    bn128_p_mont_set_one( Y3 );
  }
  else {
    if (bn128_p_mont_is_one( Z1 )) {
      // already normalized
      if (tgt != src1) { memcpy( tgt, src1, 96 ); }
    }
    else {
      uint64_t zinv [4];
      uint64_t zinv2[4];
      uint64_t zinv3[4];
      bn128_p_mont_inv( Z1, zinv );
      bn128_p_mont_sqr( zinv, zinv2 );
      bn128_p_mont_mul( zinv, zinv2, zinv3 );
      bn128_p_mont_mul( X1, zinv2, X3 );
      bn128_p_mont_mul( Y1, zinv3, Y3 );
      bn128_p_mont_set_one( Z3 );
    }
  }
}

void bn128_G1_jac_normalize_inplace( uint64_t *tgt ) {
  bn128_G1_jac_normalize( tgt, tgt );
}

// checks whether the underlying representation (projective coordinates) are the same
uint8_t bn128_G1_jac_is_same( const uint64_t *src1, const uint64_t *src2 ) {
  return ( bn128_p_mont_is_equal( X1, X2 ) &&
           bn128_p_mont_is_equal( Y1, Y2 ) &&
           bn128_p_mont_is_equal( Z1, Z2 ) );
}

// checks whether two curve points are equal
uint8_t bn128_G1_jac_is_equal( const uint64_t *src1, const uint64_t *src2 ) {
  uint64_t tmp1[12];
  uint64_t tmp2[12];
  bn128_G1_jac_normalize( src1, tmp1 );
  bn128_G1_jac_normalize( src2, tmp2 );
  return bn128_G1_jac_is_same( tmp1, tmp2 );
}

// converts from affine coordinates
void bn128_G1_jac_from_affine( const uint64_t *src1 , uint64_t *tgt ) {
  memcpy( tgt, src1, 64 );
  if (bn128_G1_affine_is_infinity( src1 )) {
    bn128_G1_jac_set_infinity( tgt );
  }
  else {
    bn128_p_mont_set_one( Z3 );
  }
}

// converts to affine coordinates
// remark: the point at infinity will result in the special string `0xffff...ffff`
void bn128_G1_jac_to_affine( const uint64_t *src1 , uint64_t *tgt ) {
  if (bn128_p_mont_is_zero( Z1 )) {
    // in the affine coordinate system, the point at infinity is represented by a hack
    // consisting all 0xff bytes (note that that's an invalid value for prime fields)
    memset( tgt, 0xff, 64 );
  }
  else {
    uint64_t zinv [4];
    uint64_t zinv2[4];
    uint64_t zinv3[4];
    bn128_p_mont_inv( Z1, zinv );
    bn128_p_mont_mul( zinv, zinv , zinv2 );
    bn128_p_mont_mul( zinv, zinv2, zinv3 );
    bn128_p_mont_mul( X1, zinv2, X3 );
    bn128_p_mont_mul( Y1, zinv3, Y3 );
  }
}

void bn128_G1_jac_copy( const uint64_t *src1 , uint64_t *tgt ) {
  if (tgt != src1) { memcpy( tgt, src1, 96 ); }
}

uint8_t bn128_G1_jac_is_infinity ( const uint64_t *src1 ) {
  if ( ( bn128_p_mont_is_zero( Z1 )) &&
       (!bn128_p_mont_is_zero( X1 )) &&
       (!bn128_p_mont_is_zero( Y1 )) ) {
    // for Z=0 we have the equation Y^2 = X^3
    uint64_t XX [4];
    uint64_t XXX[4];
    uint64_t YY [4];
    bn128_p_mont_sqr(X1, XX);
    bn128_p_mont_mul(X1, XX, XXX);
    bn128_p_mont_sqr(Y1, YY);
    return bn128_p_mont_is_equal( YY, XXX );
  }
  else {
    return 0;
  }
}

// note: In Jacobian coordinates, the point at infinity is [1:1:0]
void bn128_G1_jac_set_infinity ( uint64_t *tgt ) {
  bn128_p_mont_set_one ( X3 );
  bn128_p_mont_set_one ( Y3 );
  bn128_p_mont_set_zero( Z3 );
}

// checks the curve equation
//   y^2 == x^3 + A*x*z^4 + B*z^6
uint8_t bn128_G1_jac_is_on_curve ( const uint64_t *src1 ) {
  uint64_t ZZ2[4];
  uint64_t ZZ4[4];
  uint64_t acc[4];
  uint64_t tmp[4];
  bn128_p_mont_sqr( Y1, acc );             // Y^2
  bn128_p_mont_neg_inplace( acc );         // -Y^2
  bn128_p_mont_sqr( X1, tmp );             // X^2
  bn128_p_mont_mul_inplace( tmp, X1 );     // X^3
  bn128_p_mont_add_inplace( acc, tmp );    // - Y^2 + X^3
  bn128_p_mont_sqr( Z1 , ZZ2 );            // Z^2
  bn128_p_mont_sqr( ZZ2, ZZ4 );            // Z^4
  bn128_p_mont_mul( ZZ2, ZZ4, tmp );        // Z^6
  bn128_G1_jac_scale_by_B_inplace( tmp );   // B*Z^6
  bn128_p_mont_add_inplace( acc, tmp );     // - Y^2 + X^3 + A*X*Z^4 + B*Z^6
  return (bn128_p_mont_is_zero( acc ) &&
           ( (!bn128_p_mont_is_zero( Z1 )) || 
             (!bn128_p_mont_is_zero( Y1 )) ) );
}

// checks whether the given point is in the subgroup G1
uint8_t bn128_G1_jac_is_in_subgroup ( const uint64_t *src1 ) {
  uint64_t tmp[12];
  if (!bn128_G1_jac_is_on_curve(src1)) {
    return 0;
  }
  else {
    bn128_G1_jac_scl_Fr( bn128_G1_jac_cofactor , src1 , tmp );
    return bn128_G1_jac_is_infinity( tmp );
  }
}

// negates an elliptic curve point
void bn128_G1_jac_neg( const uint64_t *src, uint64_t *tgt ) {
  if (tgt != src) { memcpy( tgt, src, 96 ); }
  bn128_p_mont_neg_inplace( Y3 );
}

// negates an elliptic curve point
void bn128_G1_jac_neg_inplace( uint64_t *tgt ) {
  bn128_p_mont_neg_inplace( Y3 );
}

// doubles an elliptic curve point
// <https://hyperelliptic.org/EFD/g1p/auto-shortw-jacobian.html#doubling-dbl-2007-bl>
void bn128_G1_jac_dbl( const uint64_t *src1, uint64_t *tgt ) {
  uint64_t   XX[4];
  uint64_t   YY[4];
  uint64_t YYYY[4];
  uint64_t   ZZ[4];
  uint64_t    S[4];
  uint64_t    M[4];
  uint64_t    T[4];
  bn128_p_mont_sqr( X1, XX );           // XX = X1^2
  bn128_p_mont_sqr( Y1, YY );           // YY = Y1^2
  bn128_p_mont_sqr( YY, YYYY );         // YYYY = Y1^4
  bn128_p_mont_sqr( Z1, ZZ );           // ZZ = Z1^2
  bn128_p_mont_add( X1, YY , S );       // = (X1+YY)
  bn128_p_mont_sqr_inplace( S );        // = (X1+YY)^2
  bn128_p_mont_sub_inplace( S, XX );    // = (X1+YY)^2 - XX
  bn128_p_mont_sub_inplace( S, YYYY );  // = (X1+YY)^2 - XX -YYYY
  bn128_p_mont_add_inplace( S, S );     // = S = 2*((X1+YY)^2-XX-YYYY)
  bn128_p_mont_add( XX, XX, M );        // = 2*XX
  bn128_p_mont_add_inplace( M, XX );    // M = 3*XX
  bn128_p_mont_sqr( M, T );                // T = M^2
  bn128_p_mont_sub_inplace( T, S );        // T = M^2 - S
  bn128_p_mont_sub_inplace( T, S );        // T = M^2 - 2*S
  bn128_p_mont_copy( T, X3 );              // X3  = T
  bn128_p_mont_add( Z1, Y1, Z3 );          // Z3  = Y1+Z1
  bn128_p_mont_sqr_inplace( Z3 );          // Z3  = (Y1+Z1)^2
  bn128_p_mont_sub_inplace( Z3, YY );      // Z3  = (Y1+Z1)^2 - YY
  bn128_p_mont_sub_inplace( Z3, ZZ );      // Z3  = (Y1+Z1)^2 - YY - ZZ
  bn128_p_mont_sub( S, T, Y3 );            // Y3  = S - T
  bn128_p_mont_mul_inplace( Y3, M );       // Y3  = M*(S - T)
  bn128_p_mont_add_inplace( YYYY , YYYY ); // 2 * YYYY
  bn128_p_mont_add_inplace( YYYY , YYYY ); // 4 * YYYY
  bn128_p_mont_add_inplace( YYYY , YYYY ); // 8 * YYYY
  bn128_p_mont_sub_inplace( Y3 , YYYY);    // Y3  = M*(S - T) - 8*YYYY
}

// doubles an elliptic curve point
void bn128_G1_jac_dbl_inplace( uint64_t *tgt ) {
  bn128_G1_jac_dbl( tgt , tgt );
}

// adds two elliptic curve points
// <https://hyperelliptic.org/EFD/g1p/auto-shortw-jacobian.html#addition-add-2007-bl>
void bn128_G1_jac_add( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  if (bn128_G1_jac_is_infinity(src1)) {
    if (tgt != src2) { memcpy( tgt, src2, 96); }
    return;
  }
  if (bn128_G1_jac_is_infinity(src2)) {
    if (tgt != src1) { memcpy( tgt, src1, 96); }
    return;
  }
  uint64_t Z1Z1[4];
  uint64_t Z2Z2[4];
  uint64_t U1[4];
  uint64_t U2[4];
  uint64_t S1[4];
  uint64_t S2[4];
  uint64_t  H[4];
  uint64_t  I[4];
  uint64_t  J[4];
  uint64_t  r[4];
  uint64_t  V[4];
  bn128_p_mont_sqr( Z1, Z1Z1 );           // Z1Z1 = Z1^2
  bn128_p_mont_sqr( Z2, Z2Z2 );           // Z2Z2 = Z2^2
  bn128_p_mont_mul( X1, Z2Z2 , U1 );      // U1 = X1*Z2Z2
  bn128_p_mont_mul( X2, Z1Z1 , U2 );      // U2 = X2*Z1Z1
  bn128_p_mont_mul( Y1, Z2 , S1 );        //    = Y1 * Z2
  bn128_p_mont_mul_inplace(  S1, Z2Z2 );  // S1 = Y1 * Z2 * Z2Z2
  bn128_p_mont_mul( Y2, Z1 , S2 );        //    = Y2 * Z1
  bn128_p_mont_mul_inplace(  S2, Z1Z1 );  // S2 = Y2 * Z1 * Z1Z1
  bn128_p_mont_sub( U2, U1, H );          // H  = U2-U1
  if (bn128_p_mont_is_zero( H )) {
    // X1/Z1^2 == X2/Z2^2
    // so either Y1/Z1^3 == Y2/Z2^3, in which case it's a doubling
    // or not, in which case Y1/Z1^3 == - Y2/Z2^3 and the result is infinity
    if (bn128_p_mont_is_equal( S1, S2)) {
      // Y1/Z1^3 == Y2/Z2^3
      bn128_G1_jac_dbl( src1, tgt );
      return;
    }
    else {
      // Y1/Z1^3 != Y2/Z2^3
      bn128_G1_jac_set_infinity( tgt );
      return;
    }
  }
  bn128_p_mont_add( H, H, I );            //    = 2*H
  bn128_p_mont_sqr_inplace( I );          // I  = (2*H)^2
  bn128_p_mont_mul( H, I, J );            // J  = H*I
  bn128_p_mont_sub( S2, S1, r );          //    = S2-S1
  bn128_p_mont_add_inplace( r, r );       // r  = 2*(S2-S1)
  bn128_p_mont_mul( U1, I, V );           // V  = U1*I
  bn128_p_mont_sqr( r, X3 );              //    = r^2
  bn128_p_mont_sub_inplace( X3, J );      //    = r^2 - J
  bn128_p_mont_sub_inplace( X3, V );      //    = r^2 - J - V
  bn128_p_mont_sub_inplace( X3, V );      // X3 = r^2 - J - 2*V
  bn128_p_mont_sub( V, X3, Y3 );          //    = V-X3
  bn128_p_mont_mul_inplace( Y3, r );      //    = r*(V-X3)
  bn128_p_mont_mul_inplace( J, S1 );      // J := S1*J
  bn128_p_mont_sub_inplace( Y3, J );      //    = r*(V-X3) - S1*J
  bn128_p_mont_sub_inplace( Y3, J );      // Y3 = r*(V-X3) - 2*S1*J
  bn128_p_mont_add( Z1, Z2, Z3 );         //    = Z1+Z2
  bn128_p_mont_sqr_inplace( Z3 );         //    = (Z1+Z2)^2
  bn128_p_mont_sub_inplace( Z3, Z1Z1 );   //    = (Z1+Z2)^2-Z1Z1
  bn128_p_mont_sub_inplace( Z3, Z2Z2 );   //    = (Z1+Z2)^2-Z1Z1-Z2Z2
  bn128_p_mont_mul_inplace( Z3, H );      // Z3 = ((Z1+Z2)^2-Z1Z1-Z2Z2)*H
}

void bn128_G1_jac_add_inplace( uint64_t *tgt, const uint64_t *src2 ) {
  bn128_G1_jac_add( tgt, src2, tgt);
}

void bn128_G1_jac_sub( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  uint64_t tmp[12];
  bn128_G1_jac_neg( src2, tmp );
  bn128_G1_jac_add( src1, tmp, tgt );
}

void bn128_G1_jac_sub_inplace( uint64_t *tgt, const uint64_t *src2 ) {
  uint64_t tmp[12];
  bn128_G1_jac_neg( src2, tmp );
  bn128_G1_jac_add( tgt , tmp, tgt );
}

// adds a Jacobian projective point (src1) to an affine point (src2)
// <https://hyperelliptic.org/EFD/g1p/auto-shortw-jacobian.html#addition-madd-2007-bl>
void bn128_G1_jac_madd_jac_aff( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  if (bn128_G1_jac_is_infinity( src1 )) {
    // the formula is not valid for this case
    bn128_G1_jac_from_affine( src2 , tgt );
    return;
  }
  if (bn128_G1_affine_is_infinity( src2 )) {
    bn128_G1_jac_copy( src1 , tgt );
    return;
  }
  uint64_t Z1Z1[4];
  uint64_t U2[4];
  uint64_t S2[4];
  uint64_t  H[4];
  uint64_t HH[4];
  uint64_t  I[4];
  uint64_t  J[4];
  uint64_t  r[4];
  uint64_t  V[4];
  bn128_p_mont_sqr( Z1, Z1Z1 );           // Z1Z1 = Z1^2
  bn128_p_mont_mul( X2, Z1Z1 , U2 );      // U2 = X2*Z1Z1
  bn128_p_mont_mul( Y2, Z1 , S2 );        //    = Y2 * Z1
  bn128_p_mont_mul_inplace( S2, Z1Z1 );   // S2 = Y2 * Z1 * Z1Z1
  bn128_p_mont_sub( U2, X1, H );          // H  = U2-X1
  bn128_p_mont_sqr( H, HH );              // HH = H^2
  bn128_p_mont_add( HH, HH, I );          //    = 2*HH
  bn128_p_mont_add_inplace( I, I );       // I  = 4*HH
  bn128_p_mont_mul( H, I, J );            // J  = H*I
  bn128_p_mont_sub( S2, Y1, r );          //    = S2-Y1
  if (bn128_p_mont_is_zero(H)) {
    // H=0  <==>  X1/Z1^2 = X2
    // either a doubling or the result is infinity
    if (bn128_p_mont_is_zero(r)) {
      // r=0  <==>  Y1/Z1^2 = Y2
      // it's a doubling
      bn128_G1_jac_dbl( src1, tgt );
      return;
    }
    else {
      // X1/Z1^2 = X2 but Y1/Z1^2 /= Y2
      // so the result must be infinity
      bn128_G1_jac_set_infinity( tgt );
      return;
    }
  }
  bn128_p_mont_add_inplace( r, r );       // r  = 2*(S2-Y1)
  bn128_p_mont_mul( X1, I, V );           // V  = X1*I
  bn128_p_mont_sqr( r, X3 );              //    = r^2
  bn128_p_mont_sub_inplace( X3, J );      //    = r^2 - J
  bn128_p_mont_sub_inplace( X3, V );      //    = r^2 - J - V
  bn128_p_mont_sub_inplace( X3, V );      // X3 = r^2 - J - 2*V
  bn128_p_mont_mul_inplace( J, Y1 );      // J := Y1*J - careful, in the next row we possibly overwrite Y1!
  bn128_p_mont_sub( V, X3, Y3 );          //    = V-X3
  bn128_p_mont_mul_inplace( Y3, r );      // Y3 = r*(V-X3)
  bn128_p_mont_sub_inplace( Y3, J );      //    = r*(V-X3) - Y1*J
  bn128_p_mont_sub_inplace( Y3, J );      // Y3 = r*(V-X3) - 2*Y1*J
  bn128_p_mont_add( Z1, H , Z3 );         //    = Z1+H
  bn128_p_mont_sqr_inplace( Z3 );         //    = (Z1+H)^2
  bn128_p_mont_sub_inplace( Z3, Z1Z1 );   //    = (Z1+H)^2-Z1Z1
  bn128_p_mont_sub_inplace( Z3, HH );     // Z3 = (Z1+H)^2-Z1Z1-HH
}

// adds an affine point (src1) to a projective one (src2)
// https://hyperelliptic.org/EFD/g1p/auto-shortw-projective.html#addition-add-2015-rcb
void bn128_G1_jac_madd_aff_jac( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  bn128_G1_jac_madd_jac_aff( src2, src1, tgt );
}

// adds to a projective point (tgt) an affine point (src2), in place
void bn128_G1_jac_madd_inplace( uint64_t *tgt, const uint64_t *src2 ) {
  bn128_G1_jac_madd_jac_aff( tgt, src2, tgt );
}

// computes `expo*grp` (or `grp^expo` in multiplicative notation)
// where `grp` is a group element in G1, and `expo` is a (non-negative) bigint
// naive algorithm
void bn128_G1_jac_scl_naive(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt, int expo_len) {

  uint64_t dbl[3*NLIMBS_P];
  bn128_G1_jac_copy( grp, dbl );              // dbl := grp
  bn128_G1_jac_set_infinity( tgt );           // tgt := infinity

  int s = expo_len - 1;
  while( (s>0) && (expo[s] == 0) ) { s--; }      // skip the unneeded largest powers

  for(int i=0; i<=s; i++) {
    uint64_t e = expo[i];
    for(int j=0; j<64; j++) {
      if (e & 1) { 
        bn128_G1_jac_add( tgt, dbl, tgt ); 
      }
      bn128_G1_jac_dbl( dbl, dbl );
      e = e >> 1;
    }
  }
}

#define TBL(k) (table + (k-1)*3*NLIMBS_P)

// precalculate [ k*g | k <- [1..15] ]
void bn128_G1_jac_precalc_expos_window_16( const uint64_t *grp, uint64_t *table ) {
  bn128_G1_jac_copy( grp              , TBL( 1) );           //  1*g
  bn128_G1_jac_dbl ( TBL(1)           , TBL( 2) );           //  2*g
  bn128_G1_jac_dbl ( TBL(2)           , TBL( 4) );           //  4*g
  bn128_G1_jac_dbl ( TBL(4)           , TBL( 8) );           //  8*g
  bn128_G1_jac_add ( TBL(1) , TBL( 2) , TBL( 3) );           //  3*g
  bn128_G1_jac_dbl ( TBL(3) ,           TBL( 6) );           //  6*g
  bn128_G1_jac_dbl ( TBL(6) ,           TBL(12) );           // 12*g
  bn128_G1_jac_add ( TBL(1) , TBL( 4) , TBL( 5) );           //  5*g
  bn128_G1_jac_dbl ( TBL(5) ,           TBL(10) );           // 10*g
  bn128_G1_jac_add ( TBL(1) , TBL( 6) , TBL( 7) );           //  7*g
  bn128_G1_jac_dbl ( TBL(7) ,           TBL(14) );           // 14*g
  bn128_G1_jac_add ( TBL(1) , TBL( 8) , TBL( 9) );           //  9*g
  bn128_G1_jac_add ( TBL(1) , TBL(10) , TBL(11) );           // 11*g
  bn128_G1_jac_add ( TBL(1) , TBL(12) , TBL(13) );           // 13*g
  bn128_G1_jac_add ( TBL(1) , TBL(14) , TBL(15) );           // 15*g
}

// computes `expo*grp` (or `grp^expo` in multiplicative notation)
// where `grp` is a group element in G1, and `expo` is a (non-negative) bigint
// generic windowed algo, 4-bit windows
void bn128_G1_jac_scl_windowed(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt, int expo_len) {

  // precalculate [ k*g | k <- [1..15] ]
  uint64_t table[15*3*NLIMBS_P];
  bn128_G1_jac_precalc_expos_window_16( grp, table );

  bn128_G1_jac_set_infinity( tgt );           // tgt := infinity

  int s = expo_len - 1;
  while( (s>0) && (expo[s] == 0) ) { s--; }      // skip the unneeded largest powers

  for(int i=s; i>=0; i--) {
    uint64_t e = expo[i];
    for(int j=0; j<16; j++) {
      // we can skip doubling when infinity
      if (!bn128_p_mont_is_zero(tgt+2*NLIMBS_P)) {
        bn128_G1_jac_dbl_inplace( tgt );
        bn128_G1_jac_dbl_inplace( tgt );
        bn128_G1_jac_dbl_inplace( tgt );
        bn128_G1_jac_dbl_inplace( tgt );
      }
      int k = (e >> 60);
      if (k) { 
        bn128_G1_jac_add_inplace( tgt, TBL(k) ); 
      }
      e = e << 4;
    }
  }
}

// computes `expo*grp` (or `grp^expo` in multiplicative notation)
// where `grp` is a group element in G1, and `expo` is in Fr
void bn128_G1_jac_scl_generic(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt, int nlimbs) {
  bn128_G1_jac_scl_windowed(expo, grp, tgt, nlimbs);
}

// computes `expo*grp` (or `grp^expo` in multiplicative notation)
// where `grp` is a group element in G1, and `expo` is in Fr
void bn128_G1_jac_scl_Fr(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt) {
  bn128_G1_jac_scl_generic(expo, grp, tgt, NLIMBS_R);
}

// computes `expo*grp` (or `grp^expo` in multiplicative notation)
// where `grp` is a group element in G1, and `expo` is the same size as Fp
void bn128_G1_jac_scl_big(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt) {
  bn128_G1_jac_scl_generic(expo, grp, tgt, NLIMBS_P);
}

// computes `expo*grp` (or `grp^expo` in multiplicative notation)
// where `grp` is a group element in G1, and `expo` is a 64 bit (unsigned!) word
void bn128_G1_jac_scl_small(uint64_t expo, const uint64_t *grp, uint64_t *tgt) {
  uint64_t expo_vec[1];
  expo_vec[0] = expo;
  bn128_G1_jac_scl_generic(expo_vec, grp, tgt, 1);
}
