
// elliptic curve "P256 ( Fp ) " in affine coordinates, Montgomery field representation
//
// NOTES:
//  - generated code, do not edit!
//  - the point at infinity is represented by (0,0) if B!=0, and the special string 0xffff ..fffff if B==0.
//    0xffff...ffff is not a valid value for prime fields, so it's OK as long as we always check for it.
//    however other libraries use (0,0), which is not a valid curve point as long as B!=0, so we adapt that
//    because interop is too painful otherwise

#include <string.h>
#include <stdlib.h>
#include <stdint.h>

#include "p256_G_affine.h"
#include "p256_G_proj.h"
#include "p256_Fp_mont.h"
#include "p256_Fr_mont.h"

#define NLIMBS_P 4
#define NLIMBS_R 4

#define X1 (src1)
#define Y1 (src1 + 4)

#define X2 (src2)
#define Y2 (src2 + 4)

#define X3 (tgt)
#define Y3 (tgt + 4)

// the generator of the subgroup G1
const uint64_t p256_G_affine_gen_G1[8] = { 0x79e730d418a9143c, 0x75ba95fc5fedb601, 0x79fb732b77622510, 0x18905f76a53755c6, 0xddf25357ce95560a, 0x8b4ab8e4ba19e45c, 0xd2e88688dd21f325, 0x8571ff1825885d85 };

// the cofactor of the curve subgroup = 1
const uint64_t p256_G_affine_cofactor[4] = { 0x0000000000000001, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000 };

// the constants A and B of the equation
const uint64_t p256_G_affine_const_A[4] = { 0xfffffffffffffffc, 0x00000003ffffffff, 0x0000000000000000, 0xfffffffc00000004 };
const uint64_t p256_G_affine_const_B[4] = { 0xd89cdf6229c4bddf, 0xacf005cd78843090, 0xe5a220abf7212ed6, 0xdc30061d04874834 };
const uint64_t p256_G_affine_const_3B[4] = { 0x89d69e267d4e399f, 0x06d01166698c91b2, 0xb0e66203e5638c84, 0x949012590d95d89c };

//------------------------------------------------------------------------------


// checks whether two curve points are equal
uint8_t p256_G_affine_is_equal( const uint64_t *src1, const uint64_t *src2 ) {
  return ( p256_Fp_mont_is_equal( X1, X2 ) &&
           p256_Fp_mont_is_equal( Y1, Y2 ) );
}

// checks whether the underlying representation is the same
uint8_t p256_G_affine_is_same( const uint64_t *src1, const uint64_t *src2 ) {
  return p256_G_affine_is_equal( src1, src2 );
}

uint8_t p256_G_affine_is_infinity ( const uint64_t *src1 ) {
  return ( p256_Fp_mont_is_zero( X1 ) &&
           p256_Fp_mont_is_zero( Y1 ) );
}

// infinity is represented by (0,0) for curves with B!=0 (like this one), and 0xffff...ffff for curves with B=0
void p256_G_affine_set_infinity ( uint64_t *tgt ) {
  p256_Fp_mont_set_zero( X3 );
  p256_Fp_mont_set_zero( Y3 );
}

// checks the curve equation
//   y^2 == x^3 + A*x + B
uint8_t p256_G_affine_is_on_curve ( const uint64_t *src1 ) {
  if (p256_G_affine_is_infinity(src1)) {
    return 1;
  }
  else {
    uint64_t acc[4];
    uint64_t tmp[4];
    p256_Fp_mont_sqr( Y1, acc );             // Y^2
    p256_Fp_mont_neg_inplace( acc );         // -Y^2
    p256_Fp_mont_sqr( X1, tmp );             // X^2
    p256_Fp_mont_mul_inplace( tmp, X1 );     // X^3
    p256_Fp_mont_add_inplace( acc, tmp );    // - Y^2 + X^3
    p256_G_affine_scale_by_A( X1, tmp );       // A*X
    p256_Fp_mont_add_inplace( acc, tmp );     // - Y^2*Z + X^3 + A*X*Z^2
    p256_Fp_mont_add_inplace( acc, p256_G_affine_const_B );     // - Y^2*Z + X^3 + A*X + B
    return p256_Fp_mont_is_zero( acc );
  }
}

// checks whether the given point is in the subgroup G1
uint8_t p256_G_affine_is_in_subgroup ( const uint64_t *src1 ) {
  if (!p256_G_affine_is_on_curve(src1)) {
    return 0;
  }
  else {
    if (p256_G_affine_is_infinity(src1)) {
      return 1;
    }
    else {
      uint64_t proj[12];
      uint64_t tmp [12];
      p256_G_proj_from_affine( src1, proj );
      p256_G_proj_scl_Fr_std( p256_G_affine_cofactor , proj , tmp );
      return p256_G_proj_is_infinity( tmp );
    }
  }
}

void p256_G_affine_copy( const uint64_t *src1 , uint64_t *tgt ) {
  if (tgt != src1) { memcpy( tgt, src1, 64 ); }
}

// negates an elliptic curve point in affine coordinates
void p256_G_affine_neg( const uint64_t *src1, uint64_t *tgt ) {
  if (p256_G_affine_is_infinity(src1)) {
    memcpy( tgt, src1, 64);
  }
  else {
    memcpy( tgt, src1, 32 );
    p256_Fp_mont_neg( Y1, Y3 );
  }
}

// negates an elliptic curve point in affine coordinates
void p256_G_affine_neg_inplace( uint64_t *tgt ) {
  if (p256_G_affine_is_infinity(tgt)) {
    return;
  }
  else {
    p256_Fp_mont_neg_inplace( Y3 );
  }
}

// doubles an affine elliptic curve point
void p256_G_affine_dbl( const uint64_t *src1, uint64_t *tgt ) {
  if (p256_G_affine_is_infinity(src1)) {
    p256_G_affine_copy( src1, tgt );
    return;
  }
  else {
    uint64_t  xx[4];
    uint64_t   t[4];
    uint64_t tmp[4];
    p256_Fp_mont_sqr( X1 , xx );            // xx = X1^2
    p256_Fp_mont_add( xx , xx, t );         // t  = 2*X1^2
    p256_Fp_mont_add_inplace( t, xx );      // t  = 3*X1^2
    p256_Fp_mont_add_inplace( t, p256_G_affine_const_A );     // t = (3*X1^2 + A)
    p256_Fp_mont_add( Y1, Y1, tmp );             // tmp = 2*Y1
    p256_Fp_mont_div_inplace( t, tmp );          // t   = (3*X1^2 + A) / (2*Y1)
    p256_Fp_mont_sqr( t, tmp );                  // tmp = t^2
    p256_Fp_mont_sub_inplace( tmp, X1 );         // tmp = t^2 - X1
    p256_Fp_mont_sub_inplace( tmp, X1 );         // tmp = t^2 - 2*X1
    p256_Fp_mont_sub( tmp, X1 , xx );            // xx =  (t^2 - 2*X1) - X1 = X3 - X1
    p256_Fp_mont_mul_inplace( xx , t );          // xx = t*(X3 - X1)
    p256_Fp_mont_add_inplace( xx , Y1);          // xx = Y1 + t*(X3 - X1)
    p256_Fp_mont_copy( tmp, X3 );                // X3 = t^2 - 2*X1
    p256_Fp_mont_neg ( xx, Y3 );                 // Y3 = - Y1 - t*(X3 - X1)
  }
}

// doubles an elliptic curve point, in place
void p256_G_affine_dbl_inplace( uint64_t *tgt ) {
  p256_G_affine_dbl( tgt , tgt );
}

// adds two affine elliptic curve points
void p256_G_affine_add( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  if (p256_G_affine_is_infinity(src1)) {
    // PT1 = infinity
    p256_G_affine_copy( src2, tgt );
    return;
  }
  if (p256_G_affine_is_infinity(src2)) {
    // PT2 = infinity
    p256_G_affine_copy( src1, tgt );
    return;
  }
  else {
    uint64_t xdif[4];
    uint64_t  tmp[4];
    uint64_t    s[4];
    p256_Fp_mont_sub( X2, X1, xdif );             // xdif = X2 - X1
    if (p256_Fp_mont_is_zero(xdif)) {
      // X1 == X2
      if (p256_Fp_mont_is_equal(Y1,Y2)) {
        // Y1 == Y2, it's a doubling
        p256_G_affine_dbl( src1, tgt );
        return;
      }
      else {
        // Y1 /= Y2, so, it must be Y1 == -Y2, result is the point at infinity
        p256_G_affine_set_infinity(X3);
      }
    }
    else {
      // normal addition
      p256_Fp_mont_sub( Y2, Y1, s );             // s   = Y2 - Y1
      p256_Fp_mont_div_inplace( s, xdif );       // s   = (Y2 - Y1) / (X2 - X1)
      p256_Fp_mont_sqr( s, tmp );                // tmp = s^2
      p256_Fp_mont_sub_inplace( tmp, X1 );       // tmp = s^2 - X1
      p256_Fp_mont_sub_inplace( tmp, X2 );       // tmp = s^2 - X1 - X2 = X3
      p256_Fp_mont_sub( tmp, X1 , xdif );        // xdif = X3 - X1
      p256_Fp_mont_mul_inplace( xdif, s );       // xdif = s*(X3 - X1)
      p256_Fp_mont_add_inplace( xdif, Y1 );      // xdif = Y1 + s*(X3 - X1)
      p256_Fp_mont_copy( tmp  , X3 );
      p256_Fp_mont_neg ( xdif , Y3 );
    }
  }
}

void p256_G_affine_add_inplace( uint64_t *tgt, const uint64_t *src2 ) {
  p256_G_affine_add( tgt, src2, tgt);
}

void p256_G_affine_sub( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  uint64_t tmp[8];
  p256_G_affine_neg( src2, tmp );
  p256_G_affine_add( src1, tmp, tgt );
}

void p256_G_affine_sub_inplace( uint64_t *tgt, const uint64_t *src2 ) {
  uint64_t tmp[8];
  p256_G_affine_neg( src2, tmp );
  p256_G_affine_add( tgt , tmp, tgt );
}

// computes `expo*grp` (or `grp^expo` in multiplicative notation)
// where `grp` is a group element in G1, and `expo` is in Fr
void p256_G_affine_scl_generic(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt, int nlimbs) {
  uint64_t proj1[3*NLIMBS_P];
  uint64_t proj2[3*NLIMBS_P];
  p256_G_proj_from_affine( grp, proj1 );
  p256_G_proj_scl_generic( expo, proj1, proj2, nlimbs);
  p256_G_proj_to_affine( proj2, tgt );
}

// computes `expo*grp` (or `grp^expo` in multiplicative notation)
// where `grp` is a group element in G1, and `expo` is in Fr (standard repr)
void p256_G_affine_scl_Fr_std(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt) {
  uint64_t proj1[3*NLIMBS_P];
  uint64_t proj2[3*NLIMBS_P];
  p256_G_proj_from_affine( grp, proj1 );
  p256_G_proj_scl_Fr_std( expo, proj1, proj2);
  p256_G_proj_to_affine( proj2, tgt );
}

// computes `expo*grp` (or `grp^expo` in multiplicative notation)
// where `grp` is a group element in G1, and `expo` is in Fr (Montgomery repr)
void p256_G_affine_scl_Fr_mont(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt) {
  uint64_t proj1[3*NLIMBS_P];
  uint64_t proj2[3*NLIMBS_P];
  p256_G_proj_from_affine( grp, proj1 );
  p256_G_proj_scl_Fr_mont( expo, proj1, proj2);
  p256_G_proj_to_affine( proj2, tgt );
}

// computes `expo*grp` (or `grp^expo` in multiplicative notation)
// where `grp` is a group element in G1, and `expo` is the same size as Fp
void p256_G_affine_scl_big(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt) {
  uint64_t proj1[3*NLIMBS_P];
  uint64_t proj2[3*NLIMBS_P];
  p256_G_proj_from_affine( grp, proj1 );
  p256_G_proj_scl_big( expo, proj1, proj2 );
  p256_G_proj_to_affine( proj2, tgt );
}

// computes `expo*grp` (or `grp^expo` in multiplicative notation)
// where `grp` is a group element in G1, and `expo` is a 64 bit (unsigned!) word
void p256_G_affine_scl_small(uint64_t expo, const uint64_t *grp, uint64_t *tgt) {
  uint64_t proj1[3*NLIMBS_P];
  uint64_t proj2[3*NLIMBS_P];
  p256_G_proj_from_affine( grp, proj1 );
  p256_G_proj_scl_small( expo, proj1, proj2 );
  p256_G_proj_to_affine( proj2, tgt );
}
