
// elliptic curve "BLS12-381" in affine coordinates, Montgomery field representation
//
// NOTES:
//  - generated code, do not edit!
//  - the point at infinity is represented by the special string 0xffff ..fffff
//    this is not a valid value for prime fields, so it's OK as long as we always check for it

#include <string.h>
#include <stdint.h>
#include <x86intrin.h>

#include "bls12_381_G1_affine.h"
#include "bls12_381_G1_proj.h"
#include "bls12_381_p_mont.h"
#include "bls12_381_r_mont.h"

#define NLIMBS_P 6
#define NLIMBS_R 4

#define X1 (src1)
#define Y1 (src1 + 6)

#define X2 (src2)
#define Y2 (src2 + 6)

#define X3 (tgt)
#define Y3 (tgt + 6)

// the generator of the subgroup G1
const uint64_t bls12_381_G1_affine_gen_G1[12] = { 0x5cb38790fd530c16, 0x7817fc679976fff5, 0x154f95c7143ba1c1, 0xf0ae6acdf3d0e747, 0xedce6ecc21dbf440, 0x120177419e0bfb75, 0xbaac93d50ce72271, 0x8c22631a7918fd8e, 0xdd595f13570725ce, 0x51ac582950405194, 0x0e1c8c3fad0059c0, 0x0bbc3efc5008a26a };

// the cofactor of the curve subgroup = 76329603384216526031706109802092473003
const uint64_t bls12_381_G1_affine_cofactor[6] = { 0x8c00aaab0000aaab, 0x396c8c005555e156, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000 };

// the constants A and B of the equation
const uint64_t bls12_381_G1_affine_const_A[6] = { 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000 };
const uint64_t bls12_381_G1_affine_const_B[6] = { 0xaa270000000cfff3, 0x53cc0032fc34000a, 0x478fe97a6b0a807f, 0xb1d37ebee6ba24d7, 0x8ec9733bbf78ab2f, 0x09d645513d83de7e };

//------------------------------------------------------------------------------

void bls12_381_G1_affine_set_ffff( uint64_t *tgt ) {
  memset( tgt, 0xff, 48 );
}

uint8_t bls12_381_G1_affine_is_ffff( const uint64_t *src ) {
  return ( (src[0] + 1 == 0) && (src[1] + 1 == 0) && (src[2] + 1 == 0) && (src[3] + 1 == 0) && (src[4] + 1 == 0) && (src[5] + 1 == 0) );
}

// checks whether two curve points are equal
uint8_t bls12_381_G1_affine_is_equal( const uint64_t *src1, const uint64_t *src2 ) {
  return ( bls12_381_p_mont_is_equal( X1, X2 ) &&
           bls12_381_p_mont_is_equal( Y1, Y2 ) );
}

// checks whether the underlying representation is the same
uint8_t bls12_381_G1_affine_is_same( const uint64_t *src1, const uint64_t *src2 ) {
  return bls12_381_G1_affine_is_equal( src1, src2 );
}

uint8_t bls12_381_G1_affine_is_infinity ( const uint64_t *src1 ) {
  return ( bls12_381_G1_affine_is_ffff( X1 ) &&
           bls12_381_G1_affine_is_ffff( Y1 ) );
}

void bls12_381_G1_affine_set_infinity ( uint64_t *tgt ) {
  bls12_381_G1_affine_set_ffff( X3 );
  bls12_381_G1_affine_set_ffff( Y3 );
}

// checks the curve equation
//   y^2 == x^3 + A*x + B
uint8_t bls12_381_G1_affine_is_on_curve ( const uint64_t *src1 ) {
  if (bls12_381_G1_affine_is_infinity(src1)) {
    return 1;
  }
  else {
    uint64_t acc[6];
    uint64_t tmp[6];
    bls12_381_p_mont_sqr( Y1, acc );             // Y^2
    bls12_381_p_mont_neg_inplace( acc );         // -Y^2
    bls12_381_p_mont_sqr( X1, tmp );             // X^2
    bls12_381_p_mont_mul_inplace( tmp, X1 );     // X^3
    bls12_381_p_mont_add_inplace( acc, tmp );    // - Y^2 + X^3
    bls12_381_p_mont_add_inplace( acc, bls12_381_G1_affine_const_B );     // - Y^2*Z + X^3 + A*X + B
    return bls12_381_p_mont_is_zero( acc );
  }
}

// checks whether the given point is in the subgroup G1
uint8_t bls12_381_G1_affine_is_in_subgroup ( const uint64_t *src1 ) {
  if (!bls12_381_G1_affine_is_on_curve(src1)) {
    return 0;
  }
  else {
    if (bls12_381_G1_affine_is_infinity(src1)) {
      return 1;
    }
    else {
      uint64_t proj[18];
      uint64_t tmp [18];
      bls12_381_G1_proj_from_affine( src1, proj );
      bls12_381_G1_proj_scl_Fr( bls12_381_G1_affine_cofactor , proj , tmp );
      return bls12_381_G1_proj_is_infinity( tmp );
    }
  }
}

void bls12_381_G1_affine_copy( const uint64_t *src1 , uint64_t *tgt ) {
  memcpy( tgt, src1, 96 );
}

// negates an elliptic curve point in affine coordinates
void bls12_381_G1_affine_neg( const uint64_t *src1, uint64_t *tgt ) {
  if (bls12_381_G1_affine_is_infinity(src1)) {
    return;
  }
  else {
    memcpy( tgt, src1, 48 );
    bls12_381_p_mont_neg( Y1, Y3 );
  }
}

// negates an elliptic curve point in affine coordinates
void bls12_381_G1_affine_neg_inplace( uint64_t *tgt ) {
  if (bls12_381_G1_affine_is_infinity(tgt)) {
    return;
  }
  else {
    bls12_381_p_mont_neg_inplace( Y3 );
  }
}

// doubles an affine elliptic curve point
void bls12_381_G1_affine_dbl( const uint64_t *src1, uint64_t *tgt ) {
  if (bls12_381_G1_affine_is_infinity(src1)) {
    bls12_381_G1_affine_copy( src1, tgt );
    return;
  }
  else {
    uint64_t  xx[6];
    uint64_t   t[6];
    uint64_t tmp[6];
    bls12_381_p_mont_sqr( X1 , xx );            // xx = X1^2
    bls12_381_p_mont_add( xx , xx, t );         // t  = 2*X1^2
    bls12_381_p_mont_add_inplace( t, xx );      // t  = 3*X1^2
    bls12_381_p_mont_add( Y1, Y1, tmp );             // tmp = 2*Y1
    bls12_381_p_mont_div_inplace( t, tmp );          // t   = (3*X1^2 + A) / (2*Y1)
    bls12_381_p_mont_sqr( t, tmp );                  // tmp = t^2
    bls12_381_p_mont_sub_inplace( tmp, X1 );         // tmp = t^2 - X1
    bls12_381_p_mont_sub_inplace( tmp, X1 );         // tmp = t^2 - 2*X1
    bls12_381_p_mont_sub( tmp, X1 , xx );            // xx =  (t^2 - 2*X1) - X1 = X3 - X1
    bls12_381_p_mont_mul_inplace( xx , t );          // xx = t*(X3 - X1)
    bls12_381_p_mont_add_inplace( xx , Y1);          // xx = Y1 + t*(X3 - X1)
    bls12_381_p_mont_copy( tmp, X3 );                // X3 = t^2 - 2*X1
    bls12_381_p_mont_neg ( xx, Y3 );                 // Y3 = - Y1 - t*(X3 - X1)
  }
}

// doubles an elliptic curve point, in place
void bls12_381_G1_affine_dbl_inplace( uint64_t *tgt ) {
  bls12_381_G1_affine_dbl( tgt , tgt );
}

// adds two affine elliptic curve points
void bls12_381_G1_affine_add( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  if (bls12_381_G1_affine_is_infinity(src1)) {
    // PT1 = infinity
    bls12_381_G1_affine_copy( src2, tgt );
    return;
  }
  if (bls12_381_G1_affine_is_infinity(src2)) {
    // PT2 = infinity
    bls12_381_G1_affine_copy( src1, tgt );
    return;
  }
  else {
    uint64_t xdif[6];
    uint64_t  tmp[6];
    uint64_t    s[6];
    bls12_381_p_mont_sub( X2, X1, xdif );             // xdif = X2 - X1
    if (bls12_381_p_mont_is_zero(xdif)) {
      // X1 == X2
      if (bls12_381_p_mont_is_equal(Y1,Y2)) {
        // Y1 == Y2, it's a doubling
        bls12_381_G1_affine_dbl( src1, tgt );
        return;
      }
      else {
        // Y1 /= Y2, so, it must be Y1 == -Y2, result is the point at infinity
        bls12_381_G1_affine_set_infinity(X3);
      }
    }
    else {
      // normal addition
      bls12_381_p_mont_sub( Y2, Y1, s );             // s   = Y2 - Y1
      bls12_381_p_mont_div_inplace( s, xdif );       // s   = (Y2 - Y1) / (X2 - X1)
      bls12_381_p_mont_sqr( s, tmp );                // tmp = s^2
      bls12_381_p_mont_sub_inplace( tmp, X1 );       // tmp = s^2 - X1
      bls12_381_p_mont_sub_inplace( tmp, X2 );       // tmp = s^2 - X1 - X2 = X3
      bls12_381_p_mont_sub( tmp, X1 , xdif );        // xdif = X3 - X1
      bls12_381_p_mont_mul_inplace( xdif, s );       // xdif = s*(X3 - X1)
      bls12_381_p_mont_add_inplace( xdif, Y1 );      // xdif = Y1 + s*(X3 - X1)
      bls12_381_p_mont_copy( tmp  , X3 );
      bls12_381_p_mont_neg ( xdif , Y3 );
    }
  }
}

void bls12_381_G1_affine_add_inplace( uint64_t *tgt, const uint64_t *src2 ) {
  bls12_381_G1_affine_add( tgt, src2, tgt);
}

void bls12_381_G1_affine_sub( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  uint64_t tmp[12];
  bls12_381_G1_affine_neg( src2, tmp );
  bls12_381_G1_affine_add( src1, tmp, tgt );
}

void bls12_381_G1_affine_sub_inplace( uint64_t *tgt, const uint64_t *src2 ) {
  uint64_t tmp[12];
  bls12_381_G1_affine_neg( src2, tmp );
  bls12_381_G1_affine_add( tgt , tmp, tgt );
}

// computes `expo*grp` (or `grp^expo` in multiplicative notation)
// where `grp` is a group element in G1, and `expo` is in Fr
void bls12_381_G1_affine_scl_generic(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt, int nlimbs) {
  uint64_t proj1[3*NLIMBS_P];
  uint64_t proj2[3*NLIMBS_P];
  bls12_381_G1_proj_from_affine( grp, proj1 );
  bls12_381_G1_proj_scl_generic( expo, proj1, proj2, nlimbs);
  bls12_381_G1_proj_to_affine( proj2, tgt );
}

// computes `expo*grp` (or `grp^expo` in multiplicative notation)
// where `grp` is a group element in G1, and `expo` is in Fr
void bls12_381_G1_affine_scl_Fr(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt) {
  uint64_t proj1[3*NLIMBS_P];
  uint64_t proj2[3*NLIMBS_P];
  bls12_381_G1_proj_from_affine( grp, proj1 );
  bls12_381_G1_proj_scl_Fr( expo, proj1, proj2);
  bls12_381_G1_proj_to_affine( proj2, tgt );
}

// computes `expo*grp` (or `grp^expo` in multiplicative notation)
// where `grp` is a group element in G1, and `expo` is the same size as Fp
void bls12_381_G1_affine_scl_big(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt) {
  uint64_t proj1[3*NLIMBS_P];
  uint64_t proj2[3*NLIMBS_P];
  bls12_381_G1_proj_from_affine( grp, proj1 );
  bls12_381_G1_proj_scl_big( expo, proj1, proj2 );
  bls12_381_G1_proj_to_affine( proj2, tgt );
}

// computes `expo*grp` (or `grp^expo` in multiplicative notation)
// where `grp` is a group element in G1, and `expo` is a 64 bit (unsigned!) word
void bls12_381_G1_affine_scl_small(uint64_t expo, const uint64_t *grp, uint64_t *tgt) {
  uint64_t proj1[3*NLIMBS_P];
  uint64_t proj2[3*NLIMBS_P];
  bls12_381_G1_proj_from_affine( grp, proj1 );
  bls12_381_G1_proj_scl_small( expo, proj1, proj2 );
  bls12_381_G1_proj_to_affine( proj2, tgt );
}
