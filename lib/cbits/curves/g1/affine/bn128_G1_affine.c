
// elliptic curve "BN128" in affine coordinates, Montgomery field representation
//
// NOTES:
//  - generated code, do not edit!
//  - the point at infinity is represented by the special string 0xffff ..fffff
//    this is not a valid value for prime fields, so it's OK as long as we always check for it

#include <string.h>
#include <stdint.h>
#include <x86intrin.h>

#include "bn128_G1_affine.h"
#include "bn128_G1_proj.h"
#include "bn128_p_mont.h"
#include "bn128_r_mont.h"

#define NLIMBS_P 4
#define NLIMBS_R 4

#define X1 (src1)
#define Y1 (src1 + 4)

#define X2 (src2)
#define Y2 (src2 + 4)

#define X3 (tgt)
#define Y3 (tgt + 4)

// the generator of the subgroup G1
const uint64_t bn128_G1_affine_gen_G1[8] = { 0xd35d438dc58f0d9d, 0x0a78eb28f5c70b3d, 0x666ea36f7879462c, 0x0e0a77c19a07df2f, 0xa6ba871b8b1e1b3a, 0x14f1d651eb8e167b, 0xccdd46def0f28c58, 0x1c14ef83340fbe5e };

// the cofactor of the curve subgroup = 1
const uint64_t bn128_G1_affine_cofactor[4] = { 0x0000000000000001, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000 };

// the constants A and B of the equation
const uint64_t bn128_G1_affine_const_A[4] = { 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000 };
const uint64_t bn128_G1_affine_const_B[4] = { 0x7a17caa950ad28d7, 0x1f6ac17ae15521b9, 0x334bea4e696bd284, 0x2a1f6744ce179d8e };

//------------------------------------------------------------------------------

void bn128_G1_affine_set_ffff( uint64_t *tgt ) {
  memset( tgt, 0xff, 32 );
}

uint8_t bn128_G1_affine_is_ffff( const uint64_t *src ) {
  return ( (src[0] + 1 == 0) && (src[1] + 1 == 0) && (src[2] + 1 == 0) && (src[3] + 1 == 0) );
}

// checks whether two curve points are equal
uint8_t bn128_G1_affine_is_equal( const uint64_t *src1, const uint64_t *src2 ) {
  return ( bn128_p_mont_is_equal( X1, X2 ) &&
           bn128_p_mont_is_equal( Y1, Y2 ) );
}

// checks whether the underlying representation is the same
uint8_t bn128_G1_affine_is_same( const uint64_t *src1, const uint64_t *src2 ) {
  return bn128_G1_affine_is_equal( src1, src2 );
}

uint8_t bn128_G1_affine_is_infinity ( const uint64_t *src1 ) {
  return ( bn128_G1_affine_is_ffff( X1 ) &&
           bn128_G1_affine_is_ffff( Y1 ) );
}

void bn128_G1_affine_set_infinity ( uint64_t *tgt ) {
  bn128_G1_affine_set_ffff( X3 );
  bn128_G1_affine_set_ffff( Y3 );
}

// checks the curve equation
//   y^2 == x^3 + A*x + B
uint8_t bn128_G1_affine_is_on_curve ( const uint64_t *src1 ) {
  if (bn128_G1_affine_is_infinity(src1)) {
    return 1;
  }
  else {
    uint64_t acc[4];
    uint64_t tmp[4];
    bn128_p_mont_sqr( Y1, acc );             // Y^2
    bn128_p_mont_neg_inplace( acc );         // -Y^2
    bn128_p_mont_sqr( X1, tmp );             // X^2
    bn128_p_mont_mul_inplace( tmp, X1 );     // X^3
    bn128_p_mont_add_inplace( acc, tmp );    // - Y^2 + X^3
    bn128_p_mont_add_inplace( acc, bn128_G1_affine_const_B );     // - Y^2*Z + X^3 + A*X + B
    return bn128_p_mont_is_zero( acc );
  }
}

// checks whether the given point is in the subgroup G1
uint8_t bn128_G1_affine_is_in_subgroup ( const uint64_t *src1 ) {
  if (!bn128_G1_affine_is_on_curve(src1)) {
    return 0;
  }
  else {
    if (bn128_G1_affine_is_infinity(src1)) {
      return 1;
    }
    else {
      uint64_t proj[12];
      uint64_t tmp [12];
      bn128_G1_proj_from_affine( src1, proj );
      bn128_G1_proj_scl_Fr( bn128_G1_affine_cofactor , proj , tmp );
      return bn128_G1_proj_is_infinity( tmp );
    }
  }
}

void bn128_G1_affine_copy( const uint64_t *src1 , uint64_t *tgt ) {
  if (tgt != src1) { memcpy( tgt, src1, 64 ); }
}

// negates an elliptic curve point in affine coordinates
void bn128_G1_affine_neg( const uint64_t *src1, uint64_t *tgt ) {
  if (bn128_G1_affine_is_infinity(src1)) {
    return;
  }
  else {
    memcpy( tgt, src1, 32 );
    bn128_p_mont_neg( Y1, Y3 );
  }
}

// negates an elliptic curve point in affine coordinates
void bn128_G1_affine_neg_inplace( uint64_t *tgt ) {
  if (bn128_G1_affine_is_infinity(tgt)) {
    return;
  }
  else {
    bn128_p_mont_neg_inplace( Y3 );
  }
}

// doubles an affine elliptic curve point
void bn128_G1_affine_dbl( const uint64_t *src1, uint64_t *tgt ) {
  if (bn128_G1_affine_is_infinity(src1)) {
    bn128_G1_affine_copy( src1, tgt );
    return;
  }
  else {
    uint64_t  xx[4];
    uint64_t   t[4];
    uint64_t tmp[4];
    bn128_p_mont_sqr( X1 , xx );            // xx = X1^2
    bn128_p_mont_add( xx , xx, t );         // t  = 2*X1^2
    bn128_p_mont_add_inplace( t, xx );      // t  = 3*X1^2
    bn128_p_mont_add( Y1, Y1, tmp );             // tmp = 2*Y1
    bn128_p_mont_div_inplace( t, tmp );          // t   = (3*X1^2 + A) / (2*Y1)
    bn128_p_mont_sqr( t, tmp );                  // tmp = t^2
    bn128_p_mont_sub_inplace( tmp, X1 );         // tmp = t^2 - X1
    bn128_p_mont_sub_inplace( tmp, X1 );         // tmp = t^2 - 2*X1
    bn128_p_mont_sub( tmp, X1 , xx );            // xx =  (t^2 - 2*X1) - X1 = X3 - X1
    bn128_p_mont_mul_inplace( xx , t );          // xx = t*(X3 - X1)
    bn128_p_mont_add_inplace( xx , Y1);          // xx = Y1 + t*(X3 - X1)
    bn128_p_mont_copy( tmp, X3 );                // X3 = t^2 - 2*X1
    bn128_p_mont_neg ( xx, Y3 );                 // Y3 = - Y1 - t*(X3 - X1)
  }
}

// doubles an elliptic curve point, in place
void bn128_G1_affine_dbl_inplace( uint64_t *tgt ) {
  bn128_G1_affine_dbl( tgt , tgt );
}

// adds two affine elliptic curve points
void bn128_G1_affine_add( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  if (bn128_G1_affine_is_infinity(src1)) {
    // PT1 = infinity
    bn128_G1_affine_copy( src2, tgt );
    return;
  }
  if (bn128_G1_affine_is_infinity(src2)) {
    // PT2 = infinity
    bn128_G1_affine_copy( src1, tgt );
    return;
  }
  else {
    uint64_t xdif[4];
    uint64_t  tmp[4];
    uint64_t    s[4];
    bn128_p_mont_sub( X2, X1, xdif );             // xdif = X2 - X1
    if (bn128_p_mont_is_zero(xdif)) {
      // X1 == X2
      if (bn128_p_mont_is_equal(Y1,Y2)) {
        // Y1 == Y2, it's a doubling
        bn128_G1_affine_dbl( src1, tgt );
        return;
      }
      else {
        // Y1 /= Y2, so, it must be Y1 == -Y2, result is the point at infinity
        bn128_G1_affine_set_infinity(X3);
      }
    }
    else {
      // normal addition
      bn128_p_mont_sub( Y2, Y1, s );             // s   = Y2 - Y1
      bn128_p_mont_div_inplace( s, xdif );       // s   = (Y2 - Y1) / (X2 - X1)
      bn128_p_mont_sqr( s, tmp );                // tmp = s^2
      bn128_p_mont_sub_inplace( tmp, X1 );       // tmp = s^2 - X1
      bn128_p_mont_sub_inplace( tmp, X2 );       // tmp = s^2 - X1 - X2 = X3
      bn128_p_mont_sub( tmp, X1 , xdif );        // xdif = X3 - X1
      bn128_p_mont_mul_inplace( xdif, s );       // xdif = s*(X3 - X1)
      bn128_p_mont_add_inplace( xdif, Y1 );      // xdif = Y1 + s*(X3 - X1)
      bn128_p_mont_copy( tmp  , X3 );
      bn128_p_mont_neg ( xdif , Y3 );
    }
  }
}

void bn128_G1_affine_add_inplace( uint64_t *tgt, const uint64_t *src2 ) {
  bn128_G1_affine_add( tgt, src2, tgt);
}

void bn128_G1_affine_sub( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  uint64_t tmp[8];
  bn128_G1_affine_neg( src2, tmp );
  bn128_G1_affine_add( src1, tmp, tgt );
}

void bn128_G1_affine_sub_inplace( uint64_t *tgt, const uint64_t *src2 ) {
  uint64_t tmp[8];
  bn128_G1_affine_neg( src2, tmp );
  bn128_G1_affine_add( tgt , tmp, tgt );
}

// computes `expo*grp` (or `grp^expo` in multiplicative notation)
// where `grp` is a group element in G1, and `expo` is in Fr
void bn128_G1_affine_scl_generic(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt, int nlimbs) {
  uint64_t proj1[3*NLIMBS_P];
  uint64_t proj2[3*NLIMBS_P];
  bn128_G1_proj_from_affine( grp, proj1 );
  bn128_G1_proj_scl_generic( expo, proj1, proj2, nlimbs);
  bn128_G1_proj_to_affine( proj2, tgt );
}

// computes `expo*grp` (or `grp^expo` in multiplicative notation)
// where `grp` is a group element in G1, and `expo` is in Fr
void bn128_G1_affine_scl_Fr(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt) {
  uint64_t proj1[3*NLIMBS_P];
  uint64_t proj2[3*NLIMBS_P];
  bn128_G1_proj_from_affine( grp, proj1 );
  bn128_G1_proj_scl_Fr( expo, proj1, proj2);
  bn128_G1_proj_to_affine( proj2, tgt );
}

// computes `expo*grp` (or `grp^expo` in multiplicative notation)
// where `grp` is a group element in G1, and `expo` is the same size as Fp
void bn128_G1_affine_scl_big(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt) {
  uint64_t proj1[3*NLIMBS_P];
  uint64_t proj2[3*NLIMBS_P];
  bn128_G1_proj_from_affine( grp, proj1 );
  bn128_G1_proj_scl_big( expo, proj1, proj2 );
  bn128_G1_proj_to_affine( proj2, tgt );
}

// computes `expo*grp` (or `grp^expo` in multiplicative notation)
// where `grp` is a group element in G1, and `expo` is a 64 bit (unsigned!) word
void bn128_G1_affine_scl_small(uint64_t expo, const uint64_t *grp, uint64_t *tgt) {
  uint64_t proj1[3*NLIMBS_P];
  uint64_t proj2[3*NLIMBS_P];
  bn128_G1_proj_from_affine( grp, proj1 );
  bn128_G1_proj_scl_small( expo, proj1, proj2 );
  bn128_G1_proj_to_affine( proj2, tgt );
}
