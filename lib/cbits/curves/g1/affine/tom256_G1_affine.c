
// elliptic curve "Tom-256 ( Fp ) " in affine coordinates, Montgomery field representation
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

#include "tom256_G1_affine.h"
#include "tom256_G1_proj.h"
#include "tom256_Fp_mont.h"
#include "tom256_Fr_mont.h"

#define NLIMBS_P 4
#define NLIMBS_R 4

#define X1 (src1)
#define Y1 (src1 + 4)

#define X2 (src2)
#define Y2 (src2 + 4)

#define X3 (tgt)
#define Y3 (tgt + 4)

// the generator of the subgroup G1
const uint64_t tom256_G1_affine_gen_G1[8] = { 0x46c5fcdaeab1ecbb, 0x84a7e37e6d4a6b99, 0xfffffffffffffffb, 0x00000002fffffffc, 0x124c58b206ee49c6, 0xd724bea2d77595d3, 0xe4520ac50468963e, 0x7b2dbcca73379b61 };

// the cofactor of the curve subgroup = 1
const uint64_t tom256_G1_affine_cofactor[4] = { 0x0000000000000001, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000 };

// the size of the subgroup = 115792089210356248762697446949407573530086143415290314195533631308867097853951
const uint64_t tom256_G1_affine_subgroup_order[4] = { 0xffffffffffffffff, 0x00000000ffffffff, 0x0000000000000000, 0xffffffff00000001 };

// the constants A and B of the equation
const uint64_t tom256_G1_affine_const_A[4] = { 0x4c4d5986c712c45c, 0xf9cad0acc39cc5de, 0x0000000000000005, 0xfffffffc00000004 };
const uint64_t tom256_G1_affine_const_B[4] = { 0x798a7f2bb5224b1e, 0xa097a4c934c25d3d, 0x491c46ec3d75190c, 0xef50f2831b4853ff };
const uint64_t tom256_G1_affine_const_3B[4] = { 0x4678d0bfbbdd7f2c, 0xe4e186053c78b4c9, 0xdb54d4c4b85f4b22, 0xcdf2d78b51d8fbfb };

//------------------------------------------------------------------------------


// scale a field element by A = -3
void tom256_G1_affine_scale_by_A(const uint64_t *src, uint64_t *tgt ) {
  uint64_t tmp[4];
  tom256_Fp_mont_add( src, src, tmp );
  tom256_Fp_mont_add( src, tmp, tgt );
  tom256_Fp_mont_neg_inplace( tgt );
}

void tom256_G1_affine_scale_by_A_inplace( uint64_t *tgt ) {
  uint64_t tmp[4];
  tom256_Fp_mont_add( tgt, tgt, tmp );
  tom256_Fp_mont_add_inplace( tgt, tmp );
  tom256_Fp_mont_neg_inplace( tgt );
}

// scale a field element by B = -3
void tom256_G1_affine_scale_by_B(const uint64_t *src, uint64_t *tgt ) {
  tom256_Fp_mont_mul( src, tom256_G1_affine_const_B, tgt );
}

void tom256_G1_affine_scale_by_B_inplace( uint64_t *tgt ) {
  tom256_Fp_mont_mul_inplace( tgt, tom256_G1_affine_const_B );
}

// checks whether two curve points are equal
uint8_t tom256_G1_affine_is_equal( const uint64_t *src1, const uint64_t *src2 ) {
  return ( tom256_Fp_mont_is_equal( X1, X2 ) &&
           tom256_Fp_mont_is_equal( Y1, Y2 ) );
}

// checks whether the underlying representation is the same
uint8_t tom256_G1_affine_is_same( const uint64_t *src1, const uint64_t *src2 ) {
  return tom256_G1_affine_is_equal( src1, src2 );
}

uint8_t tom256_G1_affine_is_infinity ( const uint64_t *src1 ) {
  return ( tom256_Fp_mont_is_zero( X1 ) &&
           tom256_Fp_mont_is_zero( Y1 ) );
}

// infinity is represented by (0,0) for curves with B!=0 (like this one), and 0xffff...ffff for curves with B=0
void tom256_G1_affine_set_infinity ( uint64_t *tgt ) {
  tom256_Fp_mont_set_zero( X3 );
  tom256_Fp_mont_set_zero( Y3 );
}

// checks the curve equation
//   y^2 == x^3 + A*x + B
uint8_t tom256_G1_affine_is_on_curve ( const uint64_t *src1 ) {
  if (tom256_G1_affine_is_infinity(src1)) {
    return 1;
  }
  else {
    uint64_t acc[4];
    uint64_t tmp[4];
    tom256_Fp_mont_sqr( Y1, acc );             // Y^2
    tom256_Fp_mont_neg_inplace( acc );         // -Y^2
    tom256_Fp_mont_sqr( X1, tmp );             // X^2
    tom256_Fp_mont_mul_inplace( tmp, X1 );     // X^3
    tom256_Fp_mont_add_inplace( acc, tmp );    // - Y^2 + X^3
    tom256_G1_affine_scale_by_A( X1, tmp );       // A*X
    tom256_Fp_mont_add_inplace( acc, tmp );     // - Y^2*Z + X^3 + A*X*Z^2
    tom256_Fp_mont_add_inplace( acc, tom256_G1_affine_const_B );     // - Y^2*Z + X^3 + A*X + B
    return tom256_Fp_mont_is_zero( acc );
  }
}

// checks whether the given point is in the subgroup G1
uint8_t tom256_G1_affine_is_in_subgroup ( const uint64_t *src1 ) {
  if (!tom256_G1_affine_is_on_curve(src1)) {
    return 0;
  }
  else {
    if (tom256_G1_affine_is_infinity(src1)) {
      return 1;
    }
    else {
      uint64_t proj[12];
      uint64_t tmp [12];
      tom256_G1_proj_from_affine( src1, proj );
      tom256_G1_proj_scl_big( tom256_G1_affine_subgroup_order , proj , tmp );
      return tom256_G1_proj_is_infinity( tmp );
    }
  }
}

void tom256_G1_affine_copy( const uint64_t *src1 , uint64_t *tgt ) {
  if (tgt != src1) { memcpy( tgt, src1, 64 ); }
}

// negates an elliptic curve point in affine coordinates
void tom256_G1_affine_neg( const uint64_t *src1, uint64_t *tgt ) {
  if (tom256_G1_affine_is_infinity(src1)) {
    memcpy( tgt, src1, 64);
  }
  else {
    memcpy( tgt, src1, 32 );
    tom256_Fp_mont_neg( Y1, Y3 );
  }
}

// negates an elliptic curve point in affine coordinates
void tom256_G1_affine_neg_inplace( uint64_t *tgt ) {
  if (tom256_G1_affine_is_infinity(tgt)) {
    return;
  }
  else {
    tom256_Fp_mont_neg_inplace( Y3 );
  }
}

// doubles an affine elliptic curve point
void tom256_G1_affine_dbl( const uint64_t *src1, uint64_t *tgt ) {
  if (tom256_G1_affine_is_infinity(src1)) {
    tom256_G1_affine_copy( src1, tgt );
    return;
  }
  else {
    uint64_t  xx[4];
    uint64_t   t[4];
    uint64_t tmp[4];
    tom256_Fp_mont_sqr( X1 , xx );            // xx = X1^2
    tom256_Fp_mont_add( xx , xx, t );         // t  = 2*X1^2
    tom256_Fp_mont_add_inplace( t, xx );      // t  = 3*X1^2
    tom256_Fp_mont_add_inplace( t, tom256_G1_affine_const_A );     // t = (3*X1^2 + A)
    tom256_Fp_mont_add( Y1, Y1, tmp );             // tmp = 2*Y1
    tom256_Fp_mont_div_inplace( t, tmp );          // t   = (3*X1^2 + A) / (2*Y1)
    tom256_Fp_mont_sqr( t, tmp );                  // tmp = t^2
    tom256_Fp_mont_sub_inplace( tmp, X1 );         // tmp = t^2 - X1
    tom256_Fp_mont_sub_inplace( tmp, X1 );         // tmp = t^2 - 2*X1
    tom256_Fp_mont_sub( tmp, X1 , xx );            // xx =  (t^2 - 2*X1) - X1 = X3 - X1
    tom256_Fp_mont_mul_inplace( xx , t );          // xx = t*(X3 - X1)
    tom256_Fp_mont_add_inplace( xx , Y1);          // xx = Y1 + t*(X3 - X1)
    tom256_Fp_mont_copy( tmp, X3 );                // X3 = t^2 - 2*X1
    tom256_Fp_mont_neg ( xx, Y3 );                 // Y3 = - Y1 - t*(X3 - X1)
  }
}

// doubles an elliptic curve point, in place
void tom256_G1_affine_dbl_inplace( uint64_t *tgt ) {
  tom256_G1_affine_dbl( tgt , tgt );
}

// adds two affine elliptic curve points
void tom256_G1_affine_add( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  if (tom256_G1_affine_is_infinity(src1)) {
    // PT1 = infinity
    tom256_G1_affine_copy( src2, tgt );
    return;
  }
  if (tom256_G1_affine_is_infinity(src2)) {
    // PT2 = infinity
    tom256_G1_affine_copy( src1, tgt );
    return;
  }
  else {
    uint64_t xdif[4];
    uint64_t  tmp[4];
    uint64_t    s[4];
    tom256_Fp_mont_sub( X2, X1, xdif );             // xdif = X2 - X1
    if (tom256_Fp_mont_is_zero(xdif)) {
      // X1 == X2
      if (tom256_Fp_mont_is_equal(Y1,Y2)) {
        // Y1 == Y2, it's a doubling
        tom256_G1_affine_dbl( src1, tgt );
        return;
      }
      else {
        // Y1 /= Y2, so, it must be Y1 == -Y2, result is the point at infinity
        tom256_G1_affine_set_infinity(X3);
      }
    }
    else {
      // normal addition
      tom256_Fp_mont_sub( Y2, Y1, s );             // s   = Y2 - Y1
      tom256_Fp_mont_div_inplace( s, xdif );       // s   = (Y2 - Y1) / (X2 - X1)
      tom256_Fp_mont_sqr( s, tmp );                // tmp = s^2
      tom256_Fp_mont_sub_inplace( tmp, X1 );       // tmp = s^2 - X1
      tom256_Fp_mont_sub_inplace( tmp, X2 );       // tmp = s^2 - X1 - X2 = X3
      tom256_Fp_mont_sub( tmp, X1 , xdif );        // xdif = X3 - X1
      tom256_Fp_mont_mul_inplace( xdif, s );       // xdif = s*(X3 - X1)
      tom256_Fp_mont_add_inplace( xdif, Y1 );      // xdif = Y1 + s*(X3 - X1)
      tom256_Fp_mont_copy( tmp  , X3 );
      tom256_Fp_mont_neg ( xdif , Y3 );
    }
  }
}

void tom256_G1_affine_add_inplace( uint64_t *tgt, const uint64_t *src2 ) {
  tom256_G1_affine_add( tgt, src2, tgt);
}

void tom256_G1_affine_sub( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  uint64_t tmp[8];
  tom256_G1_affine_neg( src2, tmp );
  tom256_G1_affine_add( src1, tmp, tgt );
}

void tom256_G1_affine_sub_inplace( uint64_t *tgt, const uint64_t *src2 ) {
  uint64_t tmp[8];
  tom256_G1_affine_neg( src2, tmp );
  tom256_G1_affine_add( tgt , tmp, tgt );
}

// computes `expo*grp` (or `grp^expo` in multiplicative notation)
// where `grp` is a group element in G1, and `expo` is in Fr
void tom256_G1_affine_scl_generic(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt, int nlimbs) {
  uint64_t proj1[3*NLIMBS_P];
  uint64_t proj2[3*NLIMBS_P];
  tom256_G1_proj_from_affine( grp, proj1 );
  tom256_G1_proj_scl_generic( expo, proj1, proj2, nlimbs);
  tom256_G1_proj_to_affine( proj2, tgt );
}

// computes `expo*grp` (or `grp^expo` in multiplicative notation)
// where `grp` is a group element in G1, and `expo` is in Fr (standard repr)
void tom256_G1_affine_scl_Fr_std(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt) {
  uint64_t proj1[3*NLIMBS_P];
  uint64_t proj2[3*NLIMBS_P];
  tom256_G1_proj_from_affine( grp, proj1 );
  tom256_G1_proj_scl_Fr_std( expo, proj1, proj2);
  tom256_G1_proj_to_affine( proj2, tgt );
}

// computes `expo*grp` (or `grp^expo` in multiplicative notation)
// where `grp` is a group element in G1, and `expo` is in Fr (Montgomery repr)
void tom256_G1_affine_scl_Fr_mont(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt) {
  uint64_t proj1[3*NLIMBS_P];
  uint64_t proj2[3*NLIMBS_P];
  tom256_G1_proj_from_affine( grp, proj1 );
  tom256_G1_proj_scl_Fr_mont( expo, proj1, proj2);
  tom256_G1_proj_to_affine( proj2, tgt );
}

// computes `expo*grp` (or `grp^expo` in multiplicative notation)
// where `grp` is a group element in G1, and `expo` is the same size as Fp
void tom256_G1_affine_scl_big(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt) {
  uint64_t proj1[3*NLIMBS_P];
  uint64_t proj2[3*NLIMBS_P];
  tom256_G1_proj_from_affine( grp, proj1 );
  tom256_G1_proj_scl_big( expo, proj1, proj2 );
  tom256_G1_proj_to_affine( proj2, tgt );
}

// computes `expo*grp` (or `grp^expo` in multiplicative notation)
// where `grp` is a group element in G1, and `expo` is a 64 bit (unsigned!) word
void tom256_G1_affine_scl_small(uint64_t expo, const uint64_t *grp, uint64_t *tgt) {
  uint64_t proj1[3*NLIMBS_P];
  uint64_t proj2[3*NLIMBS_P];
  tom256_G1_proj_from_affine( grp, proj1 );
  tom256_G1_proj_scl_small( expo, proj1, proj2 );
  tom256_G1_proj_to_affine( proj2, tgt );
}
