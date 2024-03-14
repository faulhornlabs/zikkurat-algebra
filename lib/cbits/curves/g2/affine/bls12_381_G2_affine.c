
// elliptic curve "BLS12-381 ( Fp2 ) " in affine coordinates, Montgomery field representation
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

#include "bls12_381_G2_affine.h"
#include "bls12_381_G2_proj.h"
#include "bls12_381_Fp2_mont.h"
#include "bls12_381_Fr_mont.h"

#define NLIMBS_P 12
#define NLIMBS_R 4

#define X1 (src1)
#define Y1 (src1 + 12)

#define X2 (src2)
#define Y2 (src2 + 12)

#define X3 (tgt)
#define Y3 (tgt + 12)

// the generator of the subgroup G2
const uint64_t bls12_381_G2_affine_gen_G2[36] = { 0xf5f28fa202940a10, 0xb3f5fb2687b4961a, 0xa1a893b53e2ae580, 0x9894999d1a3caee9, 0x6f67b7631863366b, 0x058191924350bcd7, 0xa5a9c0759e23f606, 0xaaa0c59dbccd60c3, 0x3bb17e18e2867806, 0x1b1ab6cc8541b367, 0xc2b6ed0ef2158547, 0x11922a097360edf3, 0x4c730af860494c4a, 0x597cfa1f5e369c5a, 0xe7e6856caa0a635a, 0xbbefb5e96e0d495f, 0x07d3a975f0ef25a2, 0x0083fd8e7e80dae5, 0xadc0fc92df64b05d, 0x18aa270a2b1461dc, 0x86adac6a3be4eba0, 0x79495c4ec93da33a, 0xe7175850a43ccaed, 0x0b2bc2a163de1bf2, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000 };

// the cofactor of the curve subgroup = 305502333931268344200999753193121504214466019254188142667664032982267604182971884026507427359259977847832272839041616661285803823378372096355777062779109
const uint64_t bls12_381_G2_affine_cofactor[12] = { 0xcf1c38e31c7238e5, 0x1616ec6e786f0c70, 0x21537e293a6691ae, 0xa628f1cb4d9e82ef, 0xa68a205b2e5a7ddf, 0xcd91de4547085aba, 0x091d50792876a202, 0x05d543a95414e7f1, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000 };

// the constants A and B of the equation
const uint64_t bls12_381_G2_affine_const_A[12] = { 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000 };
const uint64_t bls12_381_G2_affine_const_B[12] = { 0xaa270000000cfff3, 0x53cc0032fc34000a, 0x478fe97a6b0a807f, 0xb1d37ebee6ba24d7, 0x8ec9733bbf78ab2f, 0x09d645513d83de7e, 0xaa270000000cfff3, 0x53cc0032fc34000a, 0x478fe97a6b0a807f, 0xb1d37ebee6ba24d7, 0x8ec9733bbf78ab2f, 0x09d645513d83de7e };
const uint64_t bls12_381_G2_affine_const_3B[12] = { 0x447600000027552e, 0xdcb8009a43480020, 0x6f7ee9ce4a6e8b59, 0xb10330b7c0a95bc6, 0x6140b1fcfb1e54b7, 0x0381be097f0bb4e1, 0x447600000027552e, 0xdcb8009a43480020, 0x6f7ee9ce4a6e8b59, 0xb10330b7c0a95bc6, 0x6140b1fcfb1e54b7, 0x0381be097f0bb4e1 };

//------------------------------------------------------------------------------


// checks whether two curve points are equal
uint8_t bls12_381_G2_affine_is_equal( const uint64_t *src1, const uint64_t *src2 ) {
  return ( bls12_381_Fp2_mont_is_equal( X1, X2 ) &&
           bls12_381_Fp2_mont_is_equal( Y1, Y2 ) );
}

// checks whether the underlying representation is the same
uint8_t bls12_381_G2_affine_is_same( const uint64_t *src1, const uint64_t *src2 ) {
  return bls12_381_G2_affine_is_equal( src1, src2 );
}

uint8_t bls12_381_G2_affine_is_infinity ( const uint64_t *src1 ) {
  return ( bls12_381_Fp2_mont_is_zero( X1 ) &&
           bls12_381_Fp2_mont_is_zero( Y1 ) );
}

// infinity is represented by (0,0) for curves with B!=0 (like this one), and 0xffff...ffff for curves with B=0
void bls12_381_G2_affine_set_infinity ( uint64_t *tgt ) {
  bls12_381_Fp2_mont_set_zero( X3 );
  bls12_381_Fp2_mont_set_zero( Y3 );
}

// checks the curve equation
//   y^2 == x^3 + A*x + B
uint8_t bls12_381_G2_affine_is_on_curve ( const uint64_t *src1 ) {
  if (bls12_381_G2_affine_is_infinity(src1)) {
    return 1;
  }
  else {
    uint64_t acc[12];
    uint64_t tmp[12];
    bls12_381_Fp2_mont_sqr( Y1, acc );             // Y^2
    bls12_381_Fp2_mont_neg_inplace( acc );         // -Y^2
    bls12_381_Fp2_mont_sqr( X1, tmp );             // X^2
    bls12_381_Fp2_mont_mul_inplace( tmp, X1 );     // X^3
    bls12_381_Fp2_mont_add_inplace( acc, tmp );    // - Y^2 + X^3
    bls12_381_Fp2_mont_add_inplace( acc, bls12_381_G2_affine_const_B );     // - Y^2*Z + X^3 + A*X + B
    return bls12_381_Fp2_mont_is_zero( acc );
  }
}

// checks whether the given point is in the subgroup G1
uint8_t bls12_381_G2_affine_is_in_subgroup ( const uint64_t *src1 ) {
  if (!bls12_381_G2_affine_is_on_curve(src1)) {
    return 0;
  }
  else {
    if (bls12_381_G2_affine_is_infinity(src1)) {
      return 1;
    }
    else {
      uint64_t proj[36];
      uint64_t tmp [36];
      bls12_381_G2_proj_from_affine( src1, proj );
      bls12_381_G2_proj_scl_Fr_std( bls12_381_G2_affine_cofactor , proj , tmp );
      return bls12_381_G2_proj_is_infinity( tmp );
    }
  }
}

void bls12_381_G2_affine_copy( const uint64_t *src1 , uint64_t *tgt ) {
  if (tgt != src1) { memcpy( tgt, src1, 192 ); }
}

// negates an elliptic curve point in affine coordinates
void bls12_381_G2_affine_neg( const uint64_t *src1, uint64_t *tgt ) {
  if (bls12_381_G2_affine_is_infinity(src1)) {
    memcpy( tgt, src1, 192);
  }
  else {
    memcpy( tgt, src1, 96 );
    bls12_381_Fp2_mont_neg( Y1, Y3 );
  }
}

// negates an elliptic curve point in affine coordinates
void bls12_381_G2_affine_neg_inplace( uint64_t *tgt ) {
  if (bls12_381_G2_affine_is_infinity(tgt)) {
    return;
  }
  else {
    bls12_381_Fp2_mont_neg_inplace( Y3 );
  }
}

// doubles an affine elliptic curve point
void bls12_381_G2_affine_dbl( const uint64_t *src1, uint64_t *tgt ) {
  if (bls12_381_G2_affine_is_infinity(src1)) {
    bls12_381_G2_affine_copy( src1, tgt );
    return;
  }
  else {
    uint64_t  xx[12];
    uint64_t   t[12];
    uint64_t tmp[12];
    bls12_381_Fp2_mont_sqr( X1 , xx );            // xx = X1^2
    bls12_381_Fp2_mont_add( xx , xx, t );         // t  = 2*X1^2
    bls12_381_Fp2_mont_add_inplace( t, xx );      // t  = 3*X1^2
    bls12_381_Fp2_mont_add( Y1, Y1, tmp );             // tmp = 2*Y1
    bls12_381_Fp2_mont_div_inplace( t, tmp );          // t   = (3*X1^2 + A) / (2*Y1)
    bls12_381_Fp2_mont_sqr( t, tmp );                  // tmp = t^2
    bls12_381_Fp2_mont_sub_inplace( tmp, X1 );         // tmp = t^2 - X1
    bls12_381_Fp2_mont_sub_inplace( tmp, X1 );         // tmp = t^2 - 2*X1
    bls12_381_Fp2_mont_sub( tmp, X1 , xx );            // xx =  (t^2 - 2*X1) - X1 = X3 - X1
    bls12_381_Fp2_mont_mul_inplace( xx , t );          // xx = t*(X3 - X1)
    bls12_381_Fp2_mont_add_inplace( xx , Y1);          // xx = Y1 + t*(X3 - X1)
    bls12_381_Fp2_mont_copy( tmp, X3 );                // X3 = t^2 - 2*X1
    bls12_381_Fp2_mont_neg ( xx, Y3 );                 // Y3 = - Y1 - t*(X3 - X1)
  }
}

// doubles an elliptic curve point, in place
void bls12_381_G2_affine_dbl_inplace( uint64_t *tgt ) {
  bls12_381_G2_affine_dbl( tgt , tgt );
}

// adds two affine elliptic curve points
void bls12_381_G2_affine_add( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  if (bls12_381_G2_affine_is_infinity(src1)) {
    // PT1 = infinity
    bls12_381_G2_affine_copy( src2, tgt );
    return;
  }
  if (bls12_381_G2_affine_is_infinity(src2)) {
    // PT2 = infinity
    bls12_381_G2_affine_copy( src1, tgt );
    return;
  }
  else {
    uint64_t xdif[12];
    uint64_t  tmp[12];
    uint64_t    s[12];
    bls12_381_Fp2_mont_sub( X2, X1, xdif );             // xdif = X2 - X1
    if (bls12_381_Fp2_mont_is_zero(xdif)) {
      // X1 == X2
      if (bls12_381_Fp2_mont_is_equal(Y1,Y2)) {
        // Y1 == Y2, it's a doubling
        bls12_381_G2_affine_dbl( src1, tgt );
        return;
      }
      else {
        // Y1 /= Y2, so, it must be Y1 == -Y2, result is the point at infinity
        bls12_381_G2_affine_set_infinity(X3);
      }
    }
    else {
      // normal addition
      bls12_381_Fp2_mont_sub( Y2, Y1, s );             // s   = Y2 - Y1
      bls12_381_Fp2_mont_div_inplace( s, xdif );       // s   = (Y2 - Y1) / (X2 - X1)
      bls12_381_Fp2_mont_sqr( s, tmp );                // tmp = s^2
      bls12_381_Fp2_mont_sub_inplace( tmp, X1 );       // tmp = s^2 - X1
      bls12_381_Fp2_mont_sub_inplace( tmp, X2 );       // tmp = s^2 - X1 - X2 = X3
      bls12_381_Fp2_mont_sub( tmp, X1 , xdif );        // xdif = X3 - X1
      bls12_381_Fp2_mont_mul_inplace( xdif, s );       // xdif = s*(X3 - X1)
      bls12_381_Fp2_mont_add_inplace( xdif, Y1 );      // xdif = Y1 + s*(X3 - X1)
      bls12_381_Fp2_mont_copy( tmp  , X3 );
      bls12_381_Fp2_mont_neg ( xdif , Y3 );
    }
  }
}

void bls12_381_G2_affine_add_inplace( uint64_t *tgt, const uint64_t *src2 ) {
  bls12_381_G2_affine_add( tgt, src2, tgt);
}

void bls12_381_G2_affine_sub( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  uint64_t tmp[24];
  bls12_381_G2_affine_neg( src2, tmp );
  bls12_381_G2_affine_add( src1, tmp, tgt );
}

void bls12_381_G2_affine_sub_inplace( uint64_t *tgt, const uint64_t *src2 ) {
  uint64_t tmp[24];
  bls12_381_G2_affine_neg( src2, tmp );
  bls12_381_G2_affine_add( tgt , tmp, tgt );
}

// computes `expo*grp` (or `grp^expo` in multiplicative notation)
// where `grp` is a group element in G1, and `expo` is in Fr
void bls12_381_G2_affine_scl_generic(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt, int nlimbs) {
  uint64_t proj1[3*NLIMBS_P];
  uint64_t proj2[3*NLIMBS_P];
  bls12_381_G2_proj_from_affine( grp, proj1 );
  bls12_381_G2_proj_scl_generic( expo, proj1, proj2, nlimbs);
  bls12_381_G2_proj_to_affine( proj2, tgt );
}

// computes `expo*grp` (or `grp^expo` in multiplicative notation)
// where `grp` is a group element in G1, and `expo` is in Fr (standard repr)
void bls12_381_G2_affine_scl_Fr_std(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt) {
  uint64_t proj1[3*NLIMBS_P];
  uint64_t proj2[3*NLIMBS_P];
  bls12_381_G2_proj_from_affine( grp, proj1 );
  bls12_381_G2_proj_scl_Fr_std( expo, proj1, proj2);
  bls12_381_G2_proj_to_affine( proj2, tgt );
}

// computes `expo*grp` (or `grp^expo` in multiplicative notation)
// where `grp` is a group element in G1, and `expo` is in Fr (Montgomery repr)
void bls12_381_G2_affine_scl_Fr_mont(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt) {
  uint64_t proj1[3*NLIMBS_P];
  uint64_t proj2[3*NLIMBS_P];
  bls12_381_G2_proj_from_affine( grp, proj1 );
  bls12_381_G2_proj_scl_Fr_mont( expo, proj1, proj2);
  bls12_381_G2_proj_to_affine( proj2, tgt );
}

// computes `expo*grp` (or `grp^expo` in multiplicative notation)
// where `grp` is a group element in G1, and `expo` is the same size as Fp
void bls12_381_G2_affine_scl_big(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt) {
  uint64_t proj1[3*NLIMBS_P];
  uint64_t proj2[3*NLIMBS_P];
  bls12_381_G2_proj_from_affine( grp, proj1 );
  bls12_381_G2_proj_scl_big( expo, proj1, proj2 );
  bls12_381_G2_proj_to_affine( proj2, tgt );
}

// computes `expo*grp` (or `grp^expo` in multiplicative notation)
// where `grp` is a group element in G1, and `expo` is a 64 bit (unsigned!) word
void bls12_381_G2_affine_scl_small(uint64_t expo, const uint64_t *grp, uint64_t *tgt) {
  uint64_t proj1[3*NLIMBS_P];
  uint64_t proj2[3*NLIMBS_P];
  bls12_381_G2_proj_from_affine( grp, proj1 );
  bls12_381_G2_proj_scl_small( expo, proj1, proj2 );
  bls12_381_G2_proj_to_affine( proj2, tgt );
}
