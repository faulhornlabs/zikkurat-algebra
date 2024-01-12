
// elliptic curve "BN128" in projective coordinates, Montgomery field representation
//
// NOTE: generated code, do not edit!

#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <math.h>         // used only for log2()

#include "bn128_G1_proj.h"
#include "bn128_G1_affine.h"
#include "bn128_Fp_mont.h"
#include "bn128_Fr_mont.h"

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
const uint64_t bn128_G1_proj_gen_G1[12] = { 0xd35d438dc58f0d9d, 0x0a78eb28f5c70b3d, 0x666ea36f7879462c, 0x0e0a77c19a07df2f, 0xa6ba871b8b1e1b3a, 0x14f1d651eb8e167b, 0xccdd46def0f28c58, 0x1c14ef83340fbe5e, 0xd35d438dc58f0d9d, 0x0a78eb28f5c70b3d, 0x666ea36f7879462c, 0x0e0a77c19a07df2f };

// the cofactor of the curve subgroup = 1
const uint64_t bn128_G1_proj_cofactor[4] = { 0x0000000000000001, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000 };

//------------------------------------------------------------------------------

// scale a field element by A = 0
void bn128_G1_proj_scale_by_A(const uint64_t *src, uint64_t *tgt ) {
  memset( tgt, 0, 32 );
}

void bn128_G1_proj_scale_by_A_inplace( uint64_t *tgt ) {
  memset( tgt, 0, 32 );
}

// scale a field element by B = 3
void bn128_G1_proj_scale_by_B(const uint64_t *src, uint64_t *tgt ) {
  uint64_t tmp[4];
  bn128_Fp_mont_add( src, src, tmp );
  bn128_Fp_mont_add( src, tmp, tgt );
}

void bn128_G1_proj_scale_by_B_inplace( uint64_t *tgt ) {
  uint64_t tmp[4];
  bn128_Fp_mont_add( tgt, tgt, tmp );
  bn128_Fp_mont_add_inplace( tgt, tmp );
}

// scale a field element by (3*B) = 9
void bn128_G1_proj_scale_by_3B(const uint64_t *src, uint64_t *tgt ) {
  uint64_t tmp[NLIMBS_P];
  bn128_Fp_mont_add( src, src, tmp );       // 2*B
  bn128_Fp_mont_add_inplace( tmp, tmp );    // 4*B
  bn128_Fp_mont_add_inplace( tmp, tmp );    // 8*B
  bn128_Fp_mont_add( src, tmp, tgt );       // 9*B
}

void bn128_G1_proj_scale_by_3B_inplace( uint64_t *tgt ) {
  bn128_G1_proj_scale_by_3B( tgt , tgt );
}

void bn128_G1_proj_normalize( const uint64_t *src1, uint64_t *tgt ) {
  uint64_t zinv[4];
  if (bn128_Fp_mont_is_zero( Z1 ) ) {
    // Z == 0, it must be the point at infinity
    memset( tgt, 0, 96 );
    bn128_Fp_mont_set_one( Y3 );
  }
  else {
    if (bn128_Fp_mont_is_one( Z1 )) {
      // already normalized
      if (tgt != src1) { memcpy( tgt, src1, 96 ); }
    }
    else {
      bn128_Fp_mont_inv( Z1, zinv );
      bn128_Fp_mont_mul( X1, zinv, X3 );
      bn128_Fp_mont_mul( Y1, zinv, Y3 );
      bn128_Fp_mont_set_one( Z3 );
    }
  }
}

void bn128_G1_proj_normalize_inplace( uint64_t *tgt ) {
  bn128_G1_proj_normalize( tgt, tgt );
}

// checks whether the underlying representation (projective coordinates) are the same
uint8_t bn128_G1_proj_is_same( const uint64_t *src1, const uint64_t *src2 ) {
  return ( bn128_Fp_mont_is_equal( X1, X2 ) &&
           bn128_Fp_mont_is_equal( Y1, Y2 ) &&
           bn128_Fp_mont_is_equal( Z1, Z2 ) );
}

// checks whether two curve points are equal
uint8_t bn128_G1_proj_is_equal( const uint64_t *src1, const uint64_t *src2 ) {
  uint64_t tmp1[12];
  uint64_t tmp2[12];
  bn128_G1_proj_normalize( src1, tmp1 );
  bn128_G1_proj_normalize( src2, tmp2 );
  return bn128_G1_proj_is_same( tmp1, tmp2 );
}

// converts from affine coordinates
void bn128_G1_proj_from_affine( const uint64_t *src1 , uint64_t *tgt ) {
  memcpy( tgt, src1, 64 );
  if (bn128_G1_affine_is_infinity( src1 )) {
    bn128_G1_proj_set_infinity( tgt );
  }
  else {
    bn128_Fp_mont_set_one( Z3 );
  }
}

// converts to affine coordinates
// remark: the point at infinity will result in the special string `0xffff...ffff`
void bn128_G1_proj_to_affine( const uint64_t *src1 , uint64_t *tgt ) {
  if (bn128_Fp_mont_is_zero( Z1 )) {
    // in the affine coordinate system, the point at infinity is represented by a hack
    // consisting all 0xff bytes (note that that's an invalid value for prime fields)
    memset( tgt, 0xff, 64 );
  }
  else {
    uint64_t zinv[4];
    bn128_Fp_mont_inv( Z1, zinv );
    bn128_Fp_mont_mul( X1, zinv, X3 );
    bn128_Fp_mont_mul( Y1, zinv, Y3 );
  }
}

void bn128_G1_proj_copy( const uint64_t *src1 , uint64_t *tgt ) {
  if (tgt != src1) { memcpy( tgt, src1, 96 ); }
}

uint8_t bn128_G1_proj_is_infinity ( const uint64_t *src1 ) {
  return ( ( bn128_Fp_mont_is_zero( Z1 )) &&
           (!bn128_Fp_mont_is_zero( Y1 )) &&
           ( bn128_Fp_mont_is_zero( X1 )) );
}

void bn128_G1_proj_set_infinity ( uint64_t *tgt ) {
  bn128_Fp_mont_set_zero( X3 );
  bn128_Fp_mont_set_one ( Y3 );
  bn128_Fp_mont_set_zero( Z3 );
}

// checks the curve equation
//   y^2*z == x^3 + A*x*z^2 + B*z^3
uint8_t bn128_G1_proj_is_on_curve ( const uint64_t *src1 ) {
  uint64_t  ZZ[4];
  uint64_t acc[4];
  uint64_t tmp[4];
  bn128_Fp_mont_sqr( Y1, acc );             // Y^2
  bn128_Fp_mont_mul_inplace( acc, Z1 );     // Y^2*Z
  bn128_Fp_mont_neg_inplace( acc );         // -Y^2*Z
  bn128_Fp_mont_sqr( X1, tmp );             // X^2
  bn128_Fp_mont_mul_inplace( tmp, X1 );     // X^3
  bn128_Fp_mont_add_inplace( acc, tmp );    // - Y^2*Z + X^3
  bn128_Fp_mont_sqr( Z1, ZZ );              // Z^2
  bn128_Fp_mont_mul( Z1, ZZ, tmp );          // Z^3
  bn128_G1_proj_scale_by_B_inplace( tmp );   // B*Z^3
  bn128_Fp_mont_add_inplace( acc, tmp );     // - Y^2*Z + X^3 + A*X*Z^2 + B*Z^3
  return (bn128_Fp_mont_is_zero( acc ) &&
           ( (!bn128_Fp_mont_is_zero( Z1 )) || 
             (!bn128_Fp_mont_is_zero( Y1 )) ) );
}

// checks whether the given point is in the subgroup G1
uint8_t bn128_G1_proj_is_in_subgroup ( const uint64_t *src1 ) {
  uint64_t tmp[12];
  if (!bn128_G1_proj_is_on_curve(src1)) {
    return 0;
  }
  else {
    bn128_G1_proj_scl_Fr_std( bn128_G1_proj_cofactor , src1 , tmp );
    return bn128_G1_proj_is_infinity( tmp );
  }
}

// negates an elliptic curve point
void bn128_G1_proj_neg( const uint64_t *src, uint64_t *tgt ) {
  if (tgt != src) { memcpy( tgt, src, 96 ); }
  bn128_Fp_mont_neg_inplace( Y3 );
}

// negates an elliptic curve point
void bn128_G1_proj_neg_inplace( uint64_t *tgt ) {
  bn128_Fp_mont_neg_inplace( Y3 );
}

// doubles an elliptic curve point
// https://hyperelliptic.org/EFD/g1p/auto-shortw-projective.html#doubling-dbl-2007-bl
void bn128_G1_proj_dbl( const uint64_t *src1, uint64_t *tgt ) {
  uint64_t  XX[4];
  uint64_t  ZZ[4];
  uint64_t   w[4];
  uint64_t   s[4];
  uint64_t  ss[4];
  uint64_t sss[4];
  uint64_t   R[4];
  uint64_t  RR[4];
  uint64_t   B[4];
  uint64_t   h[4];
  bn128_Fp_mont_sqr( X1 , XX );         // XX = X1^2
  bn128_Fp_mont_sqr( Z1 , ZZ );         // ZZ = Z1^2
  bn128_Fp_mont_add( XX, XX, w );       // w  = 2*XX
  bn128_Fp_mont_add_inplace( w, XX );   // w  = 3*XX
  bn128_Fp_mont_mul( Y1 , Z1 , s );         // s   = Y1*Z1
  bn128_Fp_mont_add_inplace( s, s );        // s   = 2*Y1*Z1
  bn128_Fp_mont_sqr( s , ss );              // ss  = s^2
  bn128_Fp_mont_mul( Y1 , s , R);           // R   = Y1*s
  bn128_Fp_mont_sqr( R , RR );              // RR  = R^2
  bn128_Fp_mont_add( X1 , R , B);           // B   = (X1+R)
  bn128_Fp_mont_sqr_inplace( B );           // B   = (X1+R)^2
  bn128_Fp_mont_sub_inplace( B , XX );      // B   = (X1+R)^2 - XX
  bn128_Fp_mont_sub_inplace( B , RR );      // B   = (X1+R)^2 - XX - RR
  bn128_Fp_mont_sqr( w , h );               // h   = w^2
  bn128_Fp_mont_sub_inplace( h , B );       // h   = w^2 - B
  bn128_Fp_mont_sub_inplace( h , B );       // h   = w^2 - 2*B
  bn128_Fp_mont_mul( s , ss , Z3 );         // Z3  = s^3
  bn128_Fp_mont_mul( h , s , X3 );          // X3  = h*s
  bn128_Fp_mont_sub( B , h , Y3 );          // Y3  = B-h
  bn128_Fp_mont_mul_inplace( Y3 , w  );     // Y3  = w*(B-h)
  bn128_Fp_mont_sub_inplace( Y3 , RR );     // Y3  = w*(B-h) - RR
  bn128_Fp_mont_sub_inplace( Y3 , RR );     // Y3  = w*(B-h) - 2*RR
}

// doubles an elliptic curve point
void bn128_G1_proj_dbl_inplace( uint64_t *tgt ) {
  bn128_G1_proj_dbl( tgt , tgt );
}

// adds two elliptic curve points, assuming A = 0
// https://hyperelliptic.org/EFD/g1p/auto-shortw-projective.html#addition-add-2015-rcb
void bn128_G1_proj_add( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  uint64_t t0[4];
  uint64_t t1[4];
  uint64_t t2[4];
  uint64_t t3[4];
  uint64_t t4[4];
  uint64_t t5[4];
  bn128_Fp_mont_mul( X1, X2, t0 );              // t0 = X1*X2
  bn128_Fp_mont_mul( Y1, Y2, t1 );              // t1 = Y1*Y2
  bn128_Fp_mont_mul( Z1, Z2, t2 );              // t2 = Z1*Z2
  bn128_Fp_mont_add( X1, Y1, t3 );              // t3 = X1+Y1
  bn128_Fp_mont_add( X2, Y2, t4 );              // t4 = X2+Y2
  bn128_Fp_mont_mul_inplace( t3, t4 );          // t3 = t3*t4
  bn128_Fp_mont_add( t0, t1, t4 );              // t4 = t0+t1
  bn128_Fp_mont_sub_inplace( t3 , t4 );         // t3 = t3-t4
  bn128_Fp_mont_add( X1, Z1, t4 );              // t4 = X1+Z1
  bn128_Fp_mont_add( X2, Z2, t5 );              // t5 = X2+Z2
  bn128_Fp_mont_mul_inplace( t4, t5 );          // t4 = t4*t5
  bn128_Fp_mont_add( t0, t2, t5 );              // t5 = t0+t2
  bn128_Fp_mont_sub_inplace( t4, t5 );          // t4 = t4-t5
  bn128_Fp_mont_add( Y1, Z1, t5 );              // t5 = Y1+Z1
  bn128_Fp_mont_add( Y2, Z2, X3 );              // X3 = Y2+Z2
  bn128_Fp_mont_mul_inplace( t5, X3 );          // t5 = t5*X3
  bn128_Fp_mont_add( t1, t2, X3 );              // X3 = t1+t2
  bn128_Fp_mont_sub_inplace( t5, X3 );          // t5 = t5-X3
  bn128_G1_proj_scale_by_3B( t2, X3 );          // X3 = b3*t2
  bn128_Fp_mont_copy( X3, Z3 );                 // Z3 = X3
  bn128_Fp_mont_sub( t1, Z3, X3 );              // X3 = t1-Z3
  bn128_Fp_mont_add_inplace( Z3, t1 );          // Z3 = t1+Z3
  bn128_Fp_mont_mul( X3, Z3, Y3 );              // Y3 = X3*Z3
  bn128_Fp_mont_add( t0, t0, t1 );              // t1 = t0+t0
  bn128_Fp_mont_add_inplace( t1, t0 );          // t1 = t1+t0
  bn128_G1_proj_scale_by_3B_inplace( t4 );      // t4 = b3*t4
  bn128_Fp_mont_mul( t1, t4, t0 );              // t0 = t1*t4
  bn128_Fp_mont_add_inplace( Y3, t0 );          // Y3 = Y3+t0
  bn128_Fp_mont_mul( t4, t5, t0 );              // t0 = t5*t4
  bn128_Fp_mont_mul_inplace( X3, t3 );          // X3 = t3*X3
  bn128_Fp_mont_sub_inplace( X3, t0 );          // X3 = X3-t0
  bn128_Fp_mont_mul( t1, t3, t0 );              // t0 = t3*t1
  bn128_Fp_mont_mul_inplace( Z3, t5 );          // Z3 = t5*Z3
  bn128_Fp_mont_add_inplace( Z3, t0 );          // Z3 = Z3+t0
}

void bn128_G1_proj_add_inplace( uint64_t *tgt, const uint64_t *src2 ) {
  bn128_G1_proj_add( tgt, src2, tgt);
}

void bn128_G1_proj_sub( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  uint64_t tmp[12];
  bn128_G1_proj_neg( src2, tmp );
  bn128_G1_proj_add( src1, tmp, tgt );
}

void bn128_G1_proj_sub_inplace( uint64_t *tgt, const uint64_t *src2 ) {
  uint64_t tmp[12];
  bn128_G1_proj_neg( src2, tmp );
  bn128_G1_proj_add( tgt , tmp, tgt );
}

// adds a projective point (src1) to an affine point (src2)
// https://hyperelliptic.org/EFD/g1p/auto-shortw-projective.html#addition-madd-1998-cmo
void bn128_G1_proj_madd_proj_aff( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  if (bn128_G1_proj_is_infinity( src1 )) {
    // the formula is not valid for this case
    bn128_G1_proj_from_affine( src2 , tgt );
    return;
  }
  if (bn128_G1_affine_is_infinity( src2 )) {
    bn128_G1_proj_copy( src1 , tgt );
    return;
  }
  uint64_t   u[4];
  uint64_t  uu[4];
  uint64_t   v[4];
  uint64_t  vv[4];
  uint64_t vvv[4];
  uint64_t   R[4];
  uint64_t   A[4];
  bn128_Fp_mont_mul( Y2, Z1, u );              //  u = Y2*Z1     
  bn128_Fp_mont_sub_inplace( u , Y1 );         //  u = Y2*Z1-Y1  
  bn128_Fp_mont_mul( X2, Z1, v );              //  v = X2*Z1     
  bn128_Fp_mont_sub_inplace( v , X1 );         //  v = X2*Z1-X1  
  if (bn128_Fp_mont_is_zero(u) && bn128_Fp_mont_is_zero(v) ) {
    // it's doubling, the naive result would be (0,0,0)
    bn128_G1_proj_dbl( src1 , tgt );
    return;
  }
  bn128_Fp_mont_sqr( u , uu );                 //  uu = u2       
  bn128_Fp_mont_sqr( v, vv );                  //  vv = v2       
  bn128_Fp_mont_mul( v, vv, vvv );             //  vvv = v*vv    
  bn128_Fp_mont_mul( vv, X1, R );              //  R = vv*X1     
  bn128_Fp_mont_mul( uu, Z1, A );              //  A = uu*Z1     
  bn128_Fp_mont_sub_inplace( A, vvv );         //  A = uu*Z1-vvv 
  bn128_Fp_mont_sub_inplace( A, R );           //  A = uu*Z1-vvv-2  
  bn128_Fp_mont_sub_inplace( A, R );           //  A = uu*Z1-vvv-2*R
  bn128_Fp_mont_mul( v, A, X3 );               //  X3 = v*A      
  bn128_Fp_mont_mul( Z1, vvv, Z3 );            //  Z3 = vvv*Z1   
  bn128_Fp_mont_sub_inplace( R , A );          //  R' =  R-A     
  bn128_Fp_mont_mul_inplace( vvv , Y1 );       //  vvv' = vvv*Y1 
  bn128_Fp_mont_mul( u , R , Y3 );             //  Y3 = u*(R-A)  
  bn128_Fp_mont_sub_inplace( Y3, vvv );        //  Y3 = u*(R-A)-vvv*Y1
}

// adds an affine point (src1) to a projective one (src2)
// https://hyperelliptic.org/EFD/g1p/auto-shortw-projective.html#addition-add-2015-rcb
void bn128_G1_proj_madd_aff_proj( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt ) {
  bn128_G1_proj_madd_proj_aff( src2, src1, tgt );
}

// adds to a projective point (tgt) an affine point (src2), in place
void bn128_G1_proj_madd_inplace( uint64_t *tgt, const uint64_t *src2 ) {
  bn128_G1_proj_madd_proj_aff( tgt, src2, tgt );
}

// computes `expo*grp` (or `grp^expo` in multiplicative notation)
// where `grp` is a group element in G1, and `expo` is a (non-negative) bigint
// naive algorithm
void bn128_G1_proj_scl_naive(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt, int expo_len) {

  uint64_t dbl[3*NLIMBS_P];
  bn128_G1_proj_copy( grp, dbl );              // dbl := grp
  bn128_G1_proj_set_infinity( tgt );           // tgt := infinity

  int s = expo_len - 1;
  while( (s>0) && (expo[s] == 0) ) { s--; }      // skip the unneeded largest powers

  for(int i=0; i<=s; i++) {
    uint64_t e = expo[i];
    for(int j=0; j<64; j++) {
      if (e & 1) { 
        bn128_G1_proj_add( tgt, dbl, tgt ); 
      }
      bn128_G1_proj_dbl( dbl, dbl );
      e = e >> 1;
    }
  }
}

#define TBL(k) (table + (k-1)*3*NLIMBS_P)

// precalculate [ k*g | k <- [1..15] ]
void bn128_G1_proj_precalc_expos_window_16( const uint64_t *grp, uint64_t *table ) {
  bn128_G1_proj_copy( grp              , TBL( 1) );           //  1*g
  bn128_G1_proj_dbl ( TBL(1)           , TBL( 2) );           //  2*g
  bn128_G1_proj_dbl ( TBL(2)           , TBL( 4) );           //  4*g
  bn128_G1_proj_dbl ( TBL(4)           , TBL( 8) );           //  8*g
  bn128_G1_proj_add ( TBL(1) , TBL( 2) , TBL( 3) );           //  3*g
  bn128_G1_proj_dbl ( TBL(3) ,           TBL( 6) );           //  6*g
  bn128_G1_proj_dbl ( TBL(6) ,           TBL(12) );           // 12*g
  bn128_G1_proj_add ( TBL(1) , TBL( 4) , TBL( 5) );           //  5*g
  bn128_G1_proj_dbl ( TBL(5) ,           TBL(10) );           // 10*g
  bn128_G1_proj_add ( TBL(1) , TBL( 6) , TBL( 7) );           //  7*g
  bn128_G1_proj_dbl ( TBL(7) ,           TBL(14) );           // 14*g
  bn128_G1_proj_add ( TBL(1) , TBL( 8) , TBL( 9) );           //  9*g
  bn128_G1_proj_add ( TBL(1) , TBL(10) , TBL(11) );           // 11*g
  bn128_G1_proj_add ( TBL(1) , TBL(12) , TBL(13) );           // 13*g
  bn128_G1_proj_add ( TBL(1) , TBL(14) , TBL(15) );           // 15*g
}

// computes `expo*grp` (or `grp^expo` in multiplicative notation)
// where `grp` is a group element in G1, and `expo` is a (non-negative) bigint
// generic windowed algo, 4-bit windows
void bn128_G1_proj_scl_windowed(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt, int expo_len) {

  // precalculate [ k*g | k <- [1..15] ]
  uint64_t table[15*3*NLIMBS_P];
  bn128_G1_proj_precalc_expos_window_16( grp, table );

  bn128_G1_proj_set_infinity( tgt );           // tgt := infinity

  int s = expo_len - 1;
  while( (s>0) && (expo[s] == 0) ) { s--; }      // skip the unneeded largest powers

  for(int i=s; i>=0; i--) {
    uint64_t e = expo[i];
    for(int j=0; j<16; j++) {
      // we can skip doubling when infinity
      if (!bn128_Fp_mont_is_zero(tgt+2*NLIMBS_P)) {
        bn128_G1_proj_dbl_inplace( tgt );
        bn128_G1_proj_dbl_inplace( tgt );
        bn128_G1_proj_dbl_inplace( tgt );
        bn128_G1_proj_dbl_inplace( tgt );
      }
      int k = (e >> 60);
      if (k) { 
        bn128_G1_proj_add_inplace( tgt, TBL(k) ); 
      }
      e = e << 4;
    }
  }
}

// computes `expo*grp` (or `grp^expo` in multiplicative notation)
// where `grp` is a group element in G1, and `expo` is in Fr
void bn128_G1_proj_scl_generic(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt, int nlimbs) {
  bn128_G1_proj_scl_windowed(expo, grp, tgt, nlimbs);
}

// computes `expo*grp` (or `grp^expo` in multiplicative notation)
// where `grp` is a group element in G1, and `expo` is in Fr *in standard repr*
void bn128_G1_proj_scl_Fr_std(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt) {
  bn128_G1_proj_scl_generic(expo, grp, tgt, NLIMBS_R);
}

// computes `expo*grp` (or `grp^expo` in multiplicative notation)
// where `grp` is a group element in G1, and `expo` is in Fr *in Montgomery repr*
void bn128_G1_proj_scl_Fr_mont(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt) {
  uint64_t expo_std[NLIMBS_R];
  bn128_Fr_mont_to_std(expo, expo_std);
  bn128_G1_proj_scl_generic(expo_std, grp, tgt, NLIMBS_R);
}

// computes `expo*grp` (or `grp^expo` in multiplicative notation)
// where `grp` is a group element in G1, and `expo` is the same size as Fp
void bn128_G1_proj_scl_big(const uint64_t *expo, const uint64_t *grp, uint64_t *tgt) {
  bn128_G1_proj_scl_generic(expo, grp, tgt, NLIMBS_P);
}

// computes `expo*grp` (or `grp^expo` in multiplicative notation)
// where `grp` is a group element in G1, and `expo` is a 64 bit (unsigned!) word
void bn128_G1_proj_scl_small(uint64_t expo, const uint64_t *grp, uint64_t *tgt) {
  uint64_t expo_vec[1];
  expo_vec[0] = expo;
  bn128_G1_proj_scl_generic(expo_vec, grp, tgt, 1);
}

//------------------------------------------------------------------------------

#define SIDX(b) (SUMS + (b-1)*(3*NLIMBS_P))

// Multi-Scalar Multiplication (MSM)
// standard coefficients (NOT montgomery!)
// straightforward Pippenger bucketing method
// parametric bucket size
void bn128_G1_proj_MSM_std_coeff_proj_out_variable(int npoints, const uint64_t *expos, const uint64_t *grps, uint64_t *tgt, int expo_nlimbs, int window_size) {

  assert( (window_size > 0) && (window_size <= 64) );

  int nwindows = (64*expo_nlimbs + window_size - 1) / window_size;
  int nbuckets = (1 << window_size);

  bn128_G1_proj_set_infinity(tgt);

  // allocate memory for bucket sums
  uint64_t *SUMS = malloc( 3*8*NLIMBS_P * (nbuckets-1) );
  assert( SUMS !=0 );

  // loop over the windows
  for(int K=nwindows-1; K >= 0; K-- ) {

    // K-th window
    int A = K*window_size;
    int B = A + window_size;
    if (B > 64*expo_nlimbs ) { B = 64*expo_nlimbs; }

    uint64_t mask = (1<<(B-A)) - 1;

    int Adiv = (A >> 6);    // A / 64
    int Amod = (A & 0x3f);  // A mod 64

    int Bdiv = Adiv;
    int Bshl = 0;
    if (((B-1)>>6) != Adiv) { 
      // the window intersects qword boundary...
      Bdiv = Adiv + 1; 
      Bshl = 64*Bdiv - A; 
    }

    // we could do this in constant memory, but then would have 
    // to we go over the points way many (=bucket_size) times...

    // initalize bucket sums
    for( int b=nbuckets-1; b>0; b-- ) { 
      bn128_G1_proj_set_infinity( SIDX(b) );
    }

    // compute bucket sums
    for(int j=0; j<npoints; j++) {

      int ofs = expo_nlimbs*j + Adiv;
      uint64_t e = (expos[ofs] >> Amod);
      if (Bdiv != Adiv) {
        e |= (expos[ofs+1] << Bshl);
      }
      e &= mask;   // bucket coeff

      if (e>0) {
        bn128_G1_proj_madd_proj_aff( SIDX(e) , grps + (2*NLIMBS_P*j) , SIDX(e) );
      }
    }

    // compute running sums

    uint64_t T[3*NLIMBS_P];   // cumulative sum of S-es
    uint64_t R[3*NLIMBS_P];   // running sum = sum of T-s

    bn128_G1_proj_set_infinity(T);
    bn128_G1_proj_set_infinity(R);

    for( int b=nbuckets-1; b>0; b-- ) { 
      bn128_G1_proj_add_inplace( T , SIDX(b) );
      bn128_G1_proj_add_inplace( R , T       );
    }

    if (!bn128_G1_proj_is_infinity(tgt)) {    // we can skip doubling when infinity
      for(int i=0; i<window_size; i++) {
        bn128_G1_proj_dbl_inplace(tgt);
      }
    }

    bn128_G1_proj_add_inplace( tgt, R );
  }

  free(SUMS);
}

//------------------------------------------------------------------------------

// Multi-Scalar Multiplication (MSM)
// inputs: 
//  - standard coefficients (1 field element per point)
//  - affine Montgomery points (2 field elements per point)
// output:
//  - weighted projective Montgomery point
void bn128_G1_proj_MSM_std_coeff_proj_out(int npoints, const uint64_t *expos, const uint64_t *grps, uint64_t *tgt, int expo_nlimbs) {

  // guess optimal window size
  int c = round( log2(npoints) - 3.5 );
  if (c < 1 ) { c = 1;  }
  if (c > 64) { c = 64; }

  bn128_G1_proj_MSM_std_coeff_proj_out_variable(npoints, expos, grps, tgt, expo_nlimbs, c);  
}

//------------------------------------------------------------------------------

// reference (slow) implementation of MSM
// for testing purposes
void bn128_G1_proj_MSM_std_coeff_proj_out_slow_reference(int npoints, const uint64_t *expos, const uint64_t *grps, uint64_t *tgt, int expo_nlimbs) {
  uint64_t grp[3*NLIMBS_P];
  uint64_t tmp[3*NLIMBS_P];
  bn128_G1_proj_set_infinity( tgt );
  for(int i=0; i<npoints; i++) { 
    bn128_G1_proj_from_affine( grps  + i*2*NLIMBS_P , grp );                      // convert to proj coords
    bn128_G1_proj_scl_generic( expos + i*expo_nlimbs , grp , tmp , expo_nlimbs );     // exponentiate
    bn128_G1_proj_add_inplace( tgt , tmp );                                     // add to the running sum
  }
}

//------------------------------------------------------------------------------

// Multi-Scalar Multiplication (MSM)
// inputs: 
//  - Montgomery coefficients (1 field element per point)
//  - affine Montgomery points (2 field elements per point)
// output:
//  - weighted projective Montgomery point
void bn128_G1_proj_MSM_mont_coeff_proj_out(int npoints, const uint64_t *expos, const uint64_t *grps, uint64_t *tgt, int expo_nlimbs) {
  uint64_t *std_expos = malloc(8*expo_nlimbs*npoints);
  assert( std_expos != 0);
  const uint64_t *p;
  uint64_t *q;
  p = expos;
  q = std_expos;
  for(int i=0; i<npoints; i++) {
    bn128_Fr_mont_to_std( p , q );
    p += expo_nlimbs;
    q += expo_nlimbs;
  }
  bn128_G1_proj_MSM_std_coeff_proj_out(npoints, std_expos, grps, tgt, expo_nlimbs);
  free(std_expos);
}

//------------------------------------------------------------------------------

// Multi-Scalar Multiplication (MSM)
// inputs: 
//  - standard coefficients (1 field element per point)
//  - affine Montgomery points (2 field elements per point)
// output:
//  - affine Montgomery point
void bn128_G1_proj_MSM_std_coeff_affine_out(int npoints, const uint64_t *expos, const uint64_t *grps, uint64_t *tgt, int expo_nlimbs) {
  uint64_t tmp[3*NLIMBS_P];
  bn128_G1_proj_MSM_std_coeff_proj_out(npoints, expos, grps, tmp, expo_nlimbs);
  bn128_G1_proj_to_affine(tmp, tgt);
}

// Multi-Scalar Multiplication (MSM)
// inputs: 
//  - Montgomery coefficients (1 field element per point)
//  - affine Montgomery points (2 field elements per point)
// output:
//  - affine Montgomery point
void bn128_G1_proj_MSM_mont_coeff_affine_out(int npoints, const uint64_t *expos, const uint64_t *grps, uint64_t *tgt, int expo_nlimbs) {
  uint64_t tmp[3*NLIMBS_P];
  bn128_G1_proj_MSM_mont_coeff_proj_out(npoints, expos, grps, tmp, expo_nlimbs);
  bn128_G1_proj_to_affine(tmp, tgt);
}

//------------------------------------------------------------------------------


#define GRP_NLIMBS (3*NLIMBS_P)

// -----------------------------------------------------------------------------

void bn128_G1_proj_fft_forward_noalloc( int m, int src_stride, const uint64_t *gen, const uint64_t *src, uint64_t *buf, uint64_t *tgt) {

  if (m==0) {
    bn128_G1_proj_copy( src, tgt );
    return;
  }

  if (m==1) {
    // N = 2
    bn128_G1_proj_add( src , src + src_stride*GRP_NLIMBS , tgt              );    // x + y
    bn128_G1_proj_sub( src , src + src_stride*GRP_NLIMBS , tgt + GRP_NLIMBS );    // x - y
    return;
  }

  else {
  
    int N     = (1<< m   );
    int halfN = (1<<(m-1));

    uint64_t gpow[NLIMBS_R];
    bn128_Fr_mont_sqr( gen, gpow );  // gen^2
    
    bn128_G1_proj_fft_forward_noalloc( m-1 , src_stride<<1 , gpow , src                         , buf + N*GRP_NLIMBS , buf                    );
    bn128_G1_proj_fft_forward_noalloc( m-1 , src_stride<<1 , gpow , src + src_stride*GRP_NLIMBS , buf + N*GRP_NLIMBS , buf + halfN*GRP_NLIMBS );

    bn128_Fr_mont_set_one(gpow);
    for(int j=0; j<halfN; j++) {
      bn128_G1_proj_scl_Fr_mont(gpow , buf + (j+halfN)*GRP_NLIMBS  , tgt +  j *GRP_NLIMBS );  //   g*v[k]
      bn128_G1_proj_neg ( tgt +  j       *GRP_NLIMBS ,        tgt + (j+halfN)*GRP_NLIMBS );   // - g*v[k]
      bn128_G1_proj_add_inplace( tgt +  j       *GRP_NLIMBS , buf + j*GRP_NLIMBS );           // u[k] + g*v[k]
      bn128_G1_proj_add_inplace( tgt + (j+halfN)*GRP_NLIMBS , buf + j*GRP_NLIMBS );           // u[k] - g*v[k]
      bn128_Fr_mont_mul_inplace( gpow , gen );      
    }
  }
}

// forward FFT of group elements (convert from [L_k(tau)] to [tau^i])
// `src` and `tgt` should be `N = 2^m` sized arrays of group elements
// `gen` should be the generator of the multiplicative subgroup (of the scalar field) sized `N` (in _Montgomery_ representation)
// NOTE: we normalize the results
void bn128_G1_proj_fft_forward (int m, const uint64_t *gen, const uint64_t *src, uint64_t *tgt) {
  int N = (1<<m);
  uint64_t *buf = malloc( 8*GRP_NLIMBS * (2*N) );
  assert( buf !=0 );
  bn128_G1_proj_fft_forward_noalloc( m, 1, gen, src, buf, tgt);
  free(buf);
  for(int i=0; i<N; i++) { 
    bn128_G1_proj_normalize_inplace( tgt + i*GRP_NLIMBS );
  }
}


// -----------------------------------------------------------------------------

// inverse of 2 (standard repr)
const uint64_t bn128_G1_proj_oneHalf[4] = { 0x783c14d81ffffffe, 0xaf982f6f0c8d1edd, 0x8f5f7492fcfd4f45, 0x1f37631a3d9cbfac };

void bn128_G1_proj_fft_inverse_noalloc(int m, int tgt_stride, const uint64_t *gen, const uint64_t *src, uint64_t *buf, uint64_t *tgt) {

  if (m==0) {
    bn128_G1_proj_copy( src, tgt );
    return;
  }

  if (m==1) {
    // N = 2
    bn128_G1_proj_add( src , src + GRP_NLIMBS , tgt                         );   // x + y
    bn128_G1_proj_sub( src , src + GRP_NLIMBS , tgt + tgt_stride*GRP_NLIMBS );   // x - y
    bn128_G1_proj_scl_Fr_mont( bn128_G1_proj_oneHalf , tgt                         , tgt                         );      // (x + y)/2
    bn128_G1_proj_scl_Fr_mont( bn128_G1_proj_oneHalf , tgt + tgt_stride*GRP_NLIMBS , tgt + tgt_stride*GRP_NLIMBS );      // (x - y)/2
    return;
  }

  else {
  
    int N     = (1<< m   );
    int halfN = (1<<(m-1));

    uint64_t ginv[NLIMBS_R];
    bn128_Fr_mont_inv( gen , ginv );  // gen^-1

    uint64_t gpow[NLIMBS_R];    
    bn128_Fr_mont_copy(bn128_G1_proj_oneHalf , gpow);  // 1/2
    for(int j=0; j<halfN; j++) {
      bn128_G1_proj_add( src +  j* GRP_NLIMBS , src + (j+halfN)*GRP_NLIMBS , buf + j        *GRP_NLIMBS  );    // x + y
      bn128_G1_proj_sub( src +  j* GRP_NLIMBS , src + (j+halfN)*GRP_NLIMBS , buf + (j+halfN)*GRP_NLIMBS  );    // x - y
      bn128_G1_proj_scl_Fr_mont( bn128_G1_proj_oneHalf , buf + j        *GRP_NLIMBS , buf + j        *GRP_NLIMBS );    // (x + y) /  2
      bn128_G1_proj_scl_Fr_mont( gpow                    , buf + (j+halfN)*GRP_NLIMBS , buf + (j+halfN)*GRP_NLIMBS );    // (x - y) / (2*g^k)
      bn128_Fr_mont_mul_inplace( gpow , ginv );      
    }

    bn128_Fr_mont_sqr( gen, gpow );  // gen^2
    bn128_G1_proj_fft_inverse_noalloc( m-1 , tgt_stride<<1 , gpow , buf                    , buf + N*GRP_NLIMBS , tgt                         );
    bn128_G1_proj_fft_inverse_noalloc( m-1 , tgt_stride<<1 , gpow , buf + halfN*GRP_NLIMBS , buf + N*GRP_NLIMBS , tgt + tgt_stride*GRP_NLIMBS );

  }
}
 
// inverse FFT of group elements (convert from [tau^i] to [L_k(tau)]
// `src` and `tgt` should be `N = 2^m` sized arrays of group elements
// `gen` should be the generator of the multiplicative subgroup (of the scalar field) sized `N`, in _Montgomery_ representation
// NOTE: we normalize the results
void bn128_G1_proj_fft_inverse(int m, const uint64_t *gen, const uint64_t *src, uint64_t *tgt) {
  int N = (1<<m);
  uint64_t *buf = malloc( 8*GRP_NLIMBS * (2*N) );
  assert( buf !=0 );
  bn128_G1_proj_fft_inverse_noalloc( m, 1, gen, src, buf, tgt );
  free(buf);
  for(int i=0; i<N; i++) { 
    bn128_G1_proj_normalize_inplace( tgt + i*GRP_NLIMBS );
  }
}

// -----------------------------------------------------------------------------

