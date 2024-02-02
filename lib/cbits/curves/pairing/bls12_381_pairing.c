#include "stdint.h"
#include "stdlib.h"
#include "string.h"
#include "assert.h"

#include "curves/fields/mont/bls12_381_Fp_mont.h"
#include "curves/fields/mont/bls12_381_Fp2_mont.h"
#include "curves/fields/mont/bls12_381_Fp12_mont.h"

#include "curves/g1/affine/bls12_381_G1_affine.h"
#include "curves/g2/affine/bls12_381_G2_affine.h"
#include "curves/g1/proj/bls12_381_G1_proj.h"
#include "curves/g2/proj/bls12_381_G2_proj.h"

//------------------------------------------------------------------------------

#define NWORDS_FP   6
#define NWORDS_FP2  12
#define NWORDS_FP12 72

//------------------------------------------------------------------------------

const uint64_t bls12_381_pairing_twist_B [12] = { 0xaa270000000cfff3, 0x53cc0032fc34000a, 0x478fe97a6b0a807f, 0xb1d37ebee6ba24d7, 0x8ec9733bbf78ab2f, 0x09d645513d83de7e, 0xaa270000000cfff3, 0x53cc0032fc34000a, 0x478fe97a6b0a807f, 0xb1d37ebee6ba24d7, 0x8ec9733bbf78ab2f, 0x09d645513d83de7e };
const uint64_t bls12_381_pairing_twist_3B[12] = { 0x447600000027552e, 0xdcb8009a43480020, 0x6f7ee9ce4a6e8b59, 0xb10330b7c0a95bc6, 0x6140b1fcfb1e54b7, 0x0381be097f0bb4e1, 0x447600000027552e, 0xdcb8009a43480020, 0x6f7ee9ce4a6e8b59, 0xb10330b7c0a95bc6, 0x6140b1fcfb1e54b7, 0x0381be097f0bb4e1 };

#define MILLER_LOOP_LENGTH 63

const uint64_t bls12_381_miller_loop_param = 0xd201000000010000;

const uint64_t bls12_381_pairing_fp12_w2[72] = 
  { 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x760900000002fffd, 0xebf4000bc40c0002, 0x5f48985753c758ba, 0x77ce585370525745, 0x5c071a97a256ec6d, 0x15f65ec3fa80e493, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000 };
const uint64_t bls12_381_pairing_fp12_w3[72] = 
  { 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x760900000002fffd, 0xebf4000bc40c0002, 0x5f48985753c758ba, 0x77ce585370525745, 0x5c071a97a256ec6d, 0x15f65ec3fa80e493, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000 };

const uint64_t bls12_381_pairing_fp12_inv_w2[72] = 
  { 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x1804000000015554, 0x855000053ab00001, 0x633cb57c253c276f, 0x6e22d1ec31ebb502, 0xd3916126f2d14ca2, 0x17fbb8571a006596, 0xa1fafffffffe5557, 0x995bfff976a3fffe, 0x03f41d24d174ceb4, 0xf6547998c1995dbd, 0x778a468f507a6034, 0x020559931f7f8103, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000 }; 
const uint64_t bls12_381_pairing_fp12_inv_w3[72] = 
  { 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x1804000000015554, 0x855000053ab00001, 0x633cb57c253c276f, 0x6e22d1ec31ebb502, 0xd3916126f2d14ca2, 0x17fbb8571a006596, 0xa1fafffffffe5557, 0x995bfff976a3fffe, 0x03f41d24d174ceb4, 0xf6547998c1995dbd, 0x778a468f507a6034, 0x020559931f7f8103, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000 }; 

//------------------------------------------------------------------------------

// TODO: optimize this: (w^2, w^3, w^{-2}, w^{-3} are all just permutations and negations and additions of the coordinates)

// affine mapping from G2 to E(Fp12)
// psi(x,y) = (w^{-2}*x, w^{-3}*y)
// w^{-2} = (((0,0),(0,0),(_,_)),((0,0),(0,0),(0,0)))
// w^{-3} = (((0,0),(0,0),(0,0)),((0,0),(_,_),(0,0)))
// note:
void bls12_381_pairing_psi(const uint64_t *src, uint64_t *tgt) {
  memset( tgt , 0 , 8*(2*NWORDS_FP12) );
  bls12_381_Fp2_mont_mul( src              , bls12_381_pairing_fp12_inv_w2 + 2*NWORDS_FP2 , tgt +               2*NWORDS_FP2 );
  bls12_381_Fp2_mont_mul( src + NWORDS_FP2 , bls12_381_pairing_fp12_inv_w3 + 4*NWORDS_FP2 , tgt + NWORDS_FP12 + 4*NWORDS_FP2 );
}

// affine mapping from E(Fp12) to G2
// psi^{-1}(x,y) = (w^2*x, w^3*y)
// w^2 = (((0,0),(1,0),(0,0)),((0,0),(0,0),(0,0)))
// w^3 = (((0,0),(0,0),(0,0)),((0,0),(1,0),(0,0)))
void bls12_381_pairing_inverse_psi(const uint64_t *src, uint64_t *tgt) {
  uint64_t x[NWORDS_FP12];
  uint64_t y[NWORDS_FP12];
  bls12_381_Fp12_mont_mul( src               , bls12_381_pairing_fp12_w2 , x );
  bls12_381_Fp12_mont_mul( src + NWORDS_FP12 , bls12_381_pairing_fp12_w3 , y );
  // note: if everything is done correctly, the remaining 5 Fp2 coordinates of x/y are zero
  bls12_381_Fp2_mont_copy( x , tgt              );    
  bls12_381_Fp2_mont_copy( y , tgt + NWORDS_FP2 );    
}

void bls12_381_pairing_frobenius_G2(const uint64_t *src, uint64_t *tgt) {
  uint64_t tmp[NWORDS_FP12*2];
  bls12_381_pairing_psi(src, tmp);
  bls12_381_Fp12_mont_frobenius_inplace( tmp               );
  bls12_381_Fp12_mont_frobenius_inplace( tmp + NWORDS_FP12 );
  bls12_381_pairing_inverse_psi(tmp, tgt);
}

//------------------------------------------------------------------------------

// used when computing the line function, M-type twist
// C,B,A in Fp2
// calculates tgt = C*w^3 + B*w^2 + A in Fp12
// note:
// w^2 = (((0,0),(1,0),(0,0)),((0,0),(0,0),(0,0)))
// w^3 = (((0,0),(0,0),(0,0)),((0,0),(1,0),(0,0)))
void bls12_381_pairing_combine_w3_w2_1(const uint64_t *C, const uint64_t* B, const uint64_t *A, uint64_t *tgt) {
  memset( tgt , 0 , 8*NWORDS_FP12 );
  memcpy( tgt + 4*NWORDS_FP2  , C , 8*NWORDS_FP2 );
  memcpy( tgt +   NWORDS_FP2  , B , 8*NWORDS_FP2 );
  memcpy( tgt                 , A , 8*NWORDS_FP2 );
}

//------------------------------------------------------------------------------
// see "Fast Software Implementations of Bilinear Pairings" for the addition w/ line formulas

#define Px (P             )
#define Py (P + NWORDS_FP )

#define Qx (Q             )
#define Qy (Q + NWORDS_FP2)

#define Tx (T               )
#define Ty (T +   NWORDS_FP2)
#define Tz (T + 2*NWORDS_FP2)

//--------------------------------------

// this doubles `T in G2`, and also computes the line function l_psi(2T)(P) in Fp12
// P should be an affine point in G1, and T projective in G2 !
void bls12_381_pairing_miller_double(const uint64_t *P, uint64_t *T, uint64_t *line) {
  uint64_t A[NWORDS_FP2];
  uint64_t B[NWORDS_FP2];
  uint64_t C[NWORDS_FP2];
  uint64_t D[NWORDS_FP2];
  uint64_t E[NWORDS_FP2];
  uint64_t F[NWORDS_FP2];
  uint64_t G[NWORDS_FP2];
  uint64_t H[NWORDS_FP2];
  uint64_t tmp12[NWORDS_FP12];

  bls12_381_Fp2_mont_mul(Tx, Ty, A);
  bls12_381_Fp2_mont_div_by_2_inplace(A);                   // A = (X*Y)/2
  bls12_381_Fp2_mont_sqr(Ty, B);                            // B = Y^2
  bls12_381_Fp2_mont_sqr(Tz, C);                            // C = Z^2
  bls12_381_Fp2_mont_sqr(Tx, D);                            // D = X^2
  bls12_381_Fp2_mont_mul(C , bls12_381_pairing_twist_3B, E);    // E = (3*b)*C
  bls12_381_Fp2_mont_add(E , E , F);                      
  bls12_381_Fp2_mont_add_inplace(F , E);                    // F = 3*E           
  bls12_381_Fp2_mont_sub(B , F , Tx);                       //         B - F     
  bls12_381_Fp2_mont_mul_inplace(Tx, A);                    // X3 = A*(B - F)
  bls12_381_Fp2_mont_add(Ty , Tz , H);                      //      Y + Z
  bls12_381_Fp2_mont_sqr_inplace(H);                       //     (Y + Z)^2
  bls12_381_Fp2_mont_sub_inplace(H, B);                    //     (Y + Z)^2 - B
  bls12_381_Fp2_mont_sub_inplace(H, C);                    // H = (Y + Z)^2 - (B+C)
  bls12_381_Fp2_mont_add(B , F , G);                      
  bls12_381_Fp2_mont_div_by_2_inplace(G);                  // G = (B+F)/2 
  bls12_381_Fp2_mont_mul(B , H , Tz);                      // Z3 = B*H
  bls12_381_Fp2_mont_sqr(G , A);             //      G^2      
  bls12_381_Fp2_mont_sqr(E , C);             //      E^2  
  bls12_381_Fp2_mont_sub(A , C, Ty);         //      G^2 - E^2
  bls12_381_Fp2_mont_sub_inplace(Ty,C);      //      G^2 - 2*E^2
  bls12_381_Fp2_mont_sub_inplace(Ty,C);      // Y3 = G^2 - 3*E^2

  bls12_381_Fp_mont_add(Px, Px, tmp12);                  // 2*X_p
  bls12_381_Fp_mont_add_inplace(tmp12, Px);              // 3*X_p
  bls12_381_Fp2_mont_scale_by_prime_field(tmp12, D, C);  // C = 3*X^2*X_p

  bls12_381_Fp2_mont_sub_inplace(E,B);                   // E - B

  bls12_381_Fp2_mont_scale_by_prime_field(Py, H, H);     // Y_p*H
  bls12_381_Fp2_mont_neg_inplace(H);                     // -Y_p*H

  bls12_381_pairing_combine_w3_w2_1(H,C,E,line);    // -H*Y_p*w^3 + 3*X^2*X_p*w^2 + (E-B)    
}

//--------------------------------------

// this computes `T+=Q` in G2, and also computes the line function l_(T+Q)(P) in Fp12
// NOTE: T is projective, but P (in G1) and Q (in G2) are affine!
void bls12_381_pairing_miller_mixed_add(const uint64_t *P, const uint64_t *Q, uint64_t *T, uint64_t *line) {
  uint64_t A[NWORDS_FP2];
  uint64_t B[NWORDS_FP2];
  uint64_t C[NWORDS_FP2];
  uint64_t D[NWORDS_FP2];
  uint64_t E[NWORDS_FP2];
  uint64_t F[NWORDS_FP2];
  uint64_t G[NWORDS_FP2];
  uint64_t H[NWORDS_FP2];
  uint64_t I[NWORDS_FP2];
  uint64_t J[NWORDS_FP2];
  uint64_t lambda[NWORDS_FP2];
  uint64_t theta [NWORDS_FP2];
  uint64_t tmp12[NWORDS_FP12];

  bls12_381_Fp2_mont_mul(Qy,Tz,A);             // A = Y2 * Z
  bls12_381_Fp2_mont_mul(Qx,Tz,B);             // B = X2 * Z
  if ( bls12_381_Fp2_mont_is_equal(A,Ty) && bls12_381_Fp2_mont_is_equal(B,Tx) ) { 
    // Q = T
    bls12_381_pairing_miller_double(P,T,line);
    return;
  }

  // Q and T are different points
  bls12_381_Fp2_mont_sub(Ty,A,theta );         // theta  = Y - A
  bls12_381_Fp2_mont_sub(Tx,B,lambda);         // lambda = X - B
  bls12_381_Fp2_mont_sqr(theta, C);            // C = theta^2
  bls12_381_Fp2_mont_sqr(lambda,D);            // D = lambda^2
  bls12_381_Fp2_mont_mul(D,lambda,E);          // E = lambda^3
  bls12_381_Fp2_mont_mul(Tz,C,F);              // F = Z*C
  bls12_381_Fp2_mont_mul(Tx,D,G);              // G = X*D
  bls12_381_Fp2_mont_add(E,F,H);               // H = E + F ...
  bls12_381_Fp2_mont_sub_inplace(H,G);         // H = E + F - G...
  bls12_381_Fp2_mont_sub_inplace(H,G);         // H = E + F - 2G
  bls12_381_Fp2_mont_mul(lambda,H,Tx);         // X3 = lambda*H
  bls12_381_Fp2_mont_mul(Ty,E,I);              // I  = Y*E
  bls12_381_Fp2_mont_sub(G,H,Ty);              // G - H ...
  bls12_381_Fp2_mont_mul_inplace(Ty,theta);    // theta*(G-H) ...
  bls12_381_Fp2_mont_sub_inplace(Ty,I);        // Y3 = theta*(G-H) - I
  bls12_381_Fp2_mont_mul_inplace(Tz,E);        // Z3 = Z*E
  bls12_381_Fp2_mont_mul(theta ,Qx,A);         // theta*X2
  bls12_381_Fp2_mont_mul(lambda,Qy,B);         // lambda*Y2
  bls12_381_Fp2_mont_sub(A,B,J);               // J = theta*X2 - lambda*Y2

  bls12_381_Fp2_mont_scale_by_prime_field( Py, lambda, D );  // D = lambda * Y_p
  bls12_381_Fp2_mont_scale_by_prime_field( Px, theta , C );  // C = theta  * X_p
  bls12_381_Fp2_mont_neg_inplace(C);                         // ... - theta * X_p

  bls12_381_pairing_combine_w3_w2_1(D,C,J,line);  // lambda*Y_p*w^3 - theta*X_p*w^2 + J
}

//--------------------------------------

// inputs:  projective coordinates of points P in G1 and Q in G2 (affine points!)
// outputs: the final value Fp12 and the final T point (projective, G2)
void bls12_381_pairing_miller_loop(const uint64_t *P, const uint64_t *Q, uint64_t *out_T, uint64_t *out_f ) {
  uint64_t line[NWORDS_FP12];
  uint64_t f[NWORDS_FP12]; 
  uint64_t T[3*NWORDS_FP2]; 

  bls12_381_Fp12_mont_set_one(f);
  bls12_381_G2_proj_from_affine(Q,T);

  uint64_t x = bls12_381_miller_loop_param;
  for(int i=MILLER_LOOP_LENGTH-1; i>=0; i--) {
    bls12_381_Fp12_mont_sqr_inplace(f);
    bls12_381_pairing_miller_double(P,T,line);
    bls12_381_Fp12_mont_mul_inplace(f,line);
    if ((x>>i)&1) {
      bls12_381_pairing_miller_mixed_add(P,Q,T,line);
      bls12_381_Fp12_mont_mul_inplace(f,line);
    }
  }

  bls12_381_G2_proj_copy  (T, out_T);
  bls12_381_Fp12_mont_copy(f, out_f);
}

//------------------------------------------------------------------------------
// NOTE: currently the "hard exponentiation" dominates, so we should optimize that 


const uint64_t bls12_381_pairing_lam2_lam0[6] = { 0x73fefffeaaa9ffff, 0x7efb5555d8a7cffd, 0xd1bb89fe01c38e69, 0x6cd40a3c157b538a, 0x1fb322654a7cef70, 0x0000000000000000 };
const uint64_t bls12_381_pairing_lam1[6]      = { 0x73ffffffffff5554, 0x9d586d584eacaaaa, 0xc49f25e1a737f5e2, 0x26a48d1bb889d46d, 0x0000000000000000, 0x0000000000000000 };
const uint64_t bls12_381_pairing_p_lam2[6]    = { 0x9b560000aaab0000, 0x6c2f6d56d2021801, 0x2f1b4444d201019b, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000 };
const uint64_t bls12_381_pairing_lam3[6]      = { 0x8c00aaab0000aaaa, 0x396c8c005555e156, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000 };

void bls12_381_pairing_hard_expo(const uint64_t *src, uint64_t *tgt) {
  uint64_t A0[NWORDS_FP12]; 
  uint64_t A1[NWORDS_FP12]; 
  uint64_t A2[NWORDS_FP12]; 
  uint64_t A3[NWORDS_FP12]; 
  uint64_t frob_x0[NWORDS_FP12]; 

  // --- do the exponentiations ---

  uint64_t running[NWORDS_FP12];
  bls12_381_Fp12_mont_copy( src, running );

  bls12_381_Fp12_mont_set_one( A0 );
  bls12_381_Fp12_mont_set_one( A1 );
  bls12_381_Fp12_mont_set_one( A2 );
  bls12_381_Fp12_mont_set_one( A3 );

  // TODO: optimal multi-chain
  for(int k=0; k<320; k++) {
    int i = (k & 63);
    int j = (k >> 6);
    if ((bls12_381_pairing_lam2_lam0[j]  >> i) & 1) bls12_381_Fp12_mont_mul_inplace( A0 , running );
    if ((bls12_381_pairing_lam1[j]       >> i) & 1) bls12_381_Fp12_mont_mul_inplace( A1 , running );
    if ((bls12_381_pairing_p_lam2[j]     >> i) & 1) bls12_381_Fp12_mont_mul_inplace( A2 , running );
    if ((bls12_381_pairing_lam3[j]       >> i) & 1) bls12_381_Fp12_mont_mul_inplace( A3 , running );
    bls12_381_Fp12_mont_sqr_inplace( running );
  }

  // --- combine the final results

  // a0 = a2           / power x0 lam2_lam0
  // a1 =                power x0 lam1
  // a2 = frobenius x0 / power x0 p_lam2
  // a3 =                power x0 lam3

  bls12_381_Fp12_mont_frobenius( src, frob_x0 );     // frob(x0)

  bls12_381_Fp12_mont_inv_inplace( A0 );   
  bls12_381_Fp12_mont_inv_inplace( A2 );   

  bls12_381_Fp12_mont_mul_inplace( A2 , frob_x0 );        // x0^p / x^(p-lam2) = x0^lam2
  bls12_381_Fp12_mont_mul_inplace( A0 , A2      );        // x0^lam2 / x^(lam2-lam0) = x0^lam0

  bls12_381_Fp12_mont_frobenius_inplace ( A3 );     // frob  (x0^lam3)
  bls12_381_Fp12_mont_frobenius_inplace ( A3 );     // frob^2(x0^lam3)
  bls12_381_Fp12_mont_frobenius_inplace ( A3 );     // frob^3(x0^lam3)

  bls12_381_Fp12_mont_frobenius_inplace ( A2 );     // frob  (x0^lam2)
  bls12_381_Fp12_mont_frobenius_inplace ( A2 );     // frob^2(x0^lam2)

  bls12_381_Fp12_mont_frobenius_inplace ( A1 );     // frob  (x0^lam1)

  // result = A0*A1*A2*A3
  bls12_381_Fp12_mont_mul        (A0,A1,tgt);
  bls12_381_Fp12_mont_mul_inplace(tgt,A2);
  bls12_381_Fp12_mont_mul_inplace(tgt,A3);

}

//--------------------------------------


// exponentiation (in Fp12) to the power `(p^12-1)/r`
void bls12_381_pairing_final_expo(const uint64_t *src, uint64_t *tgt) {
  uint64_t A[NWORDS_FP12]; 
  uint64_t B[NWORDS_FP12]; 

  bls12_381_Fp12_mont_copy(src, A);
  bls12_381_Fp12_mont_frobenius_inplace(A);      // x^p
  bls12_381_Fp12_mont_frobenius_inplace(A);      // x^(p^2)
  bls12_381_Fp12_mont_frobenius_inplace(A);      // x^(p^3)
  bls12_381_Fp12_mont_frobenius_inplace(A);      // x^(p^4)
  bls12_381_Fp12_mont_frobenius_inplace(A);      // x^(p^5)
  bls12_381_Fp12_mont_frobenius_inplace(A);      // x^(p^6)
  bls12_381_Fp12_mont_div_inplace(A, src);       // x^(p^6 - 1)

  bls12_381_Fp12_mont_copy(A, B);
  bls12_381_Fp12_mont_frobenius_inplace(B);      // y^p
  bls12_381_Fp12_mont_frobenius_inplace(B);      // y^(p^2)
  bls12_381_Fp12_mont_mul_inplace(B, A);         // y^(p^2 + 1)

  bls12_381_pairing_hard_expo(B, tgt);
}

//------------------------------------------------------------------------------

// computes the optimal Ate pairing for BLS12-381. 
// P and Q are affine points in G1 and G2, respectively
// tgt is in Fp12
void bls12_381_pairing_affine(const uint64_t *P, const uint64_t *Q, uint64_t *tgt) {
  uint64_t f [NWORDS_FP12];      // Fp12
  uint64_t T [3*NWORDS_FP2];     // proj G2

  if ( bls12_381_G1_affine_is_infinity(P) || bls12_381_G2_affine_is_infinity(Q) ) {
    bls12_381_Fp12_mont_set_one(tgt);
    return;
  }

  bls12_381_pairing_miller_loop(P,Q,T,f);
  bls12_381_pairing_final_expo(f,tgt);
}

//------------------------------------------------------------------------------

// P and Q are projective points in G1 and G2, respectively
// tgt is in Fp12
void bls12_381_pairing_projective(const uint64_t *P, const uint64_t *Q, uint64_t *tgt) {
  uint64_t aff_P[2*NWORDS_FP ]; 
  uint64_t aff_Q[2*NWORDS_FP2]; 

  bls12_381_G1_proj_to_affine( P, aff_P );
  bls12_381_G2_proj_to_affine( Q, aff_Q );

  bls12_381_pairing_affine(aff_P, aff_Q, tgt);
}

//------------------------------------------------------------------------------
