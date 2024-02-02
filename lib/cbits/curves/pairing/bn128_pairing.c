#include "stdint.h"
#include "stdlib.h"
#include "string.h"
#include "assert.h"

#include "curves/fields/mont/bn128_Fp_mont.h"
#include "curves/fields/mont/bn128_Fp2_mont.h"
#include "curves/fields/mont/bn128_Fp12_mont.h"

#include "curves/g1/affine/bn128_G1_affine.h"
#include "curves/g2/affine/bn128_G2_affine.h"
#include "curves/g1/proj/bn128_G1_proj.h"
#include "curves/g2/proj/bn128_G2_proj.h"

//------------------------------------------------------------------------------

#define NWORDS_FP   4
#define NWORDS_FP2  8
#define NWORDS_FP12 48

//------------------------------------------------------------------------------

const uint64_t bn128_pairing_twist_B [8] = { 0x3bf938e377b802a8, 0x020b1b273633535d, 0x26b7edf049755260, 0x2514c6324384a86d, 0x38e7ecccd1dcff67, 0x65f0b37d93ce0d3e, 0xd749d0dd22ac00aa, 0x0141b9ce4a688d4d };
const uint64_t bn128_pairing_twist_3B[8] = { 0x3baa927cb62e0d6a, 0xd71e7c52d1b664fd, 0x03873e63d95d4664, 0x0e75b5b1082ab8f4, 0xaab7c6667596fe35, 0x31d21a78bb6a27ba, 0x85dd7297680401ff, 0x03c52d6adf39a7e9 };

#define MILLER_LOOP_LENGTH 64

// note: the parameter is (6*x+2) = 0x19d797039be763ba8, which is 65 bits.
// but the loop doesn't use the topmost bit, so it just fits into a 64 bit word
const uint64_t bn128_miller_loop_param = 0x9d797039be763ba8;

const uint64_t bn128_pairing_fp12_inv_w2[48] = 
  { 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x9168c5b062e5ff12, 0x65af5018ad07a2d2, 0x3272d31f197d565e, 0x2c9f210801f7f840, 0x684d4eeef09effcd, 0xcca59129dbef59bf, 0x9d189af460e40038, 0x006b3def6e22d9c4, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000 };
const uint64_t bn128_pairing_fp12_inv_w3[48] = 
  { 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x9168c5b062e5ff12, 0x65af5018ad07a2d2, 0x3272d31f197d565e, 0x2c9f210801f7f840, 0x684d4eeef09effcd, 0xcca59129dbef59bf, 0x9d189af460e40038, 0x006b3def6e22d9c4, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000 };

//------------------------------------------------------------------------------

// TODO: optimize this: (w^2, w^3, w^{-2}, w^{-3} are all just permutations and negations and additions of the coordinates)

// affine mapping from G2 to E(Fp12)
// psi(x,y) = (w^2*x, w^3*y)
// note:
// w^2 = (((0,0),(1,0),(0,0)),((0,0),(0,0),(0,0)))
// w^3 = (((0,0),(0,0),(0,0)),((0,0),(1,0),(0,0)))
void bn128_pairing_psi(const uint64_t *src, uint64_t *tgt) {
  memset( tgt , 0 , 8*(2*NWORDS_FP12) );
  bn128_Fp2_mont_copy( src                , tgt +                 NWORDS_FP2 );
  bn128_Fp2_mont_copy( src + NWORDS_FP2   , tgt + NWORDS_FP12 + 4*NWORDS_FP2 );
}

// affine mapping from E(Fp12) to G2
void bn128_pairing_inverse_psi(const uint64_t *src, uint64_t *tgt) {
  uint64_t x[NWORDS_FP12];
  uint64_t y[NWORDS_FP12];
  bn128_Fp12_mont_mul( src               , bn128_pairing_fp12_inv_w2 , x );
  bn128_Fp12_mont_mul( src + NWORDS_FP12 , bn128_pairing_fp12_inv_w3 , y );
  // note: if everything is done correctly, the remaining 5 Fp2 coordinates of x/y are zero
  bn128_Fp2_mont_copy( x , tgt              );    
  bn128_Fp2_mont_copy( y , tgt + NWORDS_FP2 );    
}

void bn128_pairing_frobenius_G2(const uint64_t *src, uint64_t *tgt) {
  uint64_t tmp[NWORDS_FP12*2];
  bn128_pairing_psi(src, tmp);
  bn128_Fp12_mont_frobenius_inplace( tmp               );
  bn128_Fp12_mont_frobenius_inplace( tmp + NWORDS_FP12 );
  bn128_pairing_inverse_psi(tmp, tgt);
}

//------------------------------------------------------------------------------

// used when computing the line function, D-type twist
// A,B,C in Fp2
// calculates tgt = A + B*w + C*w^3 in Fp12
// note:
// w   = (((0,0),(0,0),(0,0)),((1,0),(0,0),(0,0)))
// w^3 = (((0,0),(0,0),(0,0)),((0,0),(1,0),(0,0)))
void bn128_pairing_combine_1_w_w3(const uint64_t *A, const uint64_t* B, const uint64_t *C, uint64_t *tgt) {
  memset( tgt , 0 , 8*NWORDS_FP12 );
  memcpy( tgt                 , A , 8*NWORDS_FP2 );
  memcpy( tgt + 3*NWORDS_FP2  , B , 8*NWORDS_FP2 );
  memcpy( tgt + 4*NWORDS_FP2  , C , 8*NWORDS_FP2 );
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
void bn128_pairing_miller_double(const uint64_t *P, uint64_t *T, uint64_t *line) {
  uint64_t A[NWORDS_FP2];
  uint64_t B[NWORDS_FP2];
  uint64_t C[NWORDS_FP2];
  uint64_t D[NWORDS_FP2];
  uint64_t E[NWORDS_FP2];
  uint64_t F[NWORDS_FP2];
  uint64_t G[NWORDS_FP2];
  uint64_t H[NWORDS_FP2];
  uint64_t tmp12[NWORDS_FP12];

  bn128_Fp2_mont_mul(Tx, Ty, A);
  bn128_Fp2_mont_div_by_2_inplace(A);                   // A = (X*Y)/2
  bn128_Fp2_mont_sqr(Ty, B);                            // B = Y^2
  bn128_Fp2_mont_sqr(Tz, C);                            // C = Z^2
  bn128_Fp2_mont_sqr(Tx, D);                            // D = X^2
  bn128_Fp2_mont_mul(C , bn128_pairing_twist_3B, E);    // E = (3*b)*C
  bn128_Fp2_mont_add(E , E , F);                      
  bn128_Fp2_mont_add_inplace(F , E);                    // F = 3*E           
  bn128_Fp2_mont_sub(B , F , Tx);                       //         B - F     
  bn128_Fp2_mont_mul_inplace(Tx, A);                    // X3 = A*(B - F)
  bn128_Fp2_mont_add(Ty , Tz , H);                      //      Y + Z
  bn128_Fp2_mont_sqr_inplace(H);                       //     (Y + Z)^2
  bn128_Fp2_mont_sub_inplace(H, B);                    //     (Y + Z)^2 - B
  bn128_Fp2_mont_sub_inplace(H, C);                    // H = (Y + Z)^2 - (B+C)
  bn128_Fp2_mont_add(B , F , G);                      
  bn128_Fp2_mont_div_by_2_inplace(G);                  // G = (B+F)/2 
  bn128_Fp2_mont_mul(B , H , Tz);                      // Z3 = B*H
  bn128_Fp2_mont_sqr(G , A);             //      G^2      
  bn128_Fp2_mont_sqr(E , C);             //      E^2  
  bn128_Fp2_mont_sub(A , C, Ty);         //      G^2 - E^2
  bn128_Fp2_mont_sub_inplace(Ty,C);      //      G^2 - 2*E^2
  bn128_Fp2_mont_sub_inplace(Ty,C);      // Y3 = G^2 - 3*E^2

  bn128_Fp_mont_add(Px, Px, tmp12);                  // 2*X_p
  bn128_Fp_mont_add_inplace(tmp12, Px);              // 3*X_p
  bn128_Fp2_mont_scale_by_prime_field(tmp12, D, C);  // C = 3*X^2*X_p

  bn128_Fp2_mont_sub_inplace(E,B);                   // E - B

  bn128_Fp2_mont_scale_by_prime_field(Py, H, H);     // Y_p*H
  bn128_Fp2_mont_neg_inplace(H);                     // -Y_p*H

  bn128_pairing_combine_1_w_w3 (H,C,E,line);    // -H*Y_p     + 3*X^2*X_p*w   + (E-B)*w^3
}

//--------------------------------------

// this computes `T+=Q` in G2, and also computes the line function l_(T+Q)(P) in Fp12
// NOTE: T is projective, but P (in G1) and Q (in G2) are affine!
void bn128_pairing_miller_mixed_add(const uint64_t *P, const uint64_t *Q, uint64_t *T, uint64_t *line) {
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

  bn128_Fp2_mont_mul(Qy,Tz,A);             // A = Y2 * Z
  bn128_Fp2_mont_mul(Qx,Tz,B);             // B = X2 * Z
  if ( bn128_Fp2_mont_is_equal(A,Ty) && bn128_Fp2_mont_is_equal(B,Tx) ) { 
    // Q = T
    bn128_pairing_miller_double(P,T,line);
    return;
  }

  // Q and T are different points
  bn128_Fp2_mont_sub(Ty,A,theta );         // theta  = Y - A
  bn128_Fp2_mont_sub(Tx,B,lambda);         // lambda = X - B
  bn128_Fp2_mont_sqr(theta, C);            // C = theta^2
  bn128_Fp2_mont_sqr(lambda,D);            // D = lambda^2
  bn128_Fp2_mont_mul(D,lambda,E);          // E = lambda^3
  bn128_Fp2_mont_mul(Tz,C,F);              // F = Z*C
  bn128_Fp2_mont_mul(Tx,D,G);              // G = X*D
  bn128_Fp2_mont_add(E,F,H);               // H = E + F ...
  bn128_Fp2_mont_sub_inplace(H,G);         // H = E + F - G...
  bn128_Fp2_mont_sub_inplace(H,G);         // H = E + F - 2G
  bn128_Fp2_mont_mul(lambda,H,Tx);         // X3 = lambda*H
  bn128_Fp2_mont_mul(Ty,E,I);              // I  = Y*E
  bn128_Fp2_mont_sub(G,H,Ty);              // G - H ...
  bn128_Fp2_mont_mul_inplace(Ty,theta);    // theta*(G-H) ...
  bn128_Fp2_mont_sub_inplace(Ty,I);        // Y3 = theta*(G-H) - I
  bn128_Fp2_mont_mul_inplace(Tz,E);        // Z3 = Z*E
  bn128_Fp2_mont_mul(theta ,Qx,A);         // theta*X2
  bn128_Fp2_mont_mul(lambda,Qy,B);         // lambda*Y2
  bn128_Fp2_mont_sub(A,B,J);               // J = theta*X2 - lambda*Y2

  bn128_Fp2_mont_scale_by_prime_field( Py, lambda, D );  // D = lambda * Y_p
  bn128_Fp2_mont_scale_by_prime_field( Px, theta , C );  // C = theta  * X_p
  bn128_Fp2_mont_neg_inplace(C);                         // ... - theta * X_p

  bn128_pairing_combine_1_w_w3 (D,C,J,line);  // lambda*Y_p     - theta*X_p*w   + J*w^3
}

//--------------------------------------

// inputs:  projective coordinates of points P in G1 and Q in G2 (affine points!)
// outputs: the final value Fp12 and the final T point (projective, G2)
void bn128_pairing_miller_loop(const uint64_t *P, const uint64_t *Q, uint64_t *out_T, uint64_t *out_f ) {
  uint64_t line[NWORDS_FP12];
  uint64_t f[NWORDS_FP12]; 
  uint64_t T[3*NWORDS_FP2]; 

  bn128_Fp12_mont_set_one(f);
  bn128_G2_proj_from_affine(Q,T);

  uint64_t x = bn128_miller_loop_param;
  for(int i=MILLER_LOOP_LENGTH-1; i>=0; i--) {
    bn128_Fp12_mont_sqr_inplace(f);
    bn128_pairing_miller_double(P,T,line);
    bn128_Fp12_mont_mul_inplace(f,line);
    if ((x>>i)&1) {
      bn128_pairing_miller_mixed_add(P,Q,T,line);
      bn128_Fp12_mont_mul_inplace(f,line);
    }
  }

  bn128_G2_proj_copy  (T, out_T);
  bn128_Fp12_mont_copy(f, out_f);
}

//------------------------------------------------------------------------------
// NOTE: currently the "hard exponentiation" dominates, so we should optimize that 


const uint64_t bn128_pairing_p_minus_lam0[4]                = { 0xb687f7e0078302b6, 0x3a97459a6afe5ea2, 0xb3c4d79d41a91759, 0x0000000000000000 };
const uint64_t bn128_pairing_lam1_minus_lam0_minus_2lam2[4] = { 0x9d797039be763ba8, 0x0000000000000001, 0x0000000000000000, 0x0000000000000000 };
const uint64_t bn128_pairing_lam2[4]                        = { 0xf83e9682e87cfd46, 0x6f4d8248eeb859fb, 0x0000000000000000, 0x0000000000000000 };

void bn128_pairing_hard_expo(const uint64_t *src, uint64_t *tgt) {
  uint64_t A0[NWORDS_FP12]; 
  uint64_t A1[NWORDS_FP12]; 
  uint64_t A2[NWORDS_FP12]; 
  uint64_t A3[NWORDS_FP12]; 

  // --- do the exponentiations ---

  uint64_t running[NWORDS_FP12];
  bn128_Fp12_mont_copy( src, running );

  // bn128_Fp12_mont_pow_gen( src, bn128_pairing_p_minus_lam0                , A0 , 3 );
  // bn128_Fp12_mont_pow_gen( src, bn128_pairing_lam1_minus_lam0_minus_2lam2 , A1 , 2 );
  // bn128_Fp12_mont_pow_gen( src, bn128_pairing_lam2                        , A2 , 2 );

  bn128_Fp12_mont_set_one( A0 );
  bn128_Fp12_mont_set_one( A1 );
  bn128_Fp12_mont_set_one( A2 );

  // TODO: optimal multi-chain
  for(int k=0; k<192; k++) {
    int i = (k & 63);
    int j = (k >> 6);
    if ((bn128_pairing_p_minus_lam0[j]                >> i) & 1) bn128_Fp12_mont_mul_inplace( A0 , running );
    if ((bn128_pairing_lam1_minus_lam0_minus_2lam2[j] >> i) & 1) bn128_Fp12_mont_mul_inplace( A1 , running );
    if ((bn128_pairing_lam2[j]                        >> i) & 1) bn128_Fp12_mont_mul_inplace( A2 , running );
    bn128_Fp12_mont_sqr_inplace( running );
  }

  // --- combine the final results

  // a0 = frobenius x0 / power x0 q_lam0
  // a1 = a0 * a2 * a2 * power x0 lam1_lam0_2lam2
  // a2 =                power x0 lam2
  // a3 =                      x0

  bn128_Fp12_mont_copy         ( src, A3 );     // x0
  bn128_Fp12_mont_frobenius_inplace ( A3 );     // frob(x0)

  bn128_Fp12_mont_inv_inplace( A0 );
  bn128_Fp12_mont_mul_inplace( A0, A3 );        // x0^p / x^(p-lam0) = x0^lam0

  bn128_Fp12_mont_mul_inplace( A1, A0 );   
  bn128_Fp12_mont_mul_inplace( A1, A2 );   
  bn128_Fp12_mont_mul_inplace( A1, A2 );        // x0^(lam1-lam0-2*lam2) * a0 * a2^2 = x0^lam1

  bn128_Fp12_mont_frobenius_inplace ( A3 );     // frob^2(x0)
  bn128_Fp12_mont_frobenius_inplace ( A3 );     // frob^3(x0)

  bn128_Fp12_mont_frobenius_inplace ( A2 );     // frob  (x0^lam2)
  bn128_Fp12_mont_frobenius_inplace ( A2 );     // frob^2(x0^lam2)

  bn128_Fp12_mont_frobenius_inplace ( A1 );     // frob  (x0^lam1)

  // result = A0*A1*A2*A3
  bn128_Fp12_mont_mul        (A0,A1,tgt);
  bn128_Fp12_mont_mul_inplace(tgt,A2);
  bn128_Fp12_mont_mul_inplace(tgt,A3);
}

//--------------------------------------


// exponentiation (in Fp12) to the power `(p^12-1)/r`
void bn128_pairing_final_expo(const uint64_t *src, uint64_t *tgt) {
  uint64_t A[NWORDS_FP12]; 
  uint64_t B[NWORDS_FP12]; 

  bn128_Fp12_mont_copy(src, A);
  bn128_Fp12_mont_frobenius_inplace(A);      // x^p
  bn128_Fp12_mont_frobenius_inplace(A);      // x^(p^2)
  bn128_Fp12_mont_frobenius_inplace(A);      // x^(p^3)
  bn128_Fp12_mont_frobenius_inplace(A);      // x^(p^4)
  bn128_Fp12_mont_frobenius_inplace(A);      // x^(p^5)
  bn128_Fp12_mont_frobenius_inplace(A);      // x^(p^6)
  bn128_Fp12_mont_div_inplace(A, src);       // x^(p^6 - 1)

  bn128_Fp12_mont_copy(A, B);
  bn128_Fp12_mont_frobenius_inplace(B);      // y^p
  bn128_Fp12_mont_frobenius_inplace(B);      // y^(p^2)
  bn128_Fp12_mont_mul_inplace(B, A);         // y^(p^2 + 1)

  bn128_pairing_hard_expo(B, tgt);
}

//------------------------------------------------------------------------------

// computes the optimal Ate pairing for BN128. 
// P and Q are affine points in G1 and G2, respectively
// tgt is in Fp12
void bn128_pairing_affine(const uint64_t *P, const uint64_t *Q, uint64_t *tgt) {
  uint64_t f [NWORDS_FP12];      // Fp12
  uint64_t f2[NWORDS_FP12];      // Fp12
  uint64_t T [3*NWORDS_FP2];     // proj G2
  uint64_t T2[3*NWORDS_FP2];     // proj G2
  uint64_t phiQ [2*NWORDS_FP2];  // affine G2
  uint64_t phi2Q[2*NWORDS_FP2];  // affine G2

  if ( bn128_G1_affine_is_infinity(P) || bn128_G2_affine_is_infinity(Q) ) {
    bn128_Fp12_mont_set_one(tgt);
    return;
  }

  bn128_pairing_frobenius_G2(Q   , phiQ );          //  pi(Q)
  bn128_pairing_frobenius_G2(phiQ, phi2Q);          //  pi^2(Q)
  bn128_G2_affine_neg_inplace(phi2Q);               // -pi^2(Q)

  bn128_pairing_miller_loop(P,Q,T,f);

  //uint64_t tmp[3*NWORDS_FP2];
  //bn128_G2_proj_from_affine(phiQ,tmp);
  //bn128_G2_proj_add(T,tmp,T2);
  bn128_G2_proj_madd_proj_aff(T,phiQ,T2);           // T2 = T + phiQ;

  bn128_pairing_miller_mixed_add(P,phiQ,T,f2);      //         line(T, phiQ)
  bn128_Fp12_mont_mul_inplace(f,f2);                // f = f * line(T, phiQ)

  bn128_pairing_miller_mixed_add(P,phi2Q,T2,f2);    //         line(T+phiQ, -phi2Q)
  bn128_Fp12_mont_mul_inplace(f,f2);                // f = f * line(T+phiQ, -phi2Q)
 
  bn128_pairing_final_expo(f, tgt);
}

//------------------------------------------------------------------------------

// P and Q are projective points in G1 and G2, respectively
// tgt is in Fp12
void bn128_pairing_projective(const uint64_t *P, const uint64_t *Q, uint64_t *tgt) {
  uint64_t aff_P[2*NWORDS_FP ]; 
  uint64_t aff_Q[2*NWORDS_FP2]; 

  bn128_G1_proj_to_affine( P, aff_P );
  bn128_G2_proj_to_affine( Q, aff_Q );

  bn128_pairing_affine(aff_P, aff_Q, tgt);
}

//------------------------------------------------------------------------------
