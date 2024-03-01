#include <stdint.h>

extern int bls12_381_poly_mont_degree( const uint64_t *src );

extern void bls12_381_poly_mont_get_coeff( int n1, const uint64_t *src1, int k, uint64_t *tgt );
extern void bls12_381_poly_mont_eval_at  ( int n1, const uint64_t *src1, const uint64_t *loc, uint64_t *tgt);

extern uint8_t bls12_381_poly_mont_is_zero     ( int n1, const uint64_t *src1 );
extern uint8_t bls12_381_poly_mont_is_constant ( int n1, const uint64_t *src  , uint64_t *tgt_constant);
extern uint8_t bls12_381_poly_mont_is_equal    ( int n1, const uint64_t *src1, int n2, const uint64_t *src2 );

extern void bls12_381_poly_mont_neg( int n1, const uint64_t *src1, uint64_t *tgt );
extern void bls12_381_poly_mont_add( int n1, const uint64_t *src1, int n2, const uint64_t *src2, uint64_t *tgt );
extern void bls12_381_poly_mont_sub( int n1, const uint64_t *src1, int n2, const uint64_t *src2, uint64_t *tgt );
extern void bls12_381_poly_mont_scale( const uint64_t *kst1, int n2, const uint64_t *src2, uint64_t *tgt );
extern void bls12_381_poly_mont_mul_naive( int n1, const uint64_t *src1, int n2, const uint64_t *src2, uint64_t *tgt );

extern void bls12_381_poly_mont_lincomb( int K, const int *ns, const uint64_t **coeffs, const uint64_t **polys, uint64_t *tgt );

extern void bls12_381_poly_mont_long_div( int n1, const uint64_t *src1, int n2, const uint64_t *src2, int nquot, uint64_t *quot, int nrem, uint64_t *rem );
extern void bls12_381_poly_mont_quot    ( int n1, const uint64_t *src1, int n2, const uint64_t *src2, int nquot, uint64_t *quot                          );
extern void bls12_381_poly_mont_rem     ( int n1, const uint64_t *src1, int n2, const uint64_t *src2,                            int nrem, uint64_t *rem );

extern void    bls12_381_poly_mont_div_by_vanishing ( int n1, const uint64_t *src1, int expo_n, const uint64_t *eta, int nquot, uint64_t *quot, int nrem, uint64_t *rem );
extern uint8_t bls12_381_poly_mont_quot_by_vanishing( int n1, const uint64_t *src1, int expo_n, const uint64_t *eta, int nquot, uint64_t *quot );

extern void bls12_381_poly_mont_ntt_forward( int m, const uint64_t *gen, const uint64_t *src, uint64_t *tgt);
extern void bls12_381_poly_mont_ntt_inverse( int m, const uint64_t *gen, const uint64_t *src, uint64_t *tgt);

extern void bls12_381_poly_mont_ntt_forward_shifted( const uint64_t *eta, int m, const uint64_t *gen, const uint64_t *src, uint64_t *tgt);
extern void bls12_381_poly_mont_ntt_inverse_shifted( const uint64_t *eta, int m, const uint64_t *gen, const uint64_t *src, uint64_t *tgt);
