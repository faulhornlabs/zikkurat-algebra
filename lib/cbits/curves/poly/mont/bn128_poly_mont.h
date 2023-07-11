#include <stdint.h>

extern int bn128_poly_mont_degree( const uint64_t *src );

extern void bn128_poly_mont_get_coeff( int n1, const uint64_t *src1, int k, uint64_t *tgt );
extern void bn128_poly_mont_eval_at  ( int n1, const uint64_t *src1, const uint64_t *loc, uint64_t *tgt);

extern uint8_t bn128_poly_mont_is_zero     ( int n1, const uint64_t *src1 );
extern uint8_t bn128_poly_mont_is_constant ( int n1, const uint64_t *src  , uint64_t *tgt_constant);
extern uint8_t bn128_poly_mont_is_equal    ( int n1, const uint64_t *src1, int n2, const uint64_t *src2 );

extern void bn128_poly_mont_neg( int n1, const uint64_t *src1, uint64_t *tgt );
extern void bn128_poly_mont_add( int n1, const uint64_t *src1, int n2, const uint64_t *src2, uint64_t *tgt );
extern void bn128_poly_mont_sub( int n1, const uint64_t *src1, int n2, const uint64_t *src2, uint64_t *tgt );
extern void bn128_poly_mont_scale( const uint64_t *kst1, int n2, const uint64_t *src2, uint64_t *tgt );
extern void bn128_poly_mont_mul_naive( int n1, const uint64_t *src1, int n2, const uint64_t *src2, uint64_t *tgt );

extern void bn128_poly_mont_lincomb( int K, const int *ns, const uint64_t **coeffs, const uint64_t **polys, uint64_t *tgt );
