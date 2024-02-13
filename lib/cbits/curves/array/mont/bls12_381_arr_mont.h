#include <stdint.h>

extern uint8_t bls12_381_arr_mont_is_valid ( int n, const uint64_t *src );
extern uint8_t bls12_381_arr_mont_is_zero  ( int n, const uint64_t *src );
extern uint8_t bls12_381_arr_mont_is_one   ( int n, const uint64_t *src );
extern uint8_t bls12_381_arr_mont_is_equal ( int n, const uint64_t *src1, const uint64_t *src2 );
extern void    bls12_381_arr_mont_set_zero ( int n,       uint64_t *tgt );
extern void    bls12_381_arr_mont_set_one  ( int n,       uint64_t *tgt );
extern void    bls12_381_arr_mont_set_const( int n, const uint64_t *src , uint64_t *tgt );
extern void    bls12_381_arr_mont_copy     ( int n, const uint64_t *src , uint64_t *tgt );

extern void bls12_381_arr_mont_from_std ( int n, const uint64_t *src , uint64_t *tgt );
extern void bls12_381_arr_mont_to_std   ( int n, const uint64_t *src , uint64_t *tgt );

extern void bls12_381_arr_mont_append( int n1, int n2, const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );

extern void bls12_381_arr_mont_neg ( int n, const uint64_t *src ,       uint64_t *tgt );
extern void bls12_381_arr_mont_add ( int n, const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );
extern void bls12_381_arr_mont_sub ( int n, const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );
extern void bls12_381_arr_mont_sqr ( int n, const uint64_t *src ,       uint64_t *tgt );
extern void bls12_381_arr_mont_mul ( int n, const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );
extern void bls12_381_arr_mont_inv ( int n, const uint64_t *src ,       uint64_t *tgt );
extern void bls12_381_arr_mont_div ( int n, const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );

extern void bls12_381_arr_mont_neg_inplace ( int n, uint64_t *tgt );
extern void bls12_381_arr_mont_add_inplace ( int n, uint64_t *tgt , const uint64_t *src2 );
extern void bls12_381_arr_mont_sub_inplace ( int n, uint64_t *tgt , const uint64_t *src2 );
extern void bls12_381_arr_mont_sqr_inplace ( int n, uint64_t *tgt );
extern void bls12_381_arr_mont_mul_inplace ( int n, uint64_t *tgt , const uint64_t *src2);
extern void bls12_381_arr_mont_inv_inplace ( int n, uint64_t *tgt );
extern void bls12_381_arr_mont_div_inplace ( int n, uint64_t *tgt , const uint64_t *src2 );
extern void bls12_381_arr_mont_sub_inplace_reverse ( int n, uint64_t *tgt , const uint64_t *src1 );

extern void bls12_381_arr_mont_mul_add ( int n, const uint64_t *src1, const uint64_t *src2, const uint64_t *src3, uint64_t *tgt );
extern void bls12_381_arr_mont_mul_sub ( int n, const uint64_t *src1, const uint64_t *src2, const uint64_t *src3, uint64_t *tgt );

extern void bls12_381_arr_mont_dot_prod ( int n, const uint64_t *src1 , const uint64_t *src2, uint64_t *tgt );
extern void bls12_381_arr_mont_powers ( int n, const uint64_t *coeffA , const uint64_t *coeffB, uint64_t *tgt );

extern void bls12_381_arr_mont_scale          ( int n, const uint64_t *coeff, const uint64_t *src2, uint64_t *tgt );
extern void bls12_381_arr_mont_scale_inplace  ( int n, const uint64_t *coeff,       uint64_t *tgt   );

extern void bls12_381_arr_mont_Ax_plus_y         ( int n, const uint64_t *coeffA, const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );
extern void bls12_381_arr_mont_Ax_plus_y_inplace ( int n, const uint64_t *coeffA,       uint64_t *tgt , const uint64_t *src2 );

extern void bls12_381_arr_mont_Ax_plus_By         ( int n, const uint64_t *coeffA, const uint64_t *coeffB, const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );
extern void bls12_381_arr_mont_Ax_plus_By_inplace ( int n, const uint64_t *coeffA, const uint64_t *coeffB,       uint64_t *tgt , const uint64_t *src2 );

