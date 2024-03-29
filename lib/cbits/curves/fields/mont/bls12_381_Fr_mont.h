#include <stdint.h>

extern void bls12_381_Fr_mont_from_std       (        const uint64_t *src ,       uint64_t *tgt );
extern void bls12_381_Fr_mont_to_std         (        const uint64_t *src ,       uint64_t *tgt );
extern void bls12_381_Fr_mont_batch_from_std ( int N, const uint64_t *src ,       uint64_t *tgt );
extern void bls12_381_Fr_mont_batch_to_std   ( int N, const uint64_t *src ,       uint64_t *tgt );

extern uint8_t bls12_381_Fr_mont_is_valid ( const uint64_t *src );
extern uint8_t bls12_381_Fr_mont_is_zero  ( const uint64_t *src );
extern uint8_t bls12_381_Fr_mont_is_one   ( const uint64_t *src );
extern uint8_t bls12_381_Fr_mont_is_equal ( const uint64_t *src1, const uint64_t *src2 );
extern void    bls12_381_Fr_mont_set_zero (       uint64_t *tgt );
extern void    bls12_381_Fr_mont_set_one  (       uint64_t *tgt );
extern void    bls12_381_Fr_mont_copy     ( const uint64_t *src , uint64_t *tgt );

extern void bls12_381_Fr_mont_neg ( const uint64_t *src ,       uint64_t *tgt );
extern void bls12_381_Fr_mont_add ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );
extern void bls12_381_Fr_mont_sub ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );
extern void bls12_381_Fr_mont_sqr ( const uint64_t *src ,       uint64_t *tgt );
extern void bls12_381_Fr_mont_mul ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );
extern void bls12_381_Fr_mont_inv ( const uint64_t *src ,       uint64_t *tgt );
extern void bls12_381_Fr_mont_div ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );

extern void bls12_381_Fr_mont_neg_inplace ( uint64_t *tgt );
extern void bls12_381_Fr_mont_sub_inplace ( uint64_t *tgt , const uint64_t *src2 );
extern void bls12_381_Fr_mont_add_inplace ( uint64_t *tgt , const uint64_t *src2 );
extern void bls12_381_Fr_mont_sqr_inplace ( uint64_t *tgt );
extern void bls12_381_Fr_mont_mul_inplace ( uint64_t *tgt , const uint64_t *src2);
extern void bls12_381_Fr_mont_inv_inplace ( uint64_t *tgt );
extern void bls12_381_Fr_mont_div_inplace ( uint64_t *tgt , const uint64_t *src2 );

extern void bls12_381_Fr_mont_sub_inplace_reverse ( uint64_t *tgt , const uint64_t *src1 );

extern void bls12_381_Fr_mont_div_by_2         ( const uint64_t *src , uint64_t *tgt );
extern void bls12_381_Fr_mont_div_by_2_inplace ( uint64_t *tgt );

extern void bls12_381_Fr_mont_batch_inv ( int n, const uint64_t *src, uint64_t *tgt );

extern void bls12_381_Fr_mont_pow_uint64( const uint64_t *src,       uint64_t  exponent, uint64_t *tgt );
extern void bls12_381_Fr_mont_pow_gen   ( const uint64_t *src, const uint64_t *expo    , uint64_t *tgt, int expo_len );
