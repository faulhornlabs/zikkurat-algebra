#include <stdint.h>

extern uint8_t bls12_381_Fr_std_is_valid( const uint64_t *src );

extern uint8_t bls12_381_Fr_std_is_zero   ( const uint64_t *src );
extern uint8_t bls12_381_Fr_std_is_one    ( const uint64_t *src );
extern uint8_t bls12_381_Fr_std_is_equal  ( const uint64_t *src1, const uint64_t *src2 );
extern void    bls12_381_Fr_std_set_zero  (       uint64_t *tgt );
extern void    bls12_381_Fr_std_set_one   (       uint64_t *tgt );
extern void    bls12_381_Fr_std_copy      ( const uint64_t *src , uint64_t *tgt );

extern void bls12_381_Fr_std_neg( const uint64_t *src , uint64_t *tgt );
extern void bls12_381_Fr_std_add( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );
extern void bls12_381_Fr_std_sub( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );
extern void bls12_381_Fr_std_sqr( const uint64_t *src , uint64_t *tgt );
extern void bls12_381_Fr_std_mul( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );
extern void bls12_381_Fr_std_inv( const uint64_t *src1, uint64_t *tgt );
extern void bls12_381_Fr_std_div( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );

extern void bls12_381_Fr_std_neg_inplace( uint64_t *tgt );
extern void bls12_381_Fr_std_add_inplace( uint64_t *tgt, const uint64_t *src2 );
extern void bls12_381_Fr_std_sub_inplace( uint64_t *tgt, const uint64_t *src2 );
extern void bls12_381_Fr_std_sqr_inplace( uint64_t *tgt );
extern void bls12_381_Fr_std_mul_inplace( uint64_t *tgt, const uint64_t *src2 );
extern void bls12_381_Fr_std_inv_inplace( uint64_t *tgt );
extern void bls12_381_Fr_std_div_inplace( uint64_t *tgt, const uint64_t *src2 );

extern void bls12_381_Fr_std_sub_inplace_reverse( uint64_t *tgt, const uint64_t *src1 );

extern void bls12_381_Fr_std_div_by_2         ( const uint64_t *src , uint64_t *tgt );
extern void bls12_381_Fr_std_div_by_2_inplace ( uint64_t *tgt );

extern void bls12_381_Fr_std_batch_inv        ( int n, const uint64_t *src, uint64_t *tgt );

extern void bls12_381_Fr_std_reduce_modp     ( const uint64_t *src , uint64_t *tgt );

extern void bls12_381_Fr_std_pow_uint64( const uint64_t *src,       uint64_t  exponent, uint64_t *tgt );
extern void bls12_381_Fr_std_pow_gen   ( const uint64_t *src, const uint64_t *expo    , uint64_t *tgt, int expo_len );

