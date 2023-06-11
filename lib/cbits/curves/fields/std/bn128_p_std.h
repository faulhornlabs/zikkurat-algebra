#include <stdint.h>

extern uint8_t bn128_p_std_is_valid( const uint64_t *src );

extern void bn128_p_std_neg( const uint64_t *src , uint64_t *tgt );
extern void bn128_p_std_neg( const uint64_t *src , uint64_t *tgt );
extern void bn128_p_std_add( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );
extern void bn128_p_std_sub( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );
extern void bn128_p_std_sqr( const uint64_t *src , uint64_t *tgt );
extern void bn128_p_std_mul( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );
extern void bn128_p_std_inv( const uint64_t *src1, uint64_t *tgt );
extern void bn128_p_std_div( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );

extern void bn128_p_std_neg_inplace( uint64_t *tgt );
extern void bn128_p_std_add_inplace( uint64_t *tgt, const uint64_t *src2 );
extern void bn128_p_std_sub_inplace( uint64_t *tgt, const uint64_t *src2 );
extern void bn128_p_std_sqr_inplace( uint64_t *tgt );
extern void bn128_p_std_mul_inplace( uint64_t *tgt, const uint64_t *src2 );
extern void bn128_p_std_inv_inplace( uint64_t *tgt );
extern void bn128_p_std_div_inplace( uint64_t *tgt, const uint64_t *src2 );

extern void bn128_p_std_sub_inplace_reverse( uint64_t *tgt, const uint64_t *src1 );

extern void bn128_p_std_div_by_2           ( const uint64_t *src , uint64_t *tgt );
extern void bn128_p_std_div_by_2_inplace   ( uint64_t *tgt );

extern void bn128_p_std_reduce_modp     ( const uint64_t *src , uint64_t *tgt );

extern void bn128_p_std_pow_uint64( const uint64_t *src,       uint64_t  exponent, uint64_t *tgt );
extern void bn128_p_std_pow_gen   ( const uint64_t *src, const uint64_t *expo    , uint64_t *tgt, int expo_len );

