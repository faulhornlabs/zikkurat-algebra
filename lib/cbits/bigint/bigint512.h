#include <stdint.h>

extern uint8_t  bigint512_is_zero   ( const uint64_t *src );
extern uint8_t  bigint512_is_one    ( const uint64_t *src );
extern uint8_t  bigint512_is_equal  ( const uint64_t *src1, const uint64_t *src2 );
extern void     bigint512_set_zero  (       uint64_t *tgt );
extern void     bigint512_set_one   (       uint64_t *tgt );
extern void     bigint512_set_small (       uint64_t *tgt , uint64_t s );
extern void     bigint512_copy      ( const uint64_t *src , uint64_t *tgt );

extern void     bigint512_print      ( const uint64_t *src, int underscore_separators );
extern void     bigint512_debug_print( const char *txt, const uint64_t *src );

extern void     bigint512_neg( const uint64_t *src , uint64_t *tgt );
extern uint8_t  bigint512_add( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );
extern uint8_t  bigint512_sub( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );
extern void     bigint512_sqr( const uint64_t *src , uint64_t *tgt );
extern void     bigint512_mul( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );
extern void     bigint512_mul_truncated( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );

extern void     bigint512_neg_inplace( uint64_t *tgt );
extern uint8_t  bigint512_inc_inplace( uint64_t *tgt );
extern uint8_t  bigint512_dec_inplace( uint64_t *tgt );
extern uint8_t  bigint512_add_inplace( uint64_t *tgt, const uint64_t *src2 );
extern uint8_t  bigint512_sub_inplace( uint64_t *tgt, const uint64_t *src2 );
extern uint8_t  bigint512_sub_inplace_reverse( uint64_t *tgt, const uint64_t *src1 );

extern void     bigint512_scale( uint64_t z, const uint64_t *src, uint64_t *tgt );

extern uint8_t  bigint512_shift_left_by_1     ( const uint64_t *src, uint64_t *tgt         );
extern void     bigint512_shift_left_by_k     ( const uint64_t *src, uint64_t *tgt, int by );
extern uint8_t  bigint512_shift_right_by_1    ( const uint64_t *src, uint64_t *tgt         );
extern void     bigint512_shift_right_by_k    ( const uint64_t *src, uint64_t *tgt, int by );
