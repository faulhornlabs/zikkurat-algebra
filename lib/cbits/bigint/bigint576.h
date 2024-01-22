#include <stdint.h>

extern uint8_t  bigint576_is_zero   ( const uint64_t *src );
extern uint8_t  bigint576_is_one    ( const uint64_t *src );
extern uint8_t  bigint576_is_equal  ( const uint64_t *src1, const uint64_t *src2 );
extern void     bigint576_set_zero  (       uint64_t *tgt );
extern void     bigint576_set_one   (       uint64_t *tgt );
extern void     bigint576_set_small (       uint64_t *tgt , uint64_t s );
extern void     bigint576_copy      ( const uint64_t *src , uint64_t *tgt );

extern void     bigint576_print      ( const uint64_t *src, int underscore_separators );
extern void     bigint576_debug_print( const char *txt, const uint64_t *src );

extern void     bigint576_neg( const uint64_t *src , uint64_t *tgt );
extern uint8_t  bigint576_add( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );
extern uint8_t  bigint576_sub( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );
extern void     bigint576_sqr( const uint64_t *src , uint64_t *tgt );
extern void     bigint576_mul( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );
extern void     bigint576_sqr_truncated( const uint64_t *src1,                       uint64_t *tgt );
extern void     bigint576_mul_truncated( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );

extern void     bigint576_neg_inplace( uint64_t *tgt );
extern uint8_t  bigint576_inc_inplace( uint64_t *tgt );
extern uint8_t  bigint576_dec_inplace( uint64_t *tgt );
extern uint8_t  bigint576_add_inplace( uint64_t *tgt, const uint64_t *src2 );
extern uint8_t  bigint576_sub_inplace( uint64_t *tgt, const uint64_t *src2 );
extern uint8_t  bigint576_sub_inplace_reverse( uint64_t *tgt, const uint64_t *src1 );

extern void     bigint576_scale( uint64_t z, const uint64_t *src, uint64_t *tgt );

extern uint8_t  bigint576_shift_left_by_1     ( const uint64_t *src, uint64_t *tgt         );
extern void     bigint576_shift_left_by_k     ( const uint64_t *src, uint64_t *tgt, int by );
extern uint8_t  bigint576_shift_right_by_1    ( const uint64_t *src, uint64_t *tgt         );
extern void     bigint576_shift_right_by_k    ( const uint64_t *src, uint64_t *tgt, int by );
