#include <stdint.h>

extern void bls12_381_G1_affine_copy( const uint64_t *src , uint64_t *tgt );

extern uint8_t bls12_381_G1_affine_is_on_curve   ( const uint64_t *src );
extern uint8_t bls12_381_G1_affine_is_infinity   ( const uint64_t *src );
extern void    bls12_381_G1_affine_set_infinity  (       uint64_t *tgt );
extern uint8_t bls12_381_G1_affine_is_in_subgroup( const uint64_t *src );

extern uint8_t bls12_381_G1_affine_is_equal( const uint64_t *src1, const uint64_t *src2 );
extern uint8_t bls12_381_G1_affine_is_same ( const uint64_t *src1, const uint64_t *src2 );

extern void bls12_381_G1_affine_neg        ( const uint64_t *src ,       uint64_t *tgt );
extern void bls12_381_G1_affine_dbl        ( const uint64_t *src ,       uint64_t *tgt );
extern void bls12_381_G1_affine_add        ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );
extern void bls12_381_G1_affine_sub        ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );

extern void bls12_381_G1_affine_neg_inplace(       uint64_t *tgt );
extern void bls12_381_G1_affine_dbl_inplace(       uint64_t *tgt );
extern void bls12_381_G1_affine_add_inplace(       uint64_t *tgt , const uint64_t *src2 );
extern void bls12_381_G1_affine_sub_inplace(       uint64_t *tgt , const uint64_t *src2 );

extern void bls12_381_G1_affine_scl_generic( const uint64_t *kst , const uint64_t *src , uint64_t *tgt , int kst_len );
extern void bls12_381_G1_affine_scl_Fr_std ( const uint64_t *kst , const uint64_t *src , uint64_t *tgt );
extern void bls12_381_G1_affine_scl_Fr_mont( const uint64_t *kst , const uint64_t *src , uint64_t *tgt );
extern void bls12_381_G1_affine_scl_big    ( const uint64_t *kst , const uint64_t *src , uint64_t *tgt );
extern void bls12_381_G1_affine_scl_small  (       uint64_t  kst , const uint64_t *src , uint64_t *tgt );
