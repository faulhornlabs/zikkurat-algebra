#include <stdint.h>

extern void bls12_381_G1_proj_normalize         ( const uint64_t *src , uint64_t *tgt );
extern void bls12_381_G1_proj_normalize_inplace (       uint64_t *tgt );

extern void bls12_381_G1_proj_copy        ( const uint64_t *src , uint64_t *tgt );
extern void bls12_381_G1_proj_from_affine ( const uint64_t *src , uint64_t *tgt );
extern void bls12_381_G1_proj_to_affine   ( const uint64_t *src , uint64_t *tgt );

extern uint8_t bls12_381_G1_proj_is_on_curve   ( const uint64_t *src );
extern uint8_t bls12_381_G1_proj_is_infinity   ( const uint64_t *src );
extern void    bls12_381_G1_proj_set_infinity  (       uint64_t *tgt );
extern uint8_t bls12_381_G1_proj_is_in_subgroup( const uint64_t *src );

extern uint8_t bls12_381_G1_proj_is_equal( const uint64_t *src1, const uint64_t *src2 );
extern uint8_t bls12_381_G1_proj_is_same ( const uint64_t *src1, const uint64_t *src2 );

extern void bls12_381_G1_proj_neg        ( const uint64_t *src ,       uint64_t *tgt );
extern void bls12_381_G1_proj_dbl        ( const uint64_t *src ,       uint64_t *tgt );
extern void bls12_381_G1_proj_add        ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );
extern void bls12_381_G1_proj_sub        ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );

extern void bls12_381_G1_proj_neg_inplace(       uint64_t *tgt );
extern void bls12_381_G1_proj_dbl_inplace(       uint64_t *tgt );
extern void bls12_381_G1_proj_add_inplace(       uint64_t *tgt , const uint64_t *src2 );
extern void bls12_381_G1_proj_sub_inplace(       uint64_t *tgt , const uint64_t *src2 );

extern void bls12_381_G1_proj_madd_proj_aff ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );
extern void bls12_381_G1_proj_madd_aff_proj ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );
extern void bls12_381_G1_proj_madd_inplace(         uint64_t *tgt , const uint64_t *src2 );

extern void bls12_381_G1_proj_scl_generic( const uint64_t *kst , const uint64_t *src , uint64_t *tgt , int kst_len );
extern void bls12_381_G1_proj_scl_Fr     ( const uint64_t *kst , const uint64_t *src , uint64_t *tgt );
extern void bls12_381_G1_proj_scl_big    ( const uint64_t *kst , const uint64_t *src , uint64_t *tgt );
extern void bls12_381_G1_proj_scl_small  (       uint64_t  kst , const uint64_t *src , uint64_t *tgt );

extern void bls12_381_G1_proj_scl_naive   ( const uint64_t *kst , const uint64_t *src , uint64_t *tgt , int kst_len );
extern void bls12_381_G1_proj_scl_windowed( const uint64_t *kst , const uint64_t *src , uint64_t *tgt , int kst_len );
