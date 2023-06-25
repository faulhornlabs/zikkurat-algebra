#include <stdint.h>

extern void bls12_381_G1_normalize         ( const uint64_t *src , uint64_t *tgt );
extern void bls12_381_G1_normalize_inplace (       uint64_t *tgt );

extern void bls12_381_G1_copy     ( const uint64_t *src , uint64_t *tgt );
extern void bls12_381_G1_from_aff ( const uint64_t *src , uint64_t *tgt );
extern void bls12_381_G1_to_aff   ( const uint64_t *src , uint64_t *tgt );

extern uint8_t bls12_381_G1_is_on_curve   ( const uint64_t *src );
extern uint8_t bls12_381_G1_is_infinity   ( const uint64_t *src );
extern void    bls12_381_G1_set_infinity  (       uint64_t *tgt );
extern uint8_t bls12_381_G1_is_in_subgroup( const uint64_t *src );

extern void bls12_381_G1_neg        ( const uint64_t *src ,       uint64_t *tgt );
extern void bls12_381_G1_dbl        ( const uint64_t *src ,       uint64_t *tgt );
extern void bls12_381_G1_add        ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );
extern void bls12_381_G1_sub        ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );

extern void bls12_381_G1_neg_inplace(       uint64_t *tgt );
extern void bls12_381_G1_dbl_inplace(       uint64_t *tgt );
extern void bls12_381_G1_add_inplace(       uint64_t *tgt , const uint64_t *src2 );
extern void bls12_381_G1_sub_inplace(       uint64_t *tgt , const uint64_t *src2 );

extern void bls12_381_G1_madd_proj_aff ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );
extern void bls12_381_G1_madd_aff_proj ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );
extern void bls12_381_G1_madd_inplace(         uint64_t *tgt , const uint64_t *src2 );

extern void bls12_381_G1_scl_Fp     ( const uint64_t *kst , const uint64_t *src , uint64_t *tgt );
extern void bls12_381_G1_scl_Fr     ( const uint64_t *kst , const uint64_t *src , uint64_t *tgt );

extern void bls12_381_G1_scl_naive   ( const uint64_t *kst , const uint64_t *src , uint64_t *tgt , int kst_len );
extern void bls12_381_G1_scl_windowed( const uint64_t *kst , const uint64_t *src , uint64_t *tgt , int kst_len );
