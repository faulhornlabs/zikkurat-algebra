#include <stdint.h>

extern void bls12_381_G1_jac_normalize         ( const uint64_t *src , uint64_t *tgt );
extern void bls12_381_G1_jac_normalize_inplace (       uint64_t *tgt );

extern void bls12_381_G1_jac_copy        ( const uint64_t *src , uint64_t *tgt );
extern void bls12_381_G1_jac_from_affine ( const uint64_t *src , uint64_t *tgt );
extern void bls12_381_G1_jac_to_affine   ( const uint64_t *src , uint64_t *tgt );

extern uint8_t bls12_381_G1_jac_is_on_curve   ( const uint64_t *src );
extern uint8_t bls12_381_G1_jac_is_infinity   ( const uint64_t *src );
extern void    bls12_381_G1_jac_set_infinity  (       uint64_t *tgt );
extern uint8_t bls12_381_G1_jac_is_in_subgroup( const uint64_t *src );

extern uint8_t bls12_381_G1_jac_is_equal( const uint64_t *src1, const uint64_t *src2 );
extern uint8_t bls12_381_G1_jac_is_same ( const uint64_t *src1, const uint64_t *src2 );

extern void bls12_381_G1_jac_neg        ( const uint64_t *src ,       uint64_t *tgt );
extern void bls12_381_G1_jac_dbl        ( const uint64_t *src ,       uint64_t *tgt );
extern void bls12_381_G1_jac_add        ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );
extern void bls12_381_G1_jac_sub        ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );

extern void bls12_381_G1_jac_neg_inplace(       uint64_t *tgt );
extern void bls12_381_G1_jac_dbl_inplace(       uint64_t *tgt );
extern void bls12_381_G1_jac_add_inplace(       uint64_t *tgt , const uint64_t *src2 );
extern void bls12_381_G1_jac_sub_inplace(       uint64_t *tgt , const uint64_t *src2 );

extern void bls12_381_G1_jac_madd_jac_aff ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );
extern void bls12_381_G1_jac_madd_aff_jac ( const uint64_t *src1, const uint64_t *src2, uint64_t *tgt );
extern void bls12_381_G1_jac_madd_inplace (       uint64_t *tgt , const uint64_t *src2 );

extern void bls12_381_G1_jac_scl_generic( const uint64_t *kst , const uint64_t *src , uint64_t *tgt , int kst_len );
extern void bls12_381_G1_jac_scl_Fr_std ( const uint64_t *kst , const uint64_t *src , uint64_t *tgt );
extern void bls12_381_G1_jac_scl_Fr_mont( const uint64_t *kst , const uint64_t *src , uint64_t *tgt );
extern void bls12_381_G1_jac_scl_big    ( const uint64_t *kst , const uint64_t *src , uint64_t *tgt );
extern void bls12_381_G1_jac_scl_small  (       uint64_t  kst , const uint64_t *src , uint64_t *tgt );

extern void bls12_381_G1_jac_scl_naive   ( const uint64_t *kst , const uint64_t *src , uint64_t *tgt , int kst_len );
extern void bls12_381_G1_jac_scl_windowed( const uint64_t *kst , const uint64_t *src , uint64_t *tgt , int kst_len );

extern void bls12_381_G1_jac_MSM_std_coeff_jac_out(int npoints, const uint64_t *expos, const uint64_t *grps, uint64_t *tgt, int expo_nlimbs);
extern void bls12_381_G1_jac_MSM_mont_coeff_jac_out(int npoints, const uint64_t *expos, const uint64_t *grps, uint64_t *tgt, int expo_nlimbs);
extern void bls12_381_G1_jac_MSM_std_coeff_affine_out (int npoints, const uint64_t *expos, const uint64_t *grps, uint64_t *tgt, int expo_nlimbs);
extern void bls12_381_G1_jac_MSM_mont_coeff_affine_out(int npoints, const uint64_t *expos, const uint64_t *grps, uint64_t *tgt, int expo_nlimbs);
extern void bls12_381_G1_jac_MSM_std_coeff_jacc_out_slow_reference(int npoints, const uint64_t *expos, const uint64_t *grps, uint64_t *tgt, int expo_nlimbs);
extern void bls12_381_G1_jac_fft_forward( int m, const uint64_t *gen, const uint64_t *src, uint64_t *tgt );
extern void bls12_381_G1_jac_fft_inverse( int m, const uint64_t *gen, const uint64_t *src, uint64_t *tgt );
