#include <stdint.h>

void bls12_381_pairing_affine    (const uint64_t *P, const uint64_t *Q, uint64_t *tgt);
void bls12_381_pairing_projective(const uint64_t *P, const uint64_t *Q, uint64_t *tgt);

// for testing purposes:
void bls12_381_pairing_psi        (const uint64_t *src, uint64_t *tgt);
void bls12_381_pairing_inverse_psi(const uint64_t *src, uint64_t *tgt);
void bls12_381_pairing_final_expo (const uint64_t *src, uint64_t *tgt);
void bls12_381_pairing_hard_expo  (const uint64_t *src, uint64_t *tgt);
void bls12_381_pairing_miller_loop(const uint64_t *P, const uint64_t *Q, uint64_t *out_T, uint64_t *out_f );
