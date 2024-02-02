#include <stdint.h>

void bn128_pairing_affine    (const uint64_t *P, const uint64_t *Q, uint64_t *tgt);
void bn128_pairing_projective(const uint64_t *P, const uint64_t *Q, uint64_t *tgt);

// for testing purposes:
void bn128_pairing_psi        (const uint64_t *src, uint64_t *tgt);
void bn128_pairing_inverse_psi(const uint64_t *src, uint64_t *tgt);
void bn128_pairing_final_expo (const uint64_t *src, uint64_t *tgt);
void bn128_pairing_hard_expo  (const uint64_t *src, uint64_t *tgt);
void bn128_pairing_miller_loop(const uint64_t *P, const uint64_t *Q, uint64_t *out_T, uint64_t *out_f );
