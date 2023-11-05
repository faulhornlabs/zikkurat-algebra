
/*

Poseidon2 arithmetic hash function, specialized to the BN128 scalar field and t=3

compile with: 

gcc -O3 \
  -I ../lib/cbits/bigint/             \
  -I ../lib/cbits/curves/fields/std/  \
  -I ../lib/cbits/curves/fields/mont/ \
  ../lib/cbits/bigint/bigint256.c     \
  ../lib/cbits/curves/fields/std/bn128_r_std.c    \
  ../lib/cbits/curves/fields/mont/bn128_r_mont.c  \
  poseidon2.c

*/

#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include "../lib/cbits/curves/fields/std/bn128_r_std.h"
#include "../lib/cbits/curves/fields/mont/bn128_r_mont.h"

#define NLIMBS 4

// -----------------------------------------------------------------------------

uint64_t initial_round_consts_mont[3*NLIMBS*4] = 
  { 0xb1693d8b42f2eba6, 0x4e710def5d24cf7a, 0xa1a5d0871fd70184, 0x2a77f249c2d97c26
  , 0x4113be97bb4b16d5, 0x026c0a64ce0b9abb, 0xecb64fde9adb21de, 0x0d4b23343bf15eac
  , 0xa9c5a16ab0b90473, 0x8627e0b6af146e12, 0xcb2d10c7294d77b2, 0x0e39626a47cba8ce
  , 0x8a269385456563d6, 0x88fddd3204fb1419, 0x0067fdf8dea1e4a6, 0x01bee8f9bb0af388
  , 0x40db6d56ff6c7fb8, 0x2afa79e5b497efc0, 0xdff3fb1baab04c64, 0x2b07d5f54efb8a54
  , 0x5bb104e3b3f647bb, 0xa8be0ae17140ea02, 0x683c9003cd1340c6, 0x1371e26f8ad068be
  , 0x2c198e385e27d01b, 0x166e6e2e2490a615, 0x4de0ab9c684fdff7, 0x29380ed4c1681989
  , 0x762c20829b525a24, 0x966ad8cb8ef323c6, 0x2effe12abdb6627e, 0x27cfd6db30659173
  , 0x885bbc0e4902b834, 0x4c2db387f2af370d, 0x464fad96b2acf5e5, 0x00cf858a0b08c5a3
  , 0xb79b8afecdc0c61e, 0x5447ea21c2cd824b, 0x67e92fdabca8be19, 0x1f745966776f72cb
  , 0x5a35908b5b07d576, 0x810b6dc1f3073940, 0xe5bb171079410b8a, 0x297ee48bbe42336f
  , 0xb089556e230bcbc0, 0x0c35d1666688210d, 0x2157524f64c571dd, 0x038f08429bd404b0
  };

uint64_t internal_round_consts_mont[56*NLIMBS] = 
  { 0x729f1c5c4e85dcf6, 0x06667b095a76e5d3, 0x46a5588adb3d1b44, 0x27618829e52473ed
  , 0x9053a8bdb24557d5, 0x358fdd39005d8c3e, 0x5a35c313f07751a2, 0x06d6fdf35a6de168
  , 0x26d2e78b29e8e6ce, 0x7cd791ee6c9e0a2f, 0xb015455b4c3cb659, 0x295b508ddb8a6817
  , 0xfb912af2b3d34fdc, 0xaa015e4fb4fbb04d, 0x8e5eaefbd4591107, 0x0b4fc2accdd5fb63
  , 0x5bc7428137e5d9da, 0x195d5f38f9889a67, 0x586423d68deba8f3, 0x11207ea4c3306561
  , 0x24bf27a18efbe8fe, 0xb1f9708f6cdc20ce, 0x5071e9fb6885d989, 0x2dd4f2b6f74dd628
  , 0x40d106494cf1ae27, 0x35fb17f488718136, 0xa972484d36b7f085, 0x29d25adc5b8fe63f
  , 0x7d712ad1f4e01b53, 0x144d389561be8b06, 0x50da584243a12106, 0x0e2b8e591bcac09c
  , 0x869a28e08d10763d, 0xb344842c63b04a7e, 0x489b6d70c58ad69b, 0x14f5dbcf5244be8f
  , 0x53f4521029438892, 0x9cbaf24bd6807018, 0xc58a3378d8357056, 0x12022bb3dc016da5
  , 0x9e235cd003da2309, 0x1a2883851c9798e2, 0x15278dc6949390f7, 0x2d8b2b264bc31467
  , 0x19200b9d015e6065, 0x13fbe131867e6cb0, 0xb5b9fbd11a1a5ae5, 0x0683e215c13f2685
  , 0x4b00a737c66ae350, 0x1d6288eb023510b7, 0xa042aa850ad9e0d3, 0x1fb234d58d52592d
  , 0x378a414c775a1ff8, 0x30a41e1a7224f0c5, 0xe3eb0de4ed1889c7, 0x18204127010f600b
  , 0x88325bd1e13fe6b3, 0xb7f3106aea53514f, 0x36a3f6ea07b3acc0, 0x243e682ddbb93bb6
  , 0x46cae2de13f33082, 0x01f6c56d28693f33, 0x3823d11b74c36e9e, 0x05d0a79dd29d75d2
  , 0xed4414eb660ea9c1, 0x9253e621359d662a, 0xe16deb4f9102952b, 0x2c262cad68c10b3f
  , 0xbc548b8935c38757, 0xcae285ce91f24c08, 0x67d48bde46e16208, 0x274b6e3e4de131ff
  , 0x90ee94fe889fd818, 0xaf41e23dfe2c52c2, 0xc2816b364e76a144, 0x0e8588229b41c593
  , 0x6a7f5a747449dcda, 0x4f7bb322f7d3b716, 0x380fd5791f2097bc, 0x186a1781bd1fcb2f
  , 0xfcc5c1f29078f1ce, 0x31cbb27530c3444d, 0x27f91d4926cef6dd, 0x05f2609e202f6b0d
  , 0x0735e6beaa5f49a5, 0x64a7594068742f0a, 0x63d385e6bdf54442, 0x008cc8fd2c7b637a
  , 0x0d374d31abc6c53e, 0x7e33771fe33e9602, 0x587591c24fe2dd44, 0x1bad829c38d3ff8c
  , 0xab825c95b43a9a03, 0x5865c9bac9d19c2e, 0x914fb969aac6d44b, 0x1775bc473a336ae5
  , 0x54708f63b5d34d95, 0xca75c6a116969889, 0x296328e0faed5abc, 0x09f43898d888067b
  , 0x9165580f03a4fcc8, 0x1cf68ff4d3c74f09, 0x7cb52f36be129fc9, 0x178e28f49c7a64e5
  , 0x69cb5a58c7bd773d, 0x5f9f5d92b12dde2c, 0x4b7c55d0994e0c37, 0x2a40f5674cf31cc3
  , 0x060e8daee5d5f6ed, 0x2e8db620c58e69be, 0xa8c9c67a90340c10, 0x1a0d23bba1f1d861
  , 0x0a8747ac3f80926c, 0xef7aecd9ae231ec4, 0x66d0278798ef4751, 0x2d4781b3af412bf6
  , 0x89bdf2205eff0df0, 0x8e272242a9a41667, 0xe85f0a25cf3b0781, 0x0802c22d41196e63
  , 0xc28bf0117c9dc2bd, 0x11b173483063e6bb, 0x35086d2cbbd0dce3, 0x2fbbe460e2ba94ec
  , 0xbfd77e5d5e639781, 0x33db73c0db93b9f6, 0x67a35257f0cd66e9, 0x0c21091dcb5f1ba8
  , 0x5697e4ce1227b9ec, 0x607d8b75030cf103, 0x8e383a200f44d7eb, 0x228c269122bcc119
  , 0x2eb59d63af607ea4, 0x34dd935271645cac, 0xca7a232bba5f377e, 0x1171a34556c9ef33
  , 0x8410529f3bc791f0, 0xaa2ba025784f8bcd, 0xf3e454b7f1b2a126, 0x0d8a3e8dddb48756
  , 0xa6794d17374c0135, 0x4ff5f7d6ba34332b, 0x6244c6e825dbd35f, 0x0c1e12ce6cae2101
  , 0xe97193e0a142867f, 0x09a5e3cf40ab67b6, 0x260f9192bcd9a799, 0x07bdf64c500b09c6
  , 0xe3cde7b1d02d73c0, 0x43f04876884bea23, 0x139e1dc6e7d62636, 0x1d3207f48566b5a4
  , 0x169d28f39078d343, 0xd051990b39d28bff, 0xef51679c6e1ef5c0, 0x2c8dbb8fff85342f
  , 0x432125d89d65a6a0, 0x5091bfebceca5494, 0x523ea9a7c6bff508, 0x14b8f6c7122ffd5d
  , 0x5e2b58000f661354, 0x5f189927dee65357, 0x99ae4e3938ae6a38, 0x2b264b2d79209fea
  , 0x3f3b759c091da1a1, 0x0cc5191238ff8753, 0xa892fb0258d74074, 0x0059a513c1970630
  , 0x593bf9f539e70aab, 0x14eac71046b83fc9, 0x314170c8480ce495, 0x1f4c5aa0b68e88b1
  , 0x62c702ee603ac0a1, 0x9e4958b5e45ed01c, 0x599fdeeb43a59c00, 0x1dff5d33d1983b75
  , 0xea6e9f09f4590cf5, 0x8a53e0411a6bdd7d, 0xff35a7d5e78b5c0e, 0x0e97922294e519d2
  , 0xadc4f723932c18ce, 0xeacad5d86752fa16, 0xf2329e87153509b8, 0x1bcbd0ddd411c58a
  , 0x960c7957aee9b40c, 0xd16b74b7d8cfb6db, 0xbb2907fe2eb831ed, 0x0a035e622e500b1d
  , 0x50b32396b4d3344e, 0xfa812c383b0bbb10, 0x6a21a78038b477f7, 0x1fdb5f78c04870ec
  , 0xa6671bb2e2fefd97, 0x7bd7afc1da4b3dbf, 0x949a22d88dc8411d, 0x2ee97af4568ec5db
  , 0x5f61c941f244590a, 0xb3f32aed6eb8ea6b, 0x56d2e58c72243faa, 0x19b0535c25ccd958
  , 0x3106c4260905e5a5, 0xe3fa20e5d6812a37, 0xc51acc1fa1e45ea7, 0x0692d3550cbdf2e8
  , 0x0ae878df7b628f44, 0x2087612c9abcf165, 0x6219b88740836a64, 0x2b4ab9eeb448ee3f
  , 0xce8792457d95d508, 0x71b237fb461e3a20, 0x964575970381ac2f, 0x0d3454a346763400
  , 0x997317408a42ab95, 0x94b452e5b078aff7, 0x03203a9659ce0efc, 0x24b7b333af4fd155
  , 0xb39b95a7bde1a9cf, 0x2c83ef5926e1cc78, 0x50da60af0adc23e3, 0x101564cced395e24
  , 0x03ef1324be1d1a1d, 0x3e083c44e59f8484, 0x630822adcbd0a743, 0x0fc929ed6e18d09b
  };

uint64_t final_round_consts_mont[3*NLIMBS*4] = 
  { 0x634471eb627906b6, 0x95dffe115e77842e, 0x1ba3cd39a61da352, 0x101f3e205edc80c0
  , 0x105d4377652121dc, 0xa1159e8bcc8f0cd3, 0x3b5ed230e9202b85, 0x24a9197d30b81917
  , 0xdfa08515bb925f72, 0xd50c4c330ef56f8b, 0xfb42bcb84c7d553f, 0x1ea8c492befffe9a
  , 0x1343b3e5aaa92bd3, 0xed59870d085e5cdb, 0x507a7731a2de8f0c, 0x27a91fea0faac518
  , 0x2d5631a1e4bea4e1, 0x98932af561b55cdf, 0x5541d4c1666118e8, 0x144a795d161724ac
  , 0x3c881f2138bcb906, 0xf18afe5c074edd94, 0xbad2ff323b8b2895, 0x158ddd4a5e5e58ed
  , 0x4450fbc96b8c863a, 0x7252f88d31caef53, 0xdd88b5e7d4b917ed, 0x121eca65a8cc153a
  , 0x0d19a3b3e0ac97fe, 0x055b50ce8f402f80, 0xde71b42cc57db1d6, 0x152ff9ae3733da62
  , 0x86d149768173614d, 0x2a322196c2cfd7ef, 0x2b71ba22c51e0667, 0x111972f4cc03d67b
  , 0x2e1bcba5c9e8fef0, 0xa92062ff3d69be22, 0x75d3016071696c8e, 0x203e6d54bbeaf53c
  , 0xaa1dae06ecaf1bb4, 0x18722e478d7c02d3, 0x6e0cf8d4b89c1922, 0x0f1b20d3adcef941
  , 0x1ab6495728ed5623, 0xe0b4f851f6e7df29, 0x138c02b95f096ec5, 0x2602b7a1f82ea12e
  };

// -----------------------------------------------------------------------------

// x -> x^5
void sbox_mont( const uint64_t *src, uint64_t *tgt) {
  uint64_t tmp[NLIMBS];
  bn128_r_mont_sqr( src, tmp );
  bn128_r_mont_sqr_inplace( tmp );
  bn128_r_mont_mul( src, tmp, tgt );
}

void sbox_inplace_mont( uint64_t *tgt ) {
  sbox_mont( tgt, tgt );
}

// the Poseidon2 permutation
// input/output: t=3 field elements, in Montgomery format
void poseidon_perm_mont( const uint64_t *src, uint64_t *tgt) {

  uint64_t state[3*NLIMBS];
  uint64_t tmp[NLIMBS];

  // linear layer
  // (x,y,z) -> (x+s, y+s, z+s) where s = x+y+z
  bn128_r_mont_add        ( src , src +   NLIMBS , tmp );
  bn128_r_mont_add_inplace( tmp , src + 2*NLIMBS  );
  bn128_r_mont_add( src            , tmp, state            );
  bn128_r_mont_add( src +   NLIMBS , tmp, state +   NLIMBS );
  bn128_r_mont_add( src + 2*NLIMBS , tmp, state + 2*NLIMBS );

  // initial external rounds
  for(int k=0; k<4; k++) {
    bn128_r_mont_add_inplace( state            , initial_round_consts_mont + (3*k  )*NLIMBS );
    bn128_r_mont_add_inplace( state +   NLIMBS , initial_round_consts_mont + (3*k+1)*NLIMBS );
    bn128_r_mont_add_inplace( state + 2*NLIMBS , initial_round_consts_mont + (3*k+2)*NLIMBS );
    sbox_inplace_mont( state            );
    sbox_inplace_mont( state +   NLIMBS );
    sbox_inplace_mont( state + 2*NLIMBS );
    // compute s = x' + y' + z'
    bn128_r_mont_add        ( state, state +   NLIMBS , tmp );
    bn128_r_mont_add_inplace( tmp  , state + 2*NLIMBS  );
    // add s to each component
    bn128_r_mont_add_inplace( state            , tmp );
    bn128_r_mont_add_inplace( state +   NLIMBS , tmp );
    bn128_r_mont_add_inplace( state + 2*NLIMBS , tmp );
  }

  // internal rounds
  for(int k=0; k<56; k++) {
    bn128_r_mont_add_inplace( state , internal_round_consts_mont + k*NLIMBS );
    sbox_inplace_mont( state );
    // compute s = x' + y + z
    bn128_r_mont_add        ( state, state +   NLIMBS , tmp );
    bn128_r_mont_add_inplace( tmp  , state + 2*NLIMBS  );
    // double z
    bn128_r_mont_add_inplace( state + 2*NLIMBS , state + 2*NLIMBS );
    // add s to each component
    bn128_r_mont_add_inplace( state            , tmp );
    bn128_r_mont_add_inplace( state +   NLIMBS , tmp );
    bn128_r_mont_add_inplace( state + 2*NLIMBS , tmp );
  }

  // final external rounds
  for(int k=0; k<4; k++) {
    bn128_r_mont_add_inplace( state            , final_round_consts_mont + (3*k  )*NLIMBS );
    bn128_r_mont_add_inplace( state +   NLIMBS , final_round_consts_mont + (3*k+1)*NLIMBS );
    bn128_r_mont_add_inplace( state + 2*NLIMBS , final_round_consts_mont + (3*k+2)*NLIMBS );
    sbox_inplace_mont( state            );
    sbox_inplace_mont( state +   NLIMBS );
    sbox_inplace_mont( state + 2*NLIMBS );
    // compute s = x' + y' + z'
    bn128_r_mont_add        ( state, state +   NLIMBS , tmp );
    bn128_r_mont_add_inplace( tmp  , state + 2*NLIMBS  );
    // add s to each component
    bn128_r_mont_add_inplace( state            , tmp );
    bn128_r_mont_add_inplace( state +   NLIMBS , tmp );
    bn128_r_mont_add_inplace( state + 2*NLIMBS , tmp );
  }

  memcpy( tgt , state , 3*8*NLIMBS );
}

// input/output: t=3 field elements, in standard format
void poseidon_perm_std( const uint64_t *src, uint64_t *tgt) {
  uint64_t state[3*NLIMBS];
  bn128_r_mont_from_std( src            , state            );
  bn128_r_mont_from_std( src +   NLIMBS , state +   NLIMBS );
  bn128_r_mont_from_std( src + 2*NLIMBS , state + 2*NLIMBS );
  poseidon_perm_mont( state, state );
  bn128_r_mont_to_std( state            , tgt            );
  bn128_r_mont_to_std( state +   NLIMBS , tgt +   NLIMBS );
  bn128_r_mont_to_std( state + 2*NLIMBS , tgt + 2*NLIMBS );
}

// -----------------------------------------------------------------------------

// compress two field elements (Montgomery) into one 
// f(x,y) := perm(x,y,0)[0]
void poseidon_compression_mont( const uint64_t *src, uint64_t *tgt ) {
  uint64_t tmp[NLIMBS*3];

  bn128_r_mont_copy     ( src          , tmp            );
  bn128_r_mont_copy     ( src + NLIMBS , tmp +   NLIMBS );
  bn128_r_mont_set_zero (                tmp + 2*NLIMBS );

  poseidon_perm_mont( tmp ,tmp );

  bn128_r_mont_copy( tmp, tgt );
}

// compute the Merkle root of a vector 2^n field elements (Montgomery repr.)
void poseidon_merkle_mont( int n, const uint64_t *src, uint64_t *tgt) {
  int N = (1<<n);
  
  uint64_t *layers = malloc( (N-1)*8*NLIMBS );
  assert( layers != 0 );

  const uint64_t *p = src;
  uint64_t       *q = layers;
  for(int k=0; k<n; k++) {
    int M = (1<<(n-1-k));
    for(int j=0; j<M; j++) {
      poseidon_compression_mont( p + 2*j*NLIMBS , q + j*NLIMBS );
    }
    p =  q;
    q += M*NLIMBS;
  }

  bn128_r_mont_copy( p , tgt );

  free(layers);
}

// the Poseidon2 permutation
// compute the Merkle root of a vector 2^n field elements (standard repr.) 
void poseidon_merkle_std( int n, const uint64_t *src, uint64_t *tgt) {

  int N = (1<<n);
  uint64_t *mont_input = malloc( N*8*NLIMBS );
  assert( mont_input != 0 );

  for(int j=0; j<N; j++) {
    bn128_r_mont_from_std( src + j*NLIMBS , mont_input + j*NLIMBS );
  }

  uint64_t mont_root[NLIMBS];
  poseidon_merkle_mont( n , mont_input , mont_root );
  bn128_r_mont_to_std( mont_root, tgt );

  free(mont_input);
}

// -----------------------------------------------------------------------------

void print_std( const char *prefix, const uint64_t *src ) {
  printf("%s0x%016llx%016llx%016llx%016llx\n", prefix , src[3], src[2], src[1], src[0] ); 
}

// -----------------------------------------------------------------------------

uint64_t example_input_std[3*NLIMBS] = 
  { 0 , 0 , 0 , 0
  , 1 , 0 , 0 , 0  
  , 2 , 0 , 0 , 0
  };

void permutation_example() {
  uint64_t tgt[3*NLIMBS];

  printf("\npermutation of (0,1,2) in F^3:\n");
  poseidon_perm_std( example_input_std , tgt );
  print_std( "  u = ", tgt            );
  print_std( "  v = ", tgt +   NLIMBS );
  print_std( "  w = ", tgt + 2*NLIMBS );
}

// -----------------------------------------------------------------------------

void merkle_root_example(int n) {
  int N = (1<<n);

  uint64_t *input = malloc( 8*N*NLIMBS );
  assert( input != 0);

  for(int j=0; j<N; j++) {
    bn128_r_std_set_zero( input + j*NLIMBS );
    input[j*NLIMBS] = j+1;
  }
  uint64_t root[NLIMBS];
  poseidon_merkle_std( n, input, root );
  printf("\nMerkle root of the array [1..%d]:\n",N);
  print_std( "  root = ", root );

  free(input);
}

// -----------------------------------------------------------------------------

int main() {
  
//  permutation_example();
//
//  merkle_root_example(3);
//  merkle_root_example(4);
//  merkle_root_example(5);
//  merkle_root_example(8);
  merkle_root_example(16);

  printf("\n");
}

