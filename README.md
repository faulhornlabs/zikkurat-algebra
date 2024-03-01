
zikkurat-algebra
================

This is a Haskell / C library implementing algebraic primitives 
(finite fields, elliptic curves, polynomials) commonly used in zero-knowledge 
proof systems and related technologies.

The core idea is that we generate C code specialized to standard fields / curves;
and also Haskell bindings to this C code, presenting a proper API while retaining 
relatively good performance. Other high-level language bindings could be added 
in the future, if there is demand for that.

Project goals:

- nice, clean Haskell API, hopefully with good documentation
- portable among at least the commonly used 64-bit platforms
- performance within a 2x slowdown compared to state-of-the-art implementations
- multithreading support in Haskell (optionally in C too)
- standalone C library for easy interop with other languages
- no external dependencies
- comprehensive testing
- the code should stay simple enough (and documented enough) so that auditing 
  correctness wouldn't be a nightmarishly daunting task 
  (this one is very much not satisfied at the moment, as the current code generator 
  is _very_ hackish.)


Metadata
--------

copyright: (c) 2023-2024 Faulhorn Labs  
author: Balazs Komuves  
license: MIT or Apache-2.0 (at your choice)  
disclaimer: Extremely preliminary software

You are very welcome to experiment with this, but don't _yet_ use it for anything serious!


Project organization
--------------------

Sub-projects:

- `docs` - description of the algorithms
- `codegen` - the code generator
- `pure` - finite fields in pure Haskell (used in the codegen)
- `lib` - the Haskell library
- `lib/cbits` - the C library
- `test` - testing
- `examples` - examples of using the library
- `bench` - benchmarks (TODO)

The essential parts of the code are written in (generated) C, maybe with some assembly.
This C code (under `lib/cbits`) is self-contained, and can be also used without the Haskell bindings.

There is specialized code for each individual field and curve, and there is
also a (slow) generic Haskell reference implementation for testing and codegen 
purposes.


Supported primitives
--------------------

It's easy to add new fields or curves, just specify the required parameters.
Currently, we have the following ones.

### Supported elliptic curves

- Pairing-friendly curves:
    - BN128 (aka. alt-bn128, BN254, BN256, etc)
    - BLS12-381
    - ...
- General curves:
    - ...
- TODO:
    - Curve25519
    - secp256k1 / secq256k1
    - Pasta (Pallas / Vesta)
    - BLS12-377
    - ...

### Supported fields

All the base and scalar fields of the curves, the field extension towers required 
for pairing, plus:

- TODO:
    - some prime fields selected specifically for testing purposes
    - Goldilocks: `p = 2^64 - 2^32 + 1`
    - Mersenne-31
    - Baby Bear
    - binary fields; Wiedemann's binary tower
    - ...


Testing
-------

Given that the algorithms needed here are pretty complex, the optimizations can
be rather tricky, and there are a whole pyramid (a zikkurat!) of them, proper
testing is very important.

Our primary testing methods are:

- property-based testing
- unit tests, especially for possible corner cases - TODO
- compare against a very straightforward, high-level (but slow) reference implementation - partially done

In property-based testing we declare the expected properties of the functions,
things like for example commutativity and associativity of ring operations. 
Then we just test them on a large number of random inputs. A sufficiently big 
set of such properties gives a pretty good assurance, but since corner cases 
have a low probability to appear from random sampling, further "manual" testing 
of those is still necessary (TODO).

The test "framework" currently is a CLI executable, in which you can select the
subset of tests to run, and the number of random samples to run per test case 
(1000 by default).


TODO
----

- [x] implement bigints
- [x] implement prime fields
- [x] implement curves
- [x] implement univariate polynomials
- [x] implement NTT and iNTT
- [x] optimize the NTT by precalculating powers of the generator!
- [x] property-based test framework
- [ ] unit-test framework
- [ ] figure out nicer polymorphic API-s
- [x] vectors of field elements
- [ ] long division of bigints
- [x] faster Frobenius automorphism 
- [ ] square roots in prime fields 
- [ ] hash-to-curve & better (faster) random curve points  
- [ ] add benchmarking
- [x] implement field extensions
- [x] implement "G2" twisted curves (WIP)
- [x] implement pairings (TODO: make it faster)
- [ ] assembly routines (x86-64, arm64) for prime field multiplication
- [ ] figure out a better meta-programming story
- [x] add pure Haskell reference implementations (also used by the codegen)
- [ ] try to optimize a bit more
- [ ] add an explicit discrete logarithm type (integers modulo `p-1`)
- [ ] implement multivariate polynomials


Optimization opportunities
--------------------------

- implement fused multiply-and-add (and multiply-and-subtract) for prime fields
- implement addition and multiplication of prime fields in hand-written assembly
- deeper study of algorithmic tricks
- check out what others do (eg. constantine)
- lazy reduction: `a*b mod p + c*d mod p == (a*b + c*d) mod p`
- specialize for finite fields fitting into a single qword (but this is not relevant for elliptic curves)

Note: The main bottleneck for KZG-based proof systems is MSM.


Similar projects
----------------

You should also check out the following projects:

- [constantine](https://github.com/mratsim/constantine) - Nim/C library for pairing-based crypto
- [gnark-crypto](https://github.com/ConsenSys/gnark-crypto) - efficient cryptographic primitives in Go
- [arkworks](https://github.com/arkworks-rs) - Rust ecosystem for programming (zk-)SNARKs
- [mcl](https://github.com/herumi/mcl) - C++ library for pairing-based cryptography

These all have similar goals, with slightly different targets, tradeoffs,
programming languages and implementation details.



