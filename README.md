
zikkurat-algebra
================

This is a Haskell / C library implementing algebraic primitives 
(finite fields, elliptic curves, polynomials) commonly used in zero-knowledge 
proof systems and related technologies.

The core idea is that we generate C code specialized to standard fields / curve;
and also Haskell bindings to this C code, presenting a proper API while retaining 
relatively good performance. Other high-level language bindings could be added 
in the future if there is demand.

Project goals:

- nice, clean Haskell API, hopefully with good documentation
- portable among at least the commonly used 64-bit platforms
- performance within a 4x (preferably 2x) slowdown compared to state-of-the-art implementations
- multithreading support in Haskell (but not in C)
- standalone C library for easy interop with other languages
- no external dependencies
- comprehensive testing
- the code should stay simple enough (and documented enough) so that auditing 
  correctness wouldn't be a nightmarishly daunting task 
  (this one is very much not satisfied at the moment, as the code generator is very hackish)


Metadata
--------

copyright: (c) 2023 Faulhorn Labs  
author: Balazs Komuves  
license: MIT or Apache-2.0 (at your choice)  
disclaimer: Extremely preliminary software

You are very welcome to experiment with this, but don't _yet_ use it for anything serious!


Project organization
--------------------

Sub-projects:

- `docs` - description of the algorithms
- `codegen` - the code generator
- `lib` - the Haskell library
- `test` - testing
- `examples` - examples of using the library
- `bench` - benchmarks (TODO)

The essential parts of the code are written in (generated) C, maybe with some assembly.
This C code (under `lib/cbits`) is self-contained, and can be also used without the Haskell bindings.

There is specialized code for each individual field and curve, and also
a generic Haskell reference implementation for testing purposes.


Supported primitives
--------------------

It's very easy to add new fields or curves, just specify the required parameters.
Currently, we have the following ones.

### Supported elliptic curves

- Pairing-friendly curves:
    - BN128 (aka. alt-bn128, BN254, BN256, etc)
    - BLS12-381
    - ...
- General curves:
    - ...
- TODO:
    - secp256k1 / secq256k1
    - Pasta (Pallas / Vesta)
    - ..

### Supported fields

All the base and scalar fields of the curves, plus:

- some prime fields selected specifically for testing purposes
- TODO:
    - the field extensions required for pairing
    - Goldilocks: `p = 2^64 - 2^32 + 1`
    - ...


TODO
----

- [x] implement bigints
- [x] implement prime fields
- [x] implement curves
- [ ] implement univariate polynomials
- [x] property-based test framework
- [ ] unit-test framework
- [ ] long division of bigints
- [ ] square roots in prime fields 
- [ ] hash-to-curve & better random curve points  
- [ ] add benchmarking
- [ ] figure out a better meta-programming story
- [ ] add pure Haskell reference implementations
- [ ] try to optimize a bit more
- [ ] add an explicit discrete logarithm type (integers modulo `p-1`)
- [ ] implement field extensions
- [ ] implement pairings
- [ ] implement multivariate polynomials


Optimization opportunities
--------------------------

- implement fused multiply-and-add for prime fields
- implement addition and multiplication of prime fields directly in assembly
- deeper study of algorithmic tricks
- check out what others do (eg. constantine)
- specialize for finite fields fitting into a single qword (but this is not relevant for elliptic curves)

Note: The main bottleneck for KZG-based proof systems is MSM.


Similar projects
----------------

You should also check out the following projects:

- [constantine](https://github.com/mratsim/constantine) - Nim/C library for pairing-based crypto
- [gnark-crypto](https://github.com/ConsenSys/gnark-crypto) - efficient cryptographic primitives in Go
- [arkworks](https://github.com/arkworks-rs) - Rust ecosystem for programming (zk-)SNARKs
- [mcl](https://github.com/herumi/mcl) - C++ library for pairing-based cryptography

These all have similar goals, with slightly different targets, tradeoffs 
and implementation details.



