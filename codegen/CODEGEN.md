
The code generator
==================

This directory contains the code generator. Most of the library code is generated,
and specialized to the particular parameters (different finite fields, elliptic 
curves, etc).

We generate both the underlying C library and the high-level Haskell bindings.

We generate code at source code level. This is not the most elegant solution
(which would be something like staged compilation with two-level type theory),
however it is lightweight and language-agnostic, so it is an acceptable choice.

At the moment we generate the source code directly as Haskell strings 
(the "quick & dirty" method). This is a much less defendable choice, for many 
reasons; and the plan is to replace it with a more principled solution, also 
with better developer experience. However on the short term the priority is 
getting the core functionality working, so the library can be used in its 
intended applications.

The C library
-------------

Organization of the generated C source files:

    /cbits/bigint/                   - unsigned integers of various bit sizes
    /cbits/curves/fields/std/        - base and scalar fields of curves, standard repr.
    /cbits/curves/fields/mont/       - base and scalar fields of curves, Montogomery repr.
    /cbits/curves/g1/affine/         - G1 curve points, affine coordinates
    /cbits/curves/g1/proj/           - G1 curve points, projective coordiates
    /cbits/curves/g1/jac/            - G1 curve points, Jacobian coordinates
    /cbits/curves/g1/poly/mont/      - polynomials with coefficients in the scalar field

