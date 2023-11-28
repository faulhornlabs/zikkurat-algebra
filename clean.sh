#!/bin/bash

ROOT=`pwd`

cd ${ROOT}/pure
cabal clean

cd ${ROOT}/codegen
cabal clean

cd ${ROOT}/lib
cabal clean
rm -rf ${ROOT}/lib/cbits
rm -rf ${ROOT}/lib/src/ZK/Algebra/Curves
rm -rf ${ROOT}/lib/src/ZK/Algebra/BigInt

cd ${ROOT}/test
cabal clean

echo "OK!"
cd $ROOT
