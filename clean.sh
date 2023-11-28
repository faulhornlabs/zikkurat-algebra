#!/bin/bash

ROOT=`pwd`

cd ${ROOT}/codegen
cabal clean

cd ${ROOT}/lib
cabal clean

cd ${ROOT}/test
cabal clean

echo "OK!"
cd $ROOT
