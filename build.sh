#!/bin/bash

ROOT=`pwd`

error_exit() {
  echo " "
  echo "----------------------------------"
  echo "Error: $1"
  cd $ROOT
  exit 1
}

cd ${ROOT}/meta
echo "===================="
echo "building the codegen"
echo "===================="
#echo "current directory = `pwd`"
cabal install --disable-documentation || error_exit "building codegen failed"

cd ${ROOT}/lib
echo "===================="
echo "building the library"
echo "===================="
#echo "current directory = `pwd`"
cabal clean     || error_exit "cleaning the lib failed"
cabal configure || error_exit "configuring the lib failed"
cabal build     || error_exit "building the lib failed"
cabal haddock   || error_exit "haddocks for the lib failed"
cabal install   || error_exit "installing the lib failed"

echo "OK!"
cd $ROOT
