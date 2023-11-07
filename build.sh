#!/bin/bash

ROOT=`pwd`

error_exit() {
  echo " "
  echo "----------------------------------"
  echo "Error: $1"
  cd $ROOT
  exit 1
}

CABAL_MAJOR_VER=`cabal --numeric-version | head -c 1`
echo "cabal major version = $CABAL_MAJOR_VER"

cd ${ROOT}/codegen
echo "===================="
echo "building the codegen"
echo "===================="
#echo "current directory = `pwd`"
if [ "$CABAL_MAJOR_VER" = "2" ]
then
  cabal install --disable-documentation || error_exit "building the codegen failed"
else
  cabal install --disable-documentation --overwrite-policy=always || error_exit "building the codegen failed"
fi

FLAGS=""

cd ${ROOT}/lib
echo "===================="
echo "building the library"
echo "===================="
#echo "current directory = `pwd`"
cabal clean              || error_exit "cleaning the lib failed"
cabal configure ${FLAGS} || error_exit "configuring the lib failed"
cabal build              || error_exit "building the lib failed"
cabal haddock            || error_exit "haddocks for the lib failed"
cabal install            || error_exit "installing the lib failed"

cd ${ROOT}/test
echo "======================"
echo "building the testsuite"
echo "======================"
cabal clean
cabal configure
cabal build || error_exit "building the test failed"

echo "OK!"
cd $ROOT
