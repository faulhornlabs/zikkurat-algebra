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

if [ "$CABAL_MAJOR_VER" = "2" ]
then
  VX=""
else
  #VX="v1-"
  VX="v2-"
fi

cd ${ROOT}/codegen
echo "===================="
echo "building the codegen"
echo "===================="
#echo "current directory = `pwd`"
cabal ${VX}clean
cabal ${VX}build || error_exit "building the codegen failed"
if [ "$CABAL_MAJOR_VER" = "2" ]
then
  cabal ${VX}install --disable-documentation || error_exit "installing the codegen failed"
else
  # cabal install exe:zikkurat-algebra-codegen       --disable-documentation --force-reinstalls --overwrite-policy=always || error_exit "installing the codegen (exe) failed"
  # cabal install lib:zikkurat-algebra-codegen --lib --disable-documentation --force-reinstalls --overwrite-policy=always || error_exit "installing the codegen (lib) failed"
  cabal ${VX}install --disable-documentation --force-reinstalls --overwrite-policy=always || error_exit "installin gthe codegen failed"
fi

FLAGS=""

cd ${ROOT}/lib
echo "===================="
echo "building the library"
echo "===================="
#echo "current directory = `pwd`"
zikkurat-algebra-codegen all ${ROOT}/lib
cabal ${VX}clean              || error_exit "cleaning the lib failed"
cabal ${VX}configure ${FLAGS} || error_exit "configuring the lib failed"
cabal ${VX}build              || error_exit "building the lib failed"
cabal ${VX}haddock            || error_exit "haddocks for the lib failed"
if [ "$CABAL_MAJOR_VER" = "2" ]
then
  cabal ${VX}install            || error_exit "installing the lib failed"
else
  if [ "$VX" = "v1-" ]
  then
    cabal ${VX}install || error_exit "installing the lib failed"
  else
    cabal ${VX}install --lib lib:zikkurat-algebra || error_exit "installing the lib failed"
  fi
fi

cd ${ROOT}/test
echo "======================"
echo "building the testsuite"
echo "======================"
cabal ${VX}clean
cabal ${VX}configure
cabal ${VX}build || error_exit "building the test failed"

echo "OK!"
cd $ROOT
