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
  VX="v1-"
  #VX="v2-"
fi
echo "cabal v1 or v2 flavour = \`$VX\`"

cd ${ROOT}/test
echo "======================"
echo "building the testsuite"
echo "======================"
# cabal ${VX}clean
cabal ${VX}configure
cabal ${VX}build || error_exit "building the test failed"
echo "building the testsuit OK!"
cd $ROOT

#---------------------------------------

# old-style cabal commands (V1)
EXE="./test/dist/build/zikkurat-algebra-tests/zikkurat-algebra-tests"

ls -la $EXE
echo ""

$EXE $@

