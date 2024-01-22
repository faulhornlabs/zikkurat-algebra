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

if [ "$VX" = "v2-" ]
then
  OPT_HYPERLINK="--haddock-hyperlink-source"
  OPT_REINSTALL="--force-reinstalls --overwrite-policy=always"
else
  OPT_HYPERLINK="--hyperlink-source"
  OPT_REINSTALL="--force-reinstalls"
fi

cd ${ROOT}/pure
echo "============================="
echo "building the pure haskell lib"
echo "============================="
#echo "current directory = `pwd`"
cabal ${VX}clean
cabal ${VX}build                     || error_exit "building the pure lib failed"
cabal ${VX}haddock ${OPT_HYPERLINK}  || error_exit "pure hlib addock failed"
cabal ${VX}install --disable-documentation $OPT_REINSTALL || error_exit "installing the codegen failed"
# #if [ "$CABAL_MAJOR_VER" = "2" ]
# if [ "$VX" = "v2-" ]
# then
#   cabal ${VX}install --disable-documentation --force-reinstalls --overwrite-policy=always || error_exit "installing the codegen failed"
# else
#   cabal ${VX}install --disable-documentation --force-reinstalls || error_exit "installing the codegen failed"
# fi

cd ${ROOT}/codegen
echo "===================="
echo "building the codegen"
echo "===================="
#echo "current directory = `pwd`"
cabal ${VX}clean
cabal ${VX}build                     || error_exit "building the codegen failed"
cabal ${VX}haddock ${OPT_HYPERLINK}  || error_exit "codegen haddock failed"
cabal ${VX}install --disable-documentation $OPT_REINSTALL || error_exit "installing the codegen failed"
# # if [ "$CABAL_MAJOR_VER" = "2" ]
# if [ "$VX" = "v2-" ]
# then
#   # cabal install exe:zikkurat-algebra-codegen       --disable-documentation --force-reinstalls --overwrite-policy=always || error_exit "installing the codegen (exe) failed"
#   # cabal install lib:zikkurat-algebra-codegen --lib --disable-documentation --force-reinstalls --overwrite-policy=always || error_exit "installing the codegen (lib) failed"
#   cabal ${VX}install --disable-documentation --force-reinstalls --overwrite-policy=always || error_exit "installin gthe codegen failed"
# else
#   cabal ${VX}install --disable-documentation --force-reinstalls || error_exit "installing the codegen failed"
# fi

FLAGS=""

cd ${ROOT}/lib
echo "===================="
echo "building the library"
echo "===================="
#echo "current directory = `pwd`"
zikkurat-algebra-codegen all ${ROOT}/lib
cabal ${VX}clean                    || error_exit "cleaning the lib failed"
cabal ${VX}configure ${FLAGS}       || error_exit "configuring the lib failed"
cabal ${VX}build                    || error_exit "building the lib failed"
cabal ${VX}haddock ${OPT_HYPERLINK} || error_exit "haddocks for the lib failed"
if [ "$CABAL_MAJOR_VER" = "2" ]
then
  cabal ${VX}install            || error_exit "installing the lib failed"
else
  if [ "$VX" = "v2-" ]
  then
    cabal ${VX}install $OPT_REINSTALL --lib lib:zikkurat-algebra || error_exit "installing the lib failed"
  else
    cabal ${VX}install $OPT_REINSTALL || error_exit "installing the lib failed"
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
