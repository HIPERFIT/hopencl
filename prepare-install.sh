#!/bin/sh

BUILD="dist/build"
C2HS_ARGS="--output-dir=$BUILD --include=$BUILD  --cppopts=-Iinclude"

# This is necessary as cabal doesn't do dependency resolving before
# invoking c2hs, and thus gets the compilation order wrong.

if [ ! -e $BUILD/Foreign/OpenCL/Bindings/Internal/Types.chi ] || 
   [ ! -e $BUILD/Foreign/OpenCL/Bindings/Internal/Finalizers.chi ];
then
    mkdir -p $BUILD/Foreign/OpenCL/Bindings/Internal
    c2hs $C2HS_ARGS Foreign/OpenCL/Bindings/Internal/Types.chs
    c2hs $C2HS_ARGS Foreign/OpenCL/Bindings/Internal/Finalizers.chs
fi
