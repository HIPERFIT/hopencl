#!/bin/sh

# This is necessary as cabal doesn't do dependency resolving before
# invoking c2hs, and thus gets the compilation order wrong.
mkdir -p dist/build/Foreign/OpenCL/Bindings/Internal
c2hs --output-dir=dist/build --include=dist/build --cppopts='-Iinclude' Foreign/OpenCL/Bindings/Internal/Types.chs
c2hs --output-dir=dist/build --include=dist/build --cppopts='-Iinclude' Foreign/OpenCL/Bindings/Internal/Finalizers.chs
