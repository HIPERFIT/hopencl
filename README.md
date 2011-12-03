hopencl
=======
The present repository contains the latest snapshot of a set of
Haskell bindings to version 1.1 of the OpenCL specification.

The following sections of the OpenCL specification are not supported
currently:

  * Image Objects (section 5.3)
  * Sampler Objects (section 5.5)
  * Profiling of memory objects and kernels (section 5.12)
  * Extensions of the OpenCL standard involving the API
  * OpenGL and D3D related functions

In addition, the are not currently any Haskell wrappers for the
following functions:

  clCreateSubBuffer, clEnqueueReadBufferRect,
  clEnqueueWriteBufferRect,
  clEnqueueCopyBufferRect, clEnqueueMapBuffer,
  clEnqueueUnmapMemObject, clEnqueueNativeKernel

Installation
------------
Installation is handled by cabal, but the tool c2hs is needed for
compilation and should be installed prior to hopencl.

```shell
cabal install c2hs
cabal install
```

Examples
--------
In examples-directory two simple examples of how the hopencl package
can be used are shown. They can be executed using runhaskell

```shell
cd examples/PlatformInfo/
runhaskell PlatformInfo.hs
```

```shell
cd examples/VectorAdd/
runhaskell VectorAdd.hs
```

Tests
-----
To compile the test suite, the flag `--enable-tests` must be given to
cabal. The tests can then be executed using `cabal test`.

```shell
cabal install --enable-tests
cabal test
```

Questions and bug reports
-------------------------
Feel free to contact me by email at <dybber@dybber.dk> with any
questions or bug reports.

Alternatively, bugs can be submitted through the bug tracker at:
http://github.com/HIPERFIT/hopencl/issues
