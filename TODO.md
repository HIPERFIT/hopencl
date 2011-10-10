Tasks
-----
  - Write tests

  - Code coverage of c2hs files when running tests

  - Write larger example program (for instance, port an example
    program from AMD or NVIDIA OpenCL distribution)

  - Write documentation and verify that the Haddock output looks nice

  - Use clCreateProgramWithBinary and programBinaries to implement a
    file based storage of compiled kernels.
    
  - Review dependency list in .cabal

Missing functionality
---------------------
The following would be nice to have, but there are no current plans of
implementing. Please send me and email if you find them necessary.

These operations are currently not supported:

  * clCreateSubBuffer
  * clEnqueueReadBufferRect
  * clEnqueueWriteBufferRect
  * clEnqueueCopyBufferRect
  * clEnqueueMapBuffer
  * clEnqueueUnmapMemObject
  * clEnqueueNativeKernel

The following full sections of the OpenCL standard are not supported:

  * Image Objects (section 5.3)
  * Sampler Objects (section 5.5)
  * Profiling of memory objects and kernels (section 5.12)
  * Extensions of the OpenCL standard involving the API
  * OpenGL and D3D related functions
