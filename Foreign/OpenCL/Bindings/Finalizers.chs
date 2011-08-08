{-# LANGUAGE ForeignFunctionInterface #-}
#include <CL/cl.h>

module Foreign.OpenCL.Bindings.Finalizers (
  attachContextFinalizer,
  attachCommandQueueFinalizer,
  attachProgramFinalizer,
  withForeignPtrs
)
where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr

{# import Foreign.OpenCL.Bindings.Types #}

-- Context --
attachContextFinalizer :: ClContext -> IO Context
attachContextFinalizer = newForeignPtr clReleaseContextFunPtr

-- Pointer to context release function
foreign import ccall "CL/cl.h &clReleaseContext" clReleaseContextFunPtr
   :: FunPtr (ClContext -> IO ())

-- CommandQueue --
attachCommandQueueFinalizer :: ClCommandQueue -> IO CommandQueue
attachCommandQueueFinalizer = newForeignPtr clReleaseCommandQueueFunPtr

-- Pointer to context release function
foreign import ccall "CL/cl.h &clReleaseCommandQueue" clReleaseCommandQueueFunPtr
   :: FunPtr (ClCommandQueue -> IO ())

-- Program Objects --
attachProgramFinalizer :: ClProgram -> IO Program
attachProgramFinalizer = newForeignPtr clReleaseProgramFunPtr

-- Pointer to context release function
foreign import ccall "CL/cl.h &clReleaseProgram" clReleaseProgramFunPtr
   :: FunPtr (ClProgram -> IO ())

-- | Look at a list of foreign pointers, ensuring the pointers are not
-- freed for at least the duration of the computation.
withForeignPtrs :: [ForeignPtr a] -> ([Ptr a] -> IO b) -> IO b
withForeignPtrs ptrs f = with' ptrs []
  where
    with' [] ys = f (reverse ys)
    with' (p:ps) ys = withForeignPtr p $ \p' -> with' ps (p':ys)

