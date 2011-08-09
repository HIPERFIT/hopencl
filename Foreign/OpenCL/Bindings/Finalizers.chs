{-# LANGUAGE ForeignFunctionInterface #-}
#include <CL/cl.h>

module Foreign.OpenCL.Bindings.Finalizers (
  attachContextFinalizer,
  attachCommandQueueFinalizer,
  attachProgramFinalizer,
  attachKernelFinalizer,
  attachEventFinalizer
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

-- Kernel Objects --
attachKernelFinalizer :: ClKernel -> IO Kernel
attachKernelFinalizer = newForeignPtr clReleaseKernelFunPtr

-- Pointer to context release function
foreign import ccall "CL/cl.h &clReleaseKernel" clReleaseKernelFunPtr
   :: FunPtr (ClKernel -> IO ())

-- Event Objects --
attachEventFinalizer :: ClEvent -> IO Event
attachEventFinalizer = newForeignPtr clReleaseEventFunPtr

-- Pointer to context release function
foreign import ccall "CL/cl.h &clReleaseEvent" clReleaseEventFunPtr
   :: FunPtr (ClEvent -> IO ())
