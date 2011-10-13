{-# LANGUAGE ForeignFunctionInterface #-}
#include <CL/cl.h>

module Foreign.OpenCL.Bindings.Internal.Finalizers (
attachFinalizer, attachRetainFinalizer
)
where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr

{# import Foreign.OpenCL.Bindings.Internal.Types #}

  
attachFinalizer :: Finalizable a => Ptr a -> IO (ForeignPtr a)
attachFinalizer = newForeignPtr releaseFn
  
attachRetainFinalizer :: Finalizable a => Ptr a -> IO (ForeignPtr a)
attachRetainFinalizer ptr = retain ptr >> attachFinalizer ptr


class Finalizable a where
  releaseFn :: FunPtr (Ptr a -> IO ())
  retain :: Ptr a -> IO ClInt

-- Context --
instance Finalizable CContext where
  releaseFn = clReleaseContextFunPtr
  retain = {# call unsafe clRetainContext #}

foreign import ccall "CL/cl.h &clReleaseContext" clReleaseContextFunPtr
   :: FunPtr (ClContext -> IO ())

-- CommandQueue --
instance Finalizable CCommandQueue where
  releaseFn = clReleaseCommandQueueFunPtr
  retain = {# call unsafe clRetainCommandQueue #}

foreign import ccall "CL/cl.h &clReleaseCommandQueue" clReleaseCommandQueueFunPtr
   :: FunPtr (ClCommandQueue -> IO ())

-- Program --
instance Finalizable CProgram where
  releaseFn = clReleaseProgramFunPtr
  retain = {# call unsafe clRetainProgram #}

foreign import ccall "CL/cl.h &clReleaseProgram" clReleaseProgramFunPtr
   :: FunPtr (ClProgram -> IO ())

-- Kernel --
instance Finalizable CKernel where
  releaseFn = clReleaseKernelFunPtr
  retain = {# call unsafe clRetainKernel #}

foreign import ccall "CL/cl.h &clReleaseKernel" clReleaseKernelFunPtr
   :: FunPtr (ClKernel -> IO ())

-- Event --
instance Finalizable CEvent where
  releaseFn = clReleaseEventFunPtr
  retain = {# call unsafe clRetainEvent #}

foreign import ccall "CL/cl.h &clReleaseEvent" clReleaseEventFunPtr
   :: FunPtr (ClEvent -> IO ())
