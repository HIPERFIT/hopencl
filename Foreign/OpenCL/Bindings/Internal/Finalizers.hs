{-# LANGUAGE ForeignFunctionInterface, CPP #-}
-- |
-- Module      : Foreign.OpenCL.Bindings.Internal.Finalizers
-- Copyright   : (c) 2011, Martin Dybdal
-- License     : BSD3
-- 
-- Maintainer  : Martin Dybdal <dybber@dybber.dk>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Foreign.OpenCL.Bindings.Internal.Finalizers (
attachFinalizer, attachRetainFinalizer
)
where

import Foreign.Ptr
import Foreign.ForeignPtr

import Foreign.OpenCL.Bindings.Internal.Types

  
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
  retain = clRetainContext

-- CommandQueue --
instance Finalizable CCommandQueue where
  releaseFn = clReleaseCommandQueueFunPtr
  retain = clRetainCommandQueue

-- Program --
instance Finalizable CProgram where
  releaseFn = clReleaseProgramFunPtr
  retain = clRetainProgram

-- Kernel --
instance Finalizable CKernel where
  releaseFn = clReleaseKernelFunPtr
  retain = clRetainKernel

-- Event --
instance Finalizable CEvent where
  releaseFn = clReleaseEventFunPtr
  retain = clRetainEvent


#ifdef __APPLE__

foreign import CALLCONV "OpenCL/cl.h clRetainContext" clRetainContext
   :: (ClContext -> IO ClInt)

foreign import CALLCONV "OpenCL/cl.h clRetainCommandQueue" clRetainCommandQueue
   :: (ClCommandQueue -> IO ClInt)

foreign import CALLCONV "OpenCL/cl.h clRetainProgram" clRetainProgram
   :: (ClProgram -> IO ClInt)

foreign import CALLCONV "OpenCL/cl.h clRetainKernel" clRetainKernel
   :: (ClKernel -> IO ClInt)

foreign import CALLCONV "OpenCL/cl.h clRetainEvent" clRetainEvent
   :: (ClEvent -> IO ClInt)

foreign import CALLCONV "OpenCL/cl.h &clReleaseContext" clReleaseContextFunPtr
   :: FunPtr (ClContext -> IO ())

foreign import CALLCONV "OpenCL/cl.h &clReleaseCommandQueue" clReleaseCommandQueueFunPtr
   :: FunPtr (ClCommandQueue -> IO ())

foreign import CALLCONV "OpenCL/cl.h &clReleaseProgram" clReleaseProgramFunPtr
   :: FunPtr (ClProgram -> IO ())

foreign import CALLCONV "OpenCL/cl.h &clReleaseKernel" clReleaseKernelFunPtr
   :: FunPtr (ClKernel -> IO ())

foreign import CALLCONV "OpenCL/cl.h &clReleaseEvent" clReleaseEventFunPtr
   :: FunPtr (ClEvent -> IO ())

#else

foreign import CALLCONV "CL/cl.h clRetainContext" clRetainContext
   :: (ClContext -> IO ClInt)

foreign import CALLCONV "CL/cl.h clRetainCommandQueue" clRetainCommandQueue
   :: (ClCommandQueue -> IO ClInt)

foreign import CALLCONV "CL/cl.h clRetainProgram" clRetainProgram
   :: (ClProgram -> IO ClInt)

foreign import CALLCONV "CL/cl.h clRetainKernel" clRetainKernel
   :: (ClKernel -> IO ClInt)

foreign import CALLCONV "CL/cl.h clRetainEvent" clRetainEvent
   :: (ClEvent -> IO ClInt)


foreign import CALLCONV "CL/cl.h &clReleaseContext" clReleaseContextFunPtr
   :: FunPtr (ClContext -> IO ())

foreign import CALLCONV "CL/cl.h &clReleaseCommandQueue" clReleaseCommandQueueFunPtr
   :: FunPtr (ClCommandQueue -> IO ())

foreign import CALLCONV "CL/cl.h &clReleaseProgram" clReleaseProgramFunPtr
   :: FunPtr (ClProgram -> IO ())

foreign import CALLCONV "CL/cl.h &clReleaseKernel" clReleaseKernelFunPtr
   :: FunPtr (ClKernel -> IO ())

foreign import CALLCONV "CL/cl.h &clReleaseEvent" clReleaseEventFunPtr
   :: FunPtr (ClEvent -> IO ())

#endif


