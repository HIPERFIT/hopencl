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
import Foreign.C.Types (CInt(..))
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Concurrent (newForeignPtr)

import Foreign.OpenCL.Bindings.Internal.Types
import Foreign.OpenCL.Bindings.Internal.Logging as Log

  
attachFinalizer :: Finalizable a => Ptr a -> IO (ForeignPtr a)
attachFinalizer ptr = do
  Log.debug $ "Adding finalizer to " ++ (finalizableName ptr)
  newForeignPtr ptr (Log.debug ("releasing " ++ (finalizableName ptr)) >> release ptr)
  
attachRetainFinalizer :: Finalizable a => Ptr a -> IO (ForeignPtr a)
attachRetainFinalizer ptr = retain ptr >> attachFinalizer ptr


class Finalizable a where
  finalizableName :: Ptr a -> String
  release :: Ptr a -> IO ()
  retain :: Ptr a -> IO ClInt

-- Context --
instance Finalizable CContext where
  finalizableName _ = "Context"
  release ctx     = clReleaseContext ctx
  retain ctx      = clRetainContext ctx

-- CommandQueue --
instance Finalizable CCommandQueue where
  finalizableName _ = "CommandQueue"
  release queue   = clReleaseCommandQueue queue
  retain queue    = clRetainCommandQueue queue

-- Program --
instance Finalizable CProgram where
  finalizableName _ = "Program"
  release prog    = clReleaseProgram prog
  retain prog     = clRetainProgram prog

-- Kernel --
instance Finalizable CKernel where
  finalizableName _ = "Kernel"
  release kernel  = clReleaseKernel kernel
  retain kernel   = clRetainKernel kernel

-- Event --
instance Finalizable CEvent where
  finalizableName _ = "Event"
  release event   =  clReleaseEvent event
  retain event    = clRetainEvent event

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

foreign import CALLCONV "CL/cl.h clReleaseContext" clReleaseContext
   :: (ClContext -> IO ())

foreign import CALLCONV "CL/cl.h clReleaseCommandQueue" clReleaseCommandQueue
   :: (ClCommandQueue -> IO ())

foreign import CALLCONV "CL/cl.h clReleaseProgram" clReleaseProgram
   :: (ClProgram -> IO ())

foreign import CALLCONV "CL/cl.h clReleaseKernel" clReleaseKernel
   :: ClKernel -> IO ()

foreign import CALLCONV "CL/cl.h clReleaseEvent" clReleaseEvent
   :: (ClEvent -> IO ())
