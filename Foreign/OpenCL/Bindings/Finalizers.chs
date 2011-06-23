{-# LANGUAGE ForeignFunctionInterface #-}
#include <CL/cl.h>

module Foreign.OpenCL.Bindings.Finalizers (
  attachPlatformFinalizer,
  attachDeviceFinalizer,
  withForeignPtrs
)
where


import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr

{# import Foreign.OpenCL.Bindings.Types #}

----- Platform -----

attachPlatformFinalizer :: ClPlatformID -> IO Platform
attachPlatformFinalizer = newForeignPtr clReleasePlatformFunPtr

-- Pointer to platform release function
foreign import ccall "CL/cl.h &clReleasePlatform" clReleasePlatformFunPtr
   :: FunPtr (ClPlatformID -> IO ())

----- Device -----

attachDeviceFinalizer :: ClDeviceID -> IO Device
attachDeviceFinalizer = newForeignPtr clReleaseDeviceFunPtr

-- Pointer to platform release function
foreign import ccall "CL/cl.h &clReleaseDevice" clReleaseDeviceFunPtr
   :: FunPtr (ClDeviceID -> IO ())



-- | Look at a list of foreign pointers, ensuring the pointers are not
-- freed for at least the duration of the computation.
withForeignPtrs :: [ForeignPtr a] -> ([Ptr a] -> IO b) -> IO b
withForeignPtrs ptrs f = with' ptrs []
  where
--    with' :: [ForeignPtr a] -> [Ptr a] -> IO b
    with' [] ys = f (reverse ys)
    with' (p:ps) ys = withForeignPtr p $ \p' -> with' ps (p':ys)

