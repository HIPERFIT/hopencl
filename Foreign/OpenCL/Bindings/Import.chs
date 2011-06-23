{-# LANGUAGE ForeignFunctionInterface #-}
#include <CL/cl.h>

module Foreign.OpenCL.Bindings.Import (
  attachPlatformFinalizer, clGetPlatformIDs_, clGetPlatformInfo_,

  attachDeviceFinalizer, clGetDeviceIDs_, clGetDeviceInfo_,

  withForeignPtrs
)
where


import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr

{# import Foreign.OpenCL.Bindings.Types #}
{# import Foreign.OpenCL.Bindings.Error #}

----- Platform -----

attachPlatformFinalizer :: ClPlatformID -> IO Platform
attachPlatformFinalizer = newForeignPtr clReleasePlatformFunPtr

-- Pointer to platform release function
foreign import ccall "CL/cl.h &clReleasePlatform" clReleasePlatformFunPtr
   :: FunPtr (ClPlatformID -> IO ())

-- Interfacing functions that performs error checking
clGetPlatformIDs_ num_entries platforms num_platforms = do
     errcode <- {#call unsafe clGetPlatformIDs #} num_entries platforms num_platforms
     checkErrorA "clGetPlatformIDs" errcode
     return errcode

clGetPlatformInfo_ platform name size value size_ret = do
     errcode <- {#call unsafe clGetPlatformInfo #} platform name size value size_ret
     checkErrorA "clGetPlatformInfo" errcode
     return errcode

----- Device -----

attachDeviceFinalizer :: ClDeviceID -> IO Device
attachDeviceFinalizer = newForeignPtr clReleaseDeviceFunPtr

-- Pointer to platform release function
foreign import ccall "CL/cl.h &clReleaseDevice" clReleaseDeviceFunPtr
   :: FunPtr (ClDeviceID -> IO ())


-- Interfacing functions that performs error checking
clGetDeviceIDs_ platform device_type num_entries devices num_devices =
  do errcode <- {#call unsafe clGetDeviceIDs #} platform typ_code num_entries devices num_devices
     checkErrorA "clGetDeviceIDs" errcode
     return errcode
    where typ_code = fromIntegral (fromEnum device_type)

clGetDeviceInfo_ device name size value size_ret =
  do errcode <- {#call unsafe clGetDeviceInfo #} device name size value size_ret
     checkErrorA "clGetDeviceInfo" errcode
     return errcode

-- | Look at a list of foreign pointers, ensuring the pointers are not
-- freed for at least the duration of the computation.
withForeignPtrs :: [ForeignPtr a] -> ([Ptr a] -> IO b) -> IO b
withForeignPtrs ptrs f = with' ptrs []
  where
--    with' :: [ForeignPtr a] -> [Ptr a] -> IO b
    with' [] ys = f (reverse ys)
    with' (p:ps) ys = withForeignPtr p $ \p' -> with' ps (p':ys)
