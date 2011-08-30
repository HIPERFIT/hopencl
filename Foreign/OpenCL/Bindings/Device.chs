{-# LANGUAGE ForeignFunctionInterface #-}
#include <CL/cl.h>

module Foreign.OpenCL.Bindings.Device (
   getDeviceIDs,

   -- |The following functions and types are used to obtain properties of
   -- devices.
   deviceAddressBits, deviceAvailable, deviceCompilerAvailable,
   deviceEndianLittle, deviceErrorCorrectionSupport,

   deviceExecutionCapabilities,

   deviceExtensions, deviceGlobalMemCacheSize, deviceGlobalMemCacheLineSize,
   deviceGlobalMemSize, deviceGlobalMemCacheType,
   deviceLocalMemSize, deviceLocalMemType,

   deviceImageSupport, deviceImage2DMaxSize, deviceImage3DMaxSize,

   deviceMaxClockFrequency, deviceMaxComputeUnits, deviceMaxConstantArgs,
   deviceMaxConstantBufferSize, deviceMaxMemAllocSize, deviceMaxParameterSize,
   deviceMaxReadImageArgs, deviceMaxSamplers, deviceMaxWorkGroupSize,
   deviceMaxWorkItemDimensions, deviceMaxWorkItemSizes,
   deviceMaxWriteImageArgs, deviceMemBaseAddrAlign, deviceMinDataTypeAlignSize,
   deviceName, devicePlatform, devicePreferredVectorWidthChar,
   devicePreferredVectorWidthShort, devicePreferredVectorWidthInt,
   devicePreferredVectorWidthLong, devicePreferredVectorWidthFloat,
   deviceProfile, deviceProfilingTimerResolution,
   deviceQueueProperties, deviceSingleFPConfig,
   deviceType, deviceVendor, deviceVendorID, deviceVersion, deviceDriverVersion
 ) where

import Data.Bits
import Data.Word

import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr

{# import Foreign.OpenCL.Bindings.Types #}
{# import Foreign.OpenCL.Bindings.Error #}

import Foreign.OpenCL.Bindings.Util

import Foreign.Storable
import Foreign.Marshal
import Control.Monad

-- TODO shouldn't the first argument be a list of device types?
-- ^Obtain a list of available platforms.
getDeviceIDs ::  DeviceType -> PlatformID -> IO [DeviceID]
getDeviceIDs typ platform =
  getList (clGetDeviceIDs_ platform typ)

getDeviceInfo device info =
    getInfo (clGetDeviceInfo_ device) info

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


------- Below are the device info query functions --------
-- |The compute device address space size in bits.
deviceAddressBits :: DeviceID -> IO Word32
deviceAddressBits dev = fromIntegral `fmap` (getDeviceInfo dev DeviceAddressBits :: IO ClUInt)

-- |True if the device is available.
deviceAvailable :: DeviceID -> IO Bool
deviceAvailable dev = getDeviceInfo dev DeviceAvailable

-- |True if the compiler is available, can only be false for the embedded
-- profile.
deviceCompilerAvailable :: DeviceID -> IO Bool
deviceCompilerAvailable dev = getDeviceInfo dev DeviceCompilerAvailable

-- |True if the device is little endian.
deviceEndianLittle :: DeviceID -> IO Bool
deviceEndianLittle dev = getDeviceInfo dev DeviceEndianLittle

-- |True if the device supports error connection for memories, cache, and
-- registers.
deviceErrorCorrectionSupport :: DeviceID -> IO Bool
deviceErrorCorrectionSupport dev = getDeviceInfo dev DeviceErrorCorrectionSupport

-- |The execution capabilities of the device
deviceExecutionCapabilities :: DeviceID -> IO [DeviceExecCapabilities]
deviceExecutionCapabilities dev = do
   cap <- (getDeviceInfo dev DeviceExecutionCapabilities :: IO {#type cl_device_exec_capabilities #})
   return . filter (\x -> cap .&. (fromIntegral $ fromEnum x) /= 0)
      $ [ExecKernel, ExecNativeKernel]

-- |The extensions supported by the device
deviceExtensions :: DeviceID -> IO [String]
deviceExtensions dev = return . words =<< getDeviceInfo dev DeviceExtensions

-- |The size in bytes of the global memory cache
deviceGlobalMemCacheSize :: DeviceID -> IO ClULong
deviceGlobalMemCacheSize dev = getDeviceInfo dev DeviceGlobalMemCacheSize

-- |Size of global memory cache line size in bytes
deviceGlobalMemCacheLineSize :: DeviceID -> IO ClUInt
deviceGlobalMemCacheLineSize dev = getDeviceInfo dev DeviceGlobalMemCachelineSize

-- |Size of global memory size in bytes
deviceGlobalMemSize :: DeviceID -> IO ClULong
deviceGlobalMemSize dev = getDeviceInfo dev DeviceGlobalMemSize

-- |The type of global memory cache on the device.
deviceGlobalMemCacheType :: DeviceID -> IO DeviceMemCacheType
deviceGlobalMemCacheType dev = do
   typ <- getDeviceInfo dev DeviceGlobalMemCacheType :: IO {#type cl_device_mem_cache_type #}
   return . toEnum $ fromIntegral typ

-- |True if images are supported by the device.
deviceImageSupport :: DeviceID -> IO Bool
deviceImageSupport dev = getDeviceInfo dev DeviceImageSupport

-- |Max size of a 2D image (width, height)
deviceImage2DMaxSize :: DeviceID -> IO (CSize, CSize)
deviceImage2DMaxSize dev = do
   w <- getDeviceInfo dev DeviceImage2DMaxWidth
   h <- getDeviceInfo dev DeviceImage2DMaxHeight
   return (w,h)

-- |Max size of a 3D image (width, height, depth)
deviceImage3DMaxSize :: DeviceID -> IO (CSize, CSize, CSize)
deviceImage3DMaxSize dev = do
   w <- getDeviceInfo dev DeviceImage3DMaxWidth
   h <- getDeviceInfo dev DeviceImage3DMaxHeight
   d <- getDeviceInfo dev DeviceImage3DMaxDepth
   return (w, h, d)

-- |The size of the local memory arena in bytes.
deviceLocalMemSize :: DeviceID -> IO ClULong
deviceLocalMemSize dev = getDeviceInfo dev DeviceLocalMemSize

-- |The type of local memory on the device.
deviceLocalMemType :: DeviceID -> IO DeviceLocalMemType
deviceLocalMemType dev = do
   typ <- getDeviceInfo dev DeviceLocalMemType :: IO {#type cl_device_local_mem_type #}
   return . toEnum $ fromIntegral typ

-- |Maximum configured clock frequency of the device in MHz
deviceMaxClockFrequency :: DeviceID -> IO Word32
deviceMaxClockFrequency dev = fromIntegral `fmap` (getDeviceInfo dev DeviceMaxClockFrequency :: IO ClUInt)

-- |The number of parallel compute cos on the device
deviceMaxComputeUnits :: DeviceID -> IO Word32
deviceMaxComputeUnits dev = fromIntegral `fmap` (getDeviceInfo dev DeviceMaxComputeUnits :: IO ClUInt)

-- |Max number of constant arguments to a kernel
deviceMaxConstantArgs :: DeviceID -> IO ClUInt
deviceMaxConstantArgs dev = getDeviceInfo dev DeviceMaxConstantArgs

-- |Max size in bytes of a constant buffer allocation
deviceMaxConstantBufferSize :: DeviceID -> IO ClULong
deviceMaxConstantBufferSize dev = getDeviceInfo dev DeviceMaxConstantBufferSize

-- |Max size of device memory allocation in bytes
deviceMaxMemAllocSize :: DeviceID -> IO ClULong
deviceMaxMemAllocSize dev = getDeviceInfo dev DeviceMaxMemAllocSize

-- |Max size in bytes of the arguments that can be passed to a kernel
deviceMaxParameterSize :: DeviceID -> IO CSize
deviceMaxParameterSize dev = getDeviceInfo dev DeviceMaxParameterSize

-- |Maximum number of simultaneous image objects that can be read by a kernel
deviceMaxReadImageArgs :: DeviceID -> IO ClUInt
deviceMaxReadImageArgs dev = getDeviceInfo dev DeviceMaxReadImageArgs

-- |Maximum number of samplers that can be used in a kernel
deviceMaxSamplers :: DeviceID -> IO ClUInt
deviceMaxSamplers dev = getDeviceInfo dev DeviceMaxSamplers

-- |Maximum number of work-items in a work-group executing a kernel using the
-- data parallel execution model.
deviceMaxWorkGroupSize :: DeviceID -> IO CSize
deviceMaxWorkGroupSize dev = getDeviceInfo dev DeviceMaxWorkGroupSize

-- |Maximum number of dimensions for work-item IDs used by the data parallel
-- execution model.
deviceMaxWorkItemDimensions :: DeviceID -> IO ClUInt
deviceMaxWorkItemDimensions dev = getDeviceInfo dev DeviceMaxWorkItemDimensions

-- |Maximum number of work items that can be specified in each dimension of the
-- work-group when using the data parallel execution model.
deviceMaxWorkItemSizes :: DeviceID -> IO [CSize]
deviceMaxWorkItemSizes dev = do
   dim <- fromIntegral `fmap` deviceMaxWorkItemDimensions dev
   let array_size = fromIntegral $ dim * (fromIntegral $ sizeOf (undefined :: CSize))
   alloca $ \psize ->
     allocaArray dim $ \arr -> do
       clGetDeviceInfo_ dev (fromIntegral $ fromEnum DeviceMaxWorkItemSizes)
                        array_size (castPtr arr) psize
       size' <- peek psize
       when (array_size /= size') $ error "Size mismatch in element size array"
       mapM (peekElemOff arr) [0 .. dim - 1]

-- |Maximum number of simultaneous image objects that can be written by a kernel
deviceMaxWriteImageArgs :: DeviceID -> IO ClUInt
deviceMaxWriteImageArgs dev = getDeviceInfo dev DeviceMaxWriteImageArgs

-- |Describes the alignment in bits of the base address of any allocated memory
-- object
deviceMemBaseAddrAlign :: DeviceID -> IO ClUInt
deviceMemBaseAddrAlign dev = getDeviceInfo dev DeviceMemBaseAddrAlign

-- |The smallest alignment in bytes which can be used for any data type
deviceMinDataTypeAlignSize :: DeviceID -> IO ClUInt
deviceMinDataTypeAlignSize dev = getDeviceInfo dev DeviceMinDataTypeAlignSize

-- |The name of the device
deviceName :: DeviceID -> IO String
deviceName dev = getDeviceInfo dev DeviceName

-- |The platform associated with this device
devicePlatform :: DeviceID -> IO PlatformID
devicePlatform dev = getDeviceInfo dev DevicePlatform

-- |Preferred native number of elements in char vectors
devicePreferredVectorWidthChar :: DeviceID -> IO ClUInt
devicePreferredVectorWidthChar dev = getDeviceInfo dev DevicePreferredVectorWidthChar

-- |Preferred native number of elements in short vectors
devicePreferredVectorWidthShort :: DeviceID -> IO ClUInt
devicePreferredVectorWidthShort dev = getDeviceInfo dev DevicePreferredVectorWidthShort

-- |Preferred native number of elements in int vectors
devicePreferredVectorWidthInt :: DeviceID -> IO ClUInt
devicePreferredVectorWidthInt dev = getDeviceInfo dev DevicePreferredVectorWidthInt

-- |Preferred native number of elements in long vectors
devicePreferredVectorWidthLong :: DeviceID -> IO ClUInt
devicePreferredVectorWidthLong dev = getDeviceInfo dev DevicePreferredVectorWidthLong

-- |Preferred native number of elements in float vectors
devicePreferredVectorWidthFloat :: DeviceID -> IO ClUInt
devicePreferredVectorWidthFloat dev = getDeviceInfo dev DevicePreferredVectorWidthFloat

-- |The profile supported by the device
deviceProfile :: DeviceID -> IO String
deviceProfile dev = getDeviceInfo dev DeviceProfile :: IO String

-- |The resolution in nanoseconds of the device timer
deviceProfilingTimerResolution :: DeviceID -> IO CSize
deviceProfilingTimerResolution dev = getDeviceInfo dev DeviceProfilingTimerResolution

-- |The command queue properties supported by the device
deviceQueueProperties :: DeviceID -> IO [CommandQueueProperties]
deviceQueueProperties dev = do
   cap <- (getDeviceInfo dev DeviceQueueProperties :: IO {#type cl_command_queue_properties #})
   return . filter (\x -> cap .&. (fromIntegral $ fromEnum x) /= 0)
      $ [QueueOutOfOrderExecModeEnable,
         QueueProfilingEnable]

-- |Describe the single precision floating point capability of the device
deviceSingleFPConfig :: DeviceID -> IO [DeviceFPConfig]
deviceSingleFPConfig dev = do
   cap <- (getDeviceInfo dev DeviceSingleFPConfig :: IO {#type cl_device_fp_config #})
   return . filter (\x -> cap .&. (fromIntegral $ fromEnum x) /= 0)
      $ [FpDenorm,
         FpInfNan,
         FpRoundToNearest,
         FpRoundToZero,
         FpRoundToInf,
         FpFma,
         FpSoftFloat]

-- |Describe the type of the device
deviceType :: DeviceID -> IO [DeviceType]
deviceType dev = do
   cap <- (getDeviceInfo dev DeviceType :: IO {#type cl_device_type #})
   return . filter (\x -> cap .&. (fromIntegral $ fromEnum x) /= 0)
      $ [DeviceTypeDefault,
         DeviceTypeCpu,
         DeviceTypeGpu,
         DeviceTypeAccelerator,
         DeviceTypeAll]

-- |Obtain the device vendor name
deviceVendor :: DeviceID -> IO String
deviceVendor dev = getDeviceInfo dev DeviceVendor

-- |Obtain a unique device vendor indentifier, which may be the PCIe ID.
deviceVendorID :: DeviceID -> IO ClUInt
deviceVendorID dev = getDeviceInfo dev DeviceVendorID

-- |Obtain the OpenCL version supported by the device
deviceVersion :: DeviceID -> IO String
deviceVersion dev = getDeviceInfo dev DeviceVersion

-- |Obtain the OpenCL software driver version string
deviceDriverVersion :: DeviceID -> IO String
deviceDriverVersion dev = getDeviceInfo dev DriverVersion

