{-# LANGUAGE ForeignFunctionInterface #-}
#include <CL/cl.h>

module Foreign.OpenCL.Bindings.Device (
   getDevices,

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
   deviceMaxWorkItemDimensions,
--   deviceMaxWorkItemSizes,
   deviceMaxWriteImageArgs, deviceMemBaseAddrAlign, deviceMinDataTypeAlignSize,
   deviceName, devicePlatform, devicePreferredVectorWidthChar,
   devicePreferredVectorWidthShort, devicePreferredVectorWidthInt,
   devicePreferredVectorWidthLong, devicePreferredVectorWidthFloat,
   deviceProfile, deviceProfilingTimerResolution,
--   deviceQueueProperties,
   deviceSingleFPConfig,
   deviceType, deviceVendor, deviceVendorID, deviceVersion, deviceDriverVersion
 ) where

import Data.Bits
import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr

{# import Foreign.OpenCL.Bindings.Types #}
{# import Foreign.OpenCL.Bindings.Error #}
{# import Foreign.OpenCL.Bindings.Finalizers #}
import Foreign.OpenCL.Bindings.Util

-- ^Obtain a list of available platforms.
getDevices ::  Platform -> DeviceType -> IO [Device]
getDevices platform typ =
  withForeignPtr platform $ \ptr ->
  mapM attachDeviceFinalizer =<< getList (clGetDeviceIDs_ ptr typ)

getDeviceInfo device info =
  withForeignPtr device $ \ptr ->
    getInfo (clGetDeviceInfo_ ptr) info

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
deviceAddressBits :: Device -> IO ClUInt
deviceAddressBits dev = getDeviceInfo dev DeviceAddressBits

-- |True if the device is available.
deviceAvailable :: Device -> IO Bool
deviceAvailable dev = getDeviceInfo dev DeviceAvailable

-- |True if the compiler is available, can only be false for the embedded
-- profile.
deviceCompilerAvailable :: Device -> IO Bool
deviceCompilerAvailable dev = getDeviceInfo dev DeviceCompilerAvailable

-- |True if the device is little endian.
deviceEndianLittle :: Device -> IO Bool
deviceEndianLittle dev = getDeviceInfo dev DeviceEndianLittle

-- |True if the device supports error connection for memories, cache, and
-- registers.
deviceErrorCorrectionSupport :: Device -> IO Bool
deviceErrorCorrectionSupport dev = getDeviceInfo dev DeviceErrorCorrectionSupport

-- |The execution capabilities of the device
deviceExecutionCapabilities :: Device -> IO [DeviceExecCapabilities]
deviceExecutionCapabilities dev = do
   cap <- (getDeviceInfo dev DeviceExecutionCapabilities :: IO {#type cl_device_exec_capabilities #})
   return . filter (\x -> cap .&. (fromIntegral $ fromEnum x) /= 0)
      $ [ExecKernel, ExecNativeKernel]

-- |The extensions supported by the device
deviceExtensions :: Device -> IO [String]
deviceExtensions dev = return . words =<< getDeviceInfo dev DeviceExtensions

-- |The size in bytes of the global memory cache
deviceGlobalMemCacheSize :: Device -> IO ClULong
deviceGlobalMemCacheSize dev = getDeviceInfo dev DeviceGlobalMemCacheSize

-- |Size of global memory cache line size in bytes
deviceGlobalMemCacheLineSize :: Device -> IO ClUInt
deviceGlobalMemCacheLineSize dev = getDeviceInfo dev DeviceGlobalMemCachelineSize

-- |Size of global memory size in bytes
deviceGlobalMemSize :: Device -> IO ClULong
deviceGlobalMemSize dev = getDeviceInfo dev DeviceGlobalMemSize

-- |The type of global memory cache on the device.
deviceGlobalMemCacheType :: Device -> IO DeviceMemCacheType
deviceGlobalMemCacheType dev = do
   typ <- getDeviceInfo dev DeviceGlobalMemCacheType :: IO {#type cl_device_mem_cache_type #}
   return . toEnum $ fromIntegral typ

-- |True if images are supported by the device.
deviceImageSupport :: Device -> IO Bool
deviceImageSupport dev = getDeviceInfo dev DeviceImageSupport

-- |Max size of a 2D image (width, height)
deviceImage2DMaxSize :: Device -> IO (CSize, CSize)
deviceImage2DMaxSize dev = do
   w <- getDeviceInfo dev DeviceImage2DMaxWidth
   h <- getDeviceInfo dev DeviceImage2DMaxHeight
   return (w,h)

-- |Max size of a 3D image (width, height, depth)
deviceImage3DMaxSize :: Device -> IO (CSize, CSize, CSize)
deviceImage3DMaxSize dev = do
   w <- getDeviceInfo dev DeviceImage3DMaxWidth
   h <- getDeviceInfo dev DeviceImage3DMaxHeight
   d <- getDeviceInfo dev DeviceImage3DMaxDepth
   return (w, h, d)

-- |The size of the local memory arena in bytes.
deviceLocalMemSize :: Device -> IO ClULong
deviceLocalMemSize dev = getDeviceInfo dev DeviceLocalMemSize

-- |The type of local memory on the device.
deviceLocalMemType :: Device -> IO DeviceLocalMemType
deviceLocalMemType dev = do
   typ <- getDeviceInfo dev DeviceLocalMemType :: IO {#type cl_device_local_mem_type #}
   return . toEnum $ fromIntegral typ

-- |Maximum configured clock frequency of the device in MHz
deviceMaxClockFrequency :: Device -> IO ClUInt
deviceMaxClockFrequency dev = getDeviceInfo dev DeviceMaxClockFrequency

-- |The number of parallel compute cos on the device
deviceMaxComputeUnits :: Device -> IO ClUInt
deviceMaxComputeUnits dev = getDeviceInfo dev DeviceMaxComputeUnits

-- |Max number of constant arguments to a kernel
deviceMaxConstantArgs :: Device -> IO ClUInt
deviceMaxConstantArgs dev = getDeviceInfo dev DeviceMaxConstantArgs

-- |Max size in bytes of a constant buffer allocation
deviceMaxConstantBufferSize :: Device -> IO ClULong
deviceMaxConstantBufferSize dev = getDeviceInfo dev DeviceMaxConstantBufferSize

-- |Max size of device memory allocation in bytes
deviceMaxMemAllocSize :: Device -> IO ClULong
deviceMaxMemAllocSize dev = getDeviceInfo dev DeviceMaxMemAllocSize

-- |Max size in bytes of the arguments that can be passed to a kernel
deviceMaxParameterSize :: Device -> IO CSize
deviceMaxParameterSize dev = getDeviceInfo dev DeviceMaxParameterSize

-- |Maximum number of simultaneous image objects that can be read by a kernel
deviceMaxReadImageArgs :: Device -> IO ClUInt
deviceMaxReadImageArgs dev = getDeviceInfo dev DeviceMaxReadImageArgs

-- |Maximum number of samplers that can be used in a kernel
deviceMaxSamplers :: Device -> IO ClUInt
deviceMaxSamplers dev = getDeviceInfo dev DeviceMaxSamplers

-- |Maximum number of work-items in a work-group executing a kernel using the
-- data parallel execution model.
deviceMaxWorkGroupSize :: Device -> IO CSize
deviceMaxWorkGroupSize dev = getDeviceInfo dev DeviceMaxWorkGroupSize

-- |Maximum number of dimensions for work-item IDs used by the data parallel
-- execution model.
deviceMaxWorkItemDimensions :: Device -> IO ClUInt
deviceMaxWorkItemDimensions dev = getDeviceInfo dev DeviceMaxWorkItemDimensions

-- -- |Maximum number of work items that can be specified in each dimension of the
-- -- work-group when using the data parallel execution model.
-- deviceMaxWorkItemSizes :: Device -> IO (Array Int CSize)
-- deviceMaxWorkItemSizes dev =

-- |Maximum number of simultaneous image objects that can be written by a kernel
deviceMaxWriteImageArgs :: Device -> IO ClUInt
deviceMaxWriteImageArgs dev = getDeviceInfo dev DeviceMaxWriteImageArgs

-- |Describes the alignment in bits of the base address of any allocated memory
-- object
deviceMemBaseAddrAlign :: Device -> IO ClUInt
deviceMemBaseAddrAlign dev = getDeviceInfo dev DeviceMemBaseAddrAlign

-- |The smallest alignment in bytes which can be used for any data type
deviceMinDataTypeAlignSize :: Device -> IO ClUInt
deviceMinDataTypeAlignSize dev = getDeviceInfo dev DeviceMinDataTypeAlignSize

-- |The name of the device
deviceName :: Device -> IO String
deviceName dev = getDeviceInfo dev DeviceName

-- |The platform associated with this device
devicePlatform :: Device -> IO ClPlatformID
devicePlatform dev = getDeviceInfo dev DevicePlatform

-- |Preferred native number of elements in char vectors
devicePreferredVectorWidthChar :: Device -> IO ClUInt
devicePreferredVectorWidthChar dev = getDeviceInfo dev DevicePreferredVectorWidthChar

-- |Preferred native number of elements in short vectors
devicePreferredVectorWidthShort :: Device -> IO ClUInt
devicePreferredVectorWidthShort dev = getDeviceInfo dev DevicePreferredVectorWidthShort

-- |Preferred native number of elements in int vectors
devicePreferredVectorWidthInt :: Device -> IO ClUInt
devicePreferredVectorWidthInt dev = getDeviceInfo dev DevicePreferredVectorWidthInt

-- |Preferred native number of elements in long vectors
devicePreferredVectorWidthLong :: Device -> IO ClUInt
devicePreferredVectorWidthLong dev = getDeviceInfo dev DevicePreferredVectorWidthLong

-- |Preferred native number of elements in float vectors
devicePreferredVectorWidthFloat :: Device -> IO ClUInt
devicePreferredVectorWidthFloat dev = getDeviceInfo dev DevicePreferredVectorWidthFloat

-- |The profile supported by the device
deviceProfile :: Device -> IO String
deviceProfile dev = getDeviceInfo dev DeviceProfile :: IO String

-- |The resolution in nanoseconds of the device timer
deviceProfilingTimerResolution :: Device -> IO CSize
deviceProfilingTimerResolution dev = getDeviceInfo dev DeviceProfilingTimerResolution

-- -- |The command queue properties supported by the device
-- deviceQueueProperties :: Device -> IO [CommandQueueProperties]
-- deviceQueueProperties dev =

-- |Describe the single precision floating point capability of the device
deviceSingleFPConfig :: Device -> IO [DeviceFPConfig]
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
deviceType :: Device -> IO [DeviceType]
deviceType dev = do
   cap <- (getDeviceInfo dev DeviceType :: IO {#type cl_device_type #})
   return . filter (\x -> cap .&. (fromIntegral $ fromEnum x) /= 0)
      $ [DeviceTypeDefault,
         DeviceTypeCpu,
         DeviceTypeGpu,
         DeviceTypeAccelerator,
         DeviceTypeAll]

-- |Obtain the device vendor name
deviceVendor :: Device -> IO String
deviceVendor dev = getDeviceInfo dev DeviceVendor

-- |Obtain a unique device vendor indentifier, which may be the PCIe ID.
deviceVendorID :: Device -> IO ClUInt
deviceVendorID dev = getDeviceInfo dev DeviceVendorID

-- |Obtain the OpenCL version supported by the device
deviceVersion :: Device -> IO String
deviceVersion dev = getDeviceInfo dev DeviceVersion

-- |Obtain the OpenCL software driver version string
deviceDriverVersion :: Device -> IO String
deviceDriverVersion dev = getDeviceInfo dev DriverVersion

