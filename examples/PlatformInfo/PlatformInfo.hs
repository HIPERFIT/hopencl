{-# LANGUAGE ExistentialQuantification #-}

import Control.Monad
import Control.Applicative

import Foreign.OpenCL.Bindings
import Foreign.Storable

import qualified Data.ByteString as B

platformInfoFn = [
  platformName,
  platformVendor,
  platformVersion,
  platformProfile,
  platformExtensions]

main = do
  ps <- getPlatformIDs
  putStrLn $ (show $ length ps) ++ " platform(s) found:"
  sequence_ [info p >>= putStrLn . ("    " ++) |
               p <- ps,
               info <- platformInfoFn]
  putStrLn ""
  devices <- liftM concat $ mapM (getDeviceIDs [DeviceTypeAll]) ps
  putStrLn $ (show $ length devices) ++ " devices(s) found:"
  forM devices $ \dev -> do
    putStrLn ""
    mapM (printDevice dev) devinfo


-- Device info functions
data DevInfo = forall a. Show a => DI String (DeviceID -> IO a)

printDevice device (DI name deviceInfoFn) = do
  info <- deviceInfoFn device
  putStrLn $ "    Device " ++ name ++ ": " ++ show info

devinfo = [
  DI "name" deviceName,
  DI "type" deviceType,
  DI "vendor" deviceVendor,
  DI "vendor id" deviceVendorID,
  DI "version" deviceVersion,
  DI "driver version" deviceDriverVersion,
  DI "profile" deviceProfile,
  DI "queue properties" deviceQueueProperties,
  DI "address bits" deviceAddressBits,
  DI "available" deviceAvailable,
  DI "compiler available" deviceCompilerAvailable,
  DI "endian little" deviceEndianLittle,
  DI "error correction support" deviceErrorCorrectionSupport,
  DI "execution capabilities" deviceExecutionCapabilities,
  DI "global mem cache size" deviceGlobalMemCacheSize,
  DI "global mem cache line size" deviceGlobalMemCacheLineSize,
  DI "global mem size" deviceGlobalMemSize,
  DI "global mem cache type" deviceGlobalMemCacheType,
  DI "local mem size" deviceLocalMemSize,
  DI "local mem type" deviceLocalMemType,
  DI "max clock frequency" deviceMaxClockFrequency,
  DI "max compute units" deviceMaxComputeUnits,
  DI "max constant args" deviceMaxConstantArgs,
  DI "max constant buffer size" deviceMaxConstantBufferSize,
  DI "max mem alloc size" deviceMaxMemAllocSize,
  DI "max parameter size" deviceMaxParameterSize,
  DI "max read image args" deviceMaxReadImageArgs,
  DI "max samplers" deviceMaxSamplers,
  DI "max work group size" deviceMaxWorkGroupSize,
  DI "max work item dimensions" deviceMaxWorkItemDimensions,
  DI "max work item sizes" deviceMaxWorkItemSizes,
  DI "max write image args" deviceMaxWriteImageArgs,
  DI "mem base addr align" deviceMemBaseAddrAlign,
  DI "min data type align size" deviceMinDataTypeAlignSize,
  DI "image support" deviceImageSupport,
  DI "image 2d max size" deviceImage2DMaxSize,
  DI "image 3d max size" deviceImage3DMaxSize,
  DI "profiling timer resolution" deviceProfilingTimerResolution,
  DI "preferred vector width char" devicePreferredVectorWidthChar,
  DI "preferred vector width short" devicePreferredVectorWidthShort,
  DI "preferred vector width int" devicePreferredVectorWidthInt,  
  DI "preferred vector width long" devicePreferredVectorWidthLong,
  DI "preferred vector width float" devicePreferredVectorWidthFloat,
  DI "extensions" deviceExtensions
  ]
