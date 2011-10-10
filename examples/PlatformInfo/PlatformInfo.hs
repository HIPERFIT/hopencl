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

main :: IO ()
main = do
  ps <- getPlatformIDs
  putStrLn $ (show $ length ps) ++ " platform(s) found:"
  sequence_ [info p >>= putStrLn . ("    " ++) |
               p <- ps,
               info <- platformInfoFn]
  putStrLn ""
  devices <- liftM concat $ mapM (getDeviceIDs [DeviceTypeAll]) ps
  putStrLn $ (show $ length devices) ++ " devices(s) found:"
  forM_ devices $ \dev -> do
    putStrLn ""
    mapM (printDevice dev) devinfo

-- Device info functions
data DevInfo = forall a. Show a => D String (DeviceID -> IO a)

printDevice device (D name deviceInfoFn) = do
  info <- deviceInfoFn device
  putStrLn $ "    Device " ++ name ++ ": " ++ show info

devinfo = [
  D "name" deviceName,
  D "type" deviceType,
  D "vendor" deviceVendor,
  D "vendor id" deviceVendorID,
  D "version" deviceVersion,
  D "driver version" deviceDriverVersion,
  D "profile" deviceProfile,
  D "available" deviceAvailable,
  D "compiler available" deviceCompilerAvailable,  
  D "execution capabilities" deviceExecutionCapabilities,
  D "queue properties" deviceQueueProperties,
  
  D "max clock frequency" deviceMaxClockFrequency,
  D "max compute units" deviceMaxComputeUnits,
  D "max constant args" deviceMaxConstantArgs,
  D "max parameter size" deviceMaxParameterSize,
  D "max work group size" deviceMaxWorkGroupSize,
  D "max work item dimensions" deviceMaxWorkItemDimensions,
  D "max work item sizes" deviceMaxWorkItemSizes,
  
  D "global mem cache type" deviceGlobalMemCacheType,
  D "global mem cache size" deviceGlobalMemCacheSize,
  D "global mem cache line size" deviceGlobalMemCacheLineSize,
  D "global mem size" deviceGlobalMemSize,
  D "local mem size" deviceLocalMemSize,
  D "local mem type" deviceLocalMemType,
  D "max mem alloc size" deviceMaxMemAllocSize,
  D "max constant buffer size" deviceMaxConstantBufferSize,

  D "address bits" deviceAddressBits,
  D "little endian" deviceEndianLittle,
  D "mem base addr align" deviceMemBaseAddrAlign,
  D "min data type align size" deviceMinDataTypeAlignSize,
  D "preferred vector width char" devicePreferredVectorWidthChar,
  D "preferred vector width short" devicePreferredVectorWidthShort,
  D "preferred vector width int" devicePreferredVectorWidthInt,  
  D "preferred vector width long" devicePreferredVectorWidthLong,
  D "preferred vector width float" devicePreferredVectorWidthFloat,
  D "error correction support" deviceErrorCorrectionSupport,
  D "profiling timer resolution" deviceProfilingTimerResolution,
  
  D "image support" deviceImageSupport,
  D "max read image args" deviceMaxReadImageArgs,
  D "max write image args" deviceMaxWriteImageArgs,
  D "max samplers" deviceMaxSamplers,
  D "image 2d max size" deviceImage2DMaxSize,
  D "image 3d max size" deviceImage3DMaxSize,

  D "extensions" deviceExtensions
  ]
