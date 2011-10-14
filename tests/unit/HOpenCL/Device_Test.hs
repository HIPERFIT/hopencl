module HOpenCL.Device_Test (tests) where

import Foreign.OpenCL.Bindings

import Test.HUnit hiding (Test, test)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework (testGroup, buildTest)

import Control.Monad (forM_, liftM)

import Test_Util (void)

--------------------
--   Test suite   --
--------------------
tests = testGroup "Device"
        [ testCase "getDeviceIDs" test_getDeviceIDs
        , testDeviceProperties
        ]

testDeviceProperties = buildTest $ do
  ps <- getPlatformIDs
  ds <- liftM concat $ mapM (getDeviceIDs [DeviceTypeAll]) ps
  return $ testGroup "Property getters"
    [ testCase "deviceAddressBits"        $ forM_ ds test_deviceAddressBits
    , testCase "deviceAvailable"          $ forM_ ds test_deviceAvailable
    , testCase "deviceCompilerAvailable"  $ forM_ ds test_deviceCompilerAvailable
    , testCase "deviceEndianLittle "      $ forM_ ds test_deviceEndianLittle
    , testCase "deviceErrorCorrectionSupport" $ forM_ ds test_deviceErrorCorrectionSupport
    , testCase "deviceExecutionCapabilities"  $ forM_ ds test_deviceExecutionCapabilities
    , testCase "deviceExtensions"         $ forM_ ds test_deviceExtensions

    , testCase "deviceGlobalMemCacheSize" $ forM_ ds test_deviceGlobalMemCacheSize
    , testCase "deviceGlobalMemCacheLineSize" $ forM_ ds test_deviceGlobalMemCacheLineSize
    , testCase "deviceGlobalMemSize"      $ forM_ ds test_deviceGlobalMemSize
    , testCase "deviceGlobalMemCacheType" $ forM_ ds test_deviceGlobalMemCacheType

    , testCase "deviceLocalMemSize"   $ forM_ ds test_deviceLocalMemSize
    , testCase "deviceLocalMemType"   $ forM_ ds test_deviceLocalMemType

    , testCase "deviceImageSupport"   $ forM_ ds test_deviceImageSupport

    , testCase "deviceImage2DMaxSize" $ forM_ ds test_deviceImage2DMaxSize
    , testCase "deviceImage3DMaxSize" $ forM_ ds test_deviceImage3DMaxSize

    , testCase "deviceMaxClockFrequency"  $ forM_ ds test_deviceMaxClockFrequency
    , testCase "deviceMaxComputeUnits"    $ forM_ ds test_deviceMaxComputeUnits
    , testCase "deviceMaxConstantArgs"    $ forM_ ds test_deviceMaxConstantArgs
    , testCase "deviceMaxConstantBufferSize" $ forM_ ds test_deviceMaxConstantBufferSize
    , testCase "deviceMaxMemAllocSize"    $ forM_ ds test_deviceMaxMemAllocSize
    , testCase "deviceMaxParameterSize"   $ forM_ ds test_deviceMaxParameterSize
    , testCase "deviceMaxReadImageArgs"   $ forM_ ds test_deviceMaxReadImageArgs
    , testCase "deviceMaxSamplers"        $ forM_ ds test_deviceMaxSamplers
    , testCase "deviceMaxWorkGroupSize"   $ forM_ ds test_deviceMaxWorkGroupSize
    , testCase "deviceMaxWorkItemDimensions" $ forM_ ds test_deviceMaxWorkItemDimensions
    , testCase "deviceMaxWriteImageArgs"  $ forM_ ds test_deviceMaxWriteImageArgs
    , testCase "deviceMemBaseAddrAlign"   $ forM_ ds test_deviceMemBaseAddrAlign
    , testCase "deviceMinDataTypeAlignSize"  $ forM_ ds test_deviceMinDataTypeAlignSize

    , testCase "devicePreferredVectorWidthChar"  $ forM_ ds test_devicePreferredVectorWidthChar
    , testCase "devicePreferredVectorWidthShort" $ forM_ ds test_devicePreferredVectorWidthShort
    , testCase "devicePreferredVectorWidthInt"   $ forM_ ds test_devicePreferredVectorWidthInt
    , testCase "devicePreferredVectorWidthLong"  $ forM_ ds test_devicePreferredVectorWidthLong
    , testCase "devicePreferredVectorWidthFloat" $ forM_ ds test_devicePreferredVectorWidthFloat

    , testCase "deviceName"            $ forM_ ds test_deviceName
    , testCase "devicePlatform"        $ forM_ ds test_devicePlatform
    , testCase "deviceProfile"         $ forM_ ds test_deviceProfile
    , testCase "deviceProfilingTimerResolution" $ forM_ ds test_deviceProfilingTimerResolution
    , testCase "deviceQueueProperties" $ forM_ ds test_deviceQueueProperties
    , testCase "deviceSingleFPConfig"  $ forM_ ds test_deviceSingleFPConfig
    , testCase "deviceType"            $ forM_ ds test_deviceType
    , testCase "deviceVendor"          $ forM_ ds test_deviceVendor
    , testCase "deviceVendorID"        $ forM_ ds test_deviceVendorID
    , testCase "deviceVersion"         $ forM_ ds test_deviceVersion
    , testCase "deviceDriverVersion"   $ forM_ ds test_deviceDriverVersion
    ]


-- Test that at least one device can be obtained for each platform
test_getDeviceIDs = do
  platforms <- getPlatformIDs
  devices <- mapM (getDeviceIDs [DeviceTypeAll]) platforms
  forM_ devices $ \d -> length d > 0 @? "Platform without device"

-- In the following our primary concern is checking that no errors are
-- thrown by the property accessors
test_deviceAddressBits = void . deviceAddressBits
test_deviceAvailable = void . deviceAvailable
test_deviceCompilerAvailable = void . deviceCompilerAvailable
test_deviceEndianLittle = void . deviceEndianLittle
test_deviceErrorCorrectionSupport = void . deviceErrorCorrectionSupport
test_deviceExecutionCapabilities = void . deviceExecutionCapabilities
test_deviceExtensions = void . deviceExtensions

test_deviceGlobalMemCacheSize = void . deviceGlobalMemCacheSize
test_deviceGlobalMemCacheLineSize = void . deviceGlobalMemCacheLineSize
test_deviceGlobalMemSize = void . deviceGlobalMemSize
test_deviceGlobalMemCacheType = void . deviceGlobalMemCacheType

test_deviceLocalMemSize = void . deviceLocalMemSize
test_deviceLocalMemType = void . deviceLocalMemType

test_deviceImageSupport = void . deviceImageSupport
test_deviceImage2DMaxSize = void . deviceImage2DMaxSize
test_deviceImage3DMaxSize = void . deviceImage3DMaxSize

test_deviceMaxClockFrequency = void . deviceMaxClockFrequency
test_deviceMaxComputeUnits d = do
  cunits <- deviceMaxComputeUnits d
  cunits >= 1 @? "Too few compute units (<1)"
test_deviceMaxConstantArgs = void . deviceMaxConstantArgs
test_deviceMaxConstantBufferSize = void . deviceMaxConstantBufferSize
test_deviceMaxMemAllocSize = void . deviceMaxMemAllocSize
test_deviceMaxParameterSize = void . deviceMaxParameterSize
test_deviceMaxReadImageArgs = void . deviceMaxReadImageArgs
test_deviceMaxSamplers = void . deviceMaxSamplers
test_deviceMaxWorkGroupSize d = do
  maxwgs <- deviceMaxWorkGroupSize d
  maxwgs >= 3 @? "Too small max work group size (<1)"
test_deviceMaxWorkItemDimensions d = do
  maxwidim <- deviceMaxWorkItemDimensions d
  maxwidim >= 3 @? "Too few work item dimensions (<3)"
test_deviceMaxWriteImageArgs = void . deviceMaxWriteImageArgs
test_deviceMemBaseAddrAlign = void . deviceMemBaseAddrAlign
test_deviceMinDataTypeAlignSize = void . deviceMinDataTypeAlignSize


test_devicePreferredVectorWidthChar = void . devicePreferredVectorWidthChar
test_devicePreferredVectorWidthShort = void . devicePreferredVectorWidthShort
test_devicePreferredVectorWidthInt = void . devicePreferredVectorWidthInt
test_devicePreferredVectorWidthLong = void . devicePreferredVectorWidthLong
test_devicePreferredVectorWidthFloat = void . devicePreferredVectorWidthFloat

test_deviceName = void . deviceName
test_devicePlatform = void . devicePlatform
test_deviceProfile = void . deviceProfile
test_deviceProfilingTimerResolution = void . deviceProfilingTimerResolution
test_deviceQueueProperties = void . deviceQueueProperties
test_deviceSingleFPConfig = void . deviceSingleFPConfig
test_deviceType = void . deviceType
test_deviceVendor = void . deviceVendor
test_deviceVendorID = void . deviceVendorID
test_deviceVersion = void . deviceVersion
test_deviceDriverVersion = void . deviceDriverVersion



