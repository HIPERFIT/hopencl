module HOpenCL.Device_Test (tests) where

import Foreign.OpenCL.Bindings

import Test.HUnit hiding (Test, test)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework (Test, testGroup, buildTest)

import Control.Monad (forM_, liftM)

import Test_Util (void)

--------------------
--   Test suite   --
--------------------
tests = testGroup "Device"
        [ testCase "Obtain device(s)" testDevice
        , testDeviceProperties
        ]

testDeviceProperties = buildTest $ do
  ps <- getPlatformIDs
  ds <- liftM concat $ mapM (getDeviceIDs DeviceTypeAll) ps
  return $ testGroup "Device properties"
    [ testCase "deviceAddressBits"        $ forM_ ds testDeviceAddressBits
    , testCase "deviceAvailable"          $ forM_ ds testDeviceAvailable
    , testCase "deviceCompilerAvailable"  $ forM_ ds testDeviceCompilerAvailable
    , testCase "deviceEndianLittle "      $ forM_ ds testDeviceEndianLittle
    , testCase "deviceErrorCorrectionSupport" $ forM_ ds testDeviceErrorCorrectionSupport
    , testCase "deviceExecutionCapabilities"  $ forM_ ds testDeviceExecutionCapabilities
    , testCase "deviceExtensions"         $ forM_ ds testDeviceExtensions

    , testCase "deviceGlobalMemCacheSize" $ forM_ ds testDeviceGlobalMemCacheSize
    , testCase "deviceGlobalMemCacheLineSize" $ forM_ ds testDeviceGlobalMemCacheLineSize
    , testCase "deviceGlobalMemSize"      $ forM_ ds testDeviceGlobalMemSize
    , testCase "deviceGlobalMemCacheType" $ forM_ ds testDeviceGlobalMemCacheType

    , testCase "deviceLocalMemSize"   $ forM_ ds testDeviceLocalMemSize
    , testCase "deviceLocalMemType"   $ forM_ ds testDeviceLocalMemType

    , testCase "deviceImageSupport"   $ forM_ ds testDeviceImageSupport

    , testCase "deviceImage2DMaxSize" $ forM_ ds testDeviceImage2DMaxSize
    , testCase "deviceImage3DMaxSize" $ forM_ ds testDeviceImage3DMaxSize

    , testCase "deviceMaxClockFrequency"  $ forM_ ds testDeviceMaxClockFrequency
    , testCase "deviceMaxComputeUnits"    $ forM_ ds testDeviceMaxComputeUnits
    , testCase "deviceMaxConstantArgs"    $ forM_ ds testDeviceMaxConstantArgs
    , testCase "deviceMaxConstantBufferSize" $ forM_ ds testDeviceMaxConstantBufferSize
    , testCase "deviceMaxMemAllocSize"    $ forM_ ds testDeviceMaxMemAllocSize
    , testCase "deviceMaxParameterSize"   $ forM_ ds testDeviceMaxParameterSize
    , testCase "deviceMaxReadImageArgs"   $ forM_ ds testDeviceMaxReadImageArgs
    , testCase "deviceMaxSamplers"        $ forM_ ds testDeviceMaxSamplers
    , testCase "deviceMaxWorkGroupSize"   $ forM_ ds testDeviceMaxWorkGroupSize
    , testCase "deviceMaxWorkItemDimensions" $ forM_ ds testDeviceMaxWorkItemDimensions
    , testCase "deviceMaxWriteImageArgs"  $ forM_ ds testDeviceMaxWriteImageArgs
    , testCase "deviceMemBaseAddrAlign"   $ forM_ ds testDeviceMemBaseAddrAlign
    , testCase "deviceMinDataTypeAlignSize"  $ forM_ ds testDeviceMinDataTypeAlignSize

    , testCase "devicePreferredVectorWidthChar"  $ forM_ ds testDevicePreferredVectorWidthChar
    , testCase "devicePreferredVectorWidthShort" $ forM_ ds testDevicePreferredVectorWidthShort
    , testCase "devicePreferredVectorWidthInt"   $ forM_ ds testDevicePreferredVectorWidthInt
    , testCase "devicePreferredVectorWidthLong"  $ forM_ ds testDevicePreferredVectorWidthLong
    , testCase "devicePreferredVectorWidthFloat" $ forM_ ds testDevicePreferredVectorWidthFloat

    , testCase "deviceName"            $ forM_ ds testDeviceName
    , testCase "devicePlatform"        $ forM_ ds testDevicePlatform
    , testCase "deviceProfile"         $ forM_ ds testDeviceProfile
    , testCase "deviceProfilingTimerResolution" $ forM_ ds testDeviceProfilingTimerResolution
    , testCase "deviceQueueProperties" $ forM_ ds testDeviceQueueProperties
    , testCase "deviceSingleFPConfig"  $ forM_ ds testDeviceSingleFPConfig
    , testCase "deviceType"            $ forM_ ds testDeviceType
    , testCase "deviceVendor"          $ forM_ ds testDeviceVendor
    , testCase "deviceVendorID"        $ forM_ ds testDeviceVendorID
    , testCase "deviceVersion"         $ forM_ ds testDeviceVersion
    , testCase "deviceDriverVersion"   $ forM_ ds testDeviceDriverVersion
    ]


-- Test that at least one device can be obtained for each platform
testDevice = do
  platforms <- getPlatformIDs
  devices <- mapM (getDeviceIDs DeviceTypeAll) platforms
  forM_ devices $ \d -> length devices > 0 @? "Platform without device"

-- In the following our primary concern is checking that no errors are
-- thrown by the property accessors
testDeviceAddressBits = void . deviceAddressBits
testDeviceAvailable = void . deviceAvailable
testDeviceCompilerAvailable = void . deviceCompilerAvailable
testDeviceEndianLittle = void . deviceEndianLittle
testDeviceErrorCorrectionSupport = void . deviceErrorCorrectionSupport
testDeviceExecutionCapabilities = void . deviceExecutionCapabilities
testDeviceExtensions = void . deviceExtensions

testDeviceGlobalMemCacheSize = void . deviceGlobalMemCacheSize
testDeviceGlobalMemCacheLineSize = void . deviceGlobalMemCacheLineSize
testDeviceGlobalMemSize = void . deviceGlobalMemSize
testDeviceGlobalMemCacheType = void . deviceGlobalMemCacheType

testDeviceLocalMemSize = void . deviceLocalMemSize
testDeviceLocalMemType = void . deviceLocalMemType

testDeviceImageSupport = void . deviceImageSupport
testDeviceImage2DMaxSize = void . deviceImage2DMaxSize
testDeviceImage3DMaxSize = void . deviceImage3DMaxSize

testDeviceMaxClockFrequency = void . deviceMaxClockFrequency
testDeviceMaxComputeUnits d = do
  cunits <- deviceMaxComputeUnits d
  cunits >= 1 @? "Too few compute units (<1)"
testDeviceMaxConstantArgs = void . deviceMaxConstantArgs
testDeviceMaxConstantBufferSize = void . deviceMaxConstantBufferSize
testDeviceMaxMemAllocSize = void . deviceMaxMemAllocSize
testDeviceMaxParameterSize = void . deviceMaxParameterSize
testDeviceMaxReadImageArgs = void . deviceMaxReadImageArgs
testDeviceMaxSamplers = void . deviceMaxSamplers
testDeviceMaxWorkGroupSize d = do
  maxwgs <- deviceMaxWorkGroupSize d
  maxwgs >= 3 @? "Too small max work group size (<1)"
testDeviceMaxWorkItemDimensions d = do
  maxwidim <- deviceMaxWorkItemDimensions d
  maxwidim >= 3 @? "Too few work item dimensions (<3)"
testDeviceMaxWriteImageArgs = void . deviceMaxWriteImageArgs
testDeviceMemBaseAddrAlign = void . deviceMemBaseAddrAlign
testDeviceMinDataTypeAlignSize = void . deviceMinDataTypeAlignSize


testDevicePreferredVectorWidthChar = void . devicePreferredVectorWidthChar
testDevicePreferredVectorWidthShort = void . devicePreferredVectorWidthShort
testDevicePreferredVectorWidthInt = void . devicePreferredVectorWidthInt
testDevicePreferredVectorWidthLong = void . devicePreferredVectorWidthLong
testDevicePreferredVectorWidthFloat = void . devicePreferredVectorWidthFloat

testDeviceName = void . deviceName
testDevicePlatform = void . devicePlatform
testDeviceProfile = void . deviceProfile
testDeviceProfilingTimerResolution = void . deviceProfilingTimerResolution
testDeviceQueueProperties = void . deviceQueueProperties
testDeviceSingleFPConfig = void . deviceSingleFPConfig
testDeviceType = void . deviceType
testDeviceVendor = void . deviceVendor
testDeviceVendorID = void . deviceVendorID
testDeviceVersion = void . deviceVersion
testDeviceDriverVersion = void . deviceDriverVersion



