module HOpenCL.Platform_Test (tests) where

import Foreign.OpenCL.Bindings

import Test.HUnit hiding (Test, test)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework (Test, testGroup, buildTest)

import Control.Monad (forM_)

import Test_Util (oneOfM, void)

--------------------
--   Test suite   --
--------------------
tests = testGroup "Platform"
        [ testCase "Obtain platform(s)" testPlatform
        , testPlatformProperties
        ]

testPlatformProperties = buildTest $ do
  ps <- getPlatformIDs
  return $ testGroup "Platform properties"
    [ testCase "Platform Version"    $ forM_ ps testPlatformVersion
    , testCase "platformName"       $ forM_ ps testPlatformName
    , testCase "platformProfile"    $ forM_ ps testPlatformProfile
    , testCase "platformVendor"     $ forM_ ps testPlatformVendor
    , testCase "platformExtensions" $ forM_ ps testPlatformExtensions
    ]

--------------------
-- Test functions --
--------------------

-- Test that it possible to obtain at least one platform
testPlatform = do
  platforms <- getPlatformIDs
  length platforms > 0 @? "No platforms found"

-- The primary test here is that no error is thrown
testPlatformVersion = void . platformVersion
testPlatformName = void . platformName
testPlatformProfile p = (platformProfile p) `oneOfM` ["FULL_PROFILE", "EMBEDDED_PROFILE"]
testPlatformVendor = void . platformVendor
testPlatformExtensions = void . platformExtensions
