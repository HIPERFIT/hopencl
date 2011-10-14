module HOpenCL.Platform_Test (tests) where

import Foreign.OpenCL.Bindings

import Test.HUnit hiding (Test, test)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework (testGroup, buildTest)

import Control.Monad (forM_)

import Test_Util (oneOfM, void)

--------------------
--   Test suite   --
--------------------
tests = testGroup "Platform"
        [ testCase "getPlatformIDs" test_getPlatformIDs
        , testPlatformProperties
        ]

testPlatformProperties = buildTest $ do
  ps <- getPlatformIDs
  return $ testGroup "Property getters"
    [ testCase "platformVersion"    $ forM_ ps test_platformVersion
    , testCase "platformName"       $ forM_ ps test_platformName
    , testCase "platformProfile"    $ forM_ ps test_platformProfile
    , testCase "platformVendor"     $ forM_ ps test_platformVendor
    , testCase "platformExtensions" $ forM_ ps test_platformExtensions
    ]

--------------------
-- Test functions --
--------------------

-- Test that it possible to obtain at least one platform
test_getPlatformIDs = do
  platforms <- getPlatformIDs
  length platforms > 0 @? "No platforms found"

-- The primary test here is that no error is thrown, the rest is
-- checked through the types
test_platformVersion = void . platformVersion
test_platformName = void . platformName
test_platformProfile p = platformProfile p `oneOfM` ["FULL_PROFILE", "EMBEDDED_PROFILE"]
test_platformVendor = void . platformVendor
test_platformExtensions = void . platformExtensions
