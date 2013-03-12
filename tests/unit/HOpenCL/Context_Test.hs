module HOpenCL.Context_Test (tests) where

import Foreign.OpenCL.Bindings

import Test.HUnit hiding (Test, test)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework (testGroup, buildTest)
import Test_Util (listSame)

import Control.Monad (forM_, forM)

--------------------
--   Test suite   --
--------------------
tests = testGroup "Context"
        [ testCase "createContext" test_createContext
        , testCase "createContextFromType" test_createContextFromType
        , testContextProps
        ]

testContextProps = buildTest $ do
  platforms <- getPlatformIDs
  devices <- mapM (getDeviceIDs [DeviceTypeAll]) platforms
  let pds = zip (map ContextPlatform platforms) devices
  cs <- forM pds $ \(p, ds) -> createContext ds [p] NoContextCallback
  return $ testGroup "Property getters"
     [ testCase "contextDevices"    $ forM_ (zip cs devices) test_contextDevices
     , testCase "contextProperties" $ forM_ (zip cs platforms) test_contextProperties
     ]

--------------------
-- Test functions --
--------------------

-- Test that no error is thrown on creation
test_createContext = do
  platforms <- getPlatformIDs
  devices <- mapM (getDeviceIDs [DeviceTypeAll]) platforms
  let pds = zip (map ContextPlatform platforms) devices
  forM_ pds $ \(p, ds) -> createContext ds [p] NoContextCallback

-- Test that no error is thrown on creation
test_createContextFromType = do
  platforms <- getPlatformIDs
  forM_ platforms $ 
    \p -> createContextFromType DeviceTypeAll [ContextPlatform p] NoContextCallback

-- Check that we obtain the same DeviceID as on creation
test_contextDevices (cs, devices) = do
  devices' <- contextDevices cs
  listSame devices devices'

-- Check that we obtain the same PlatformID as on creation
test_contextProperties (cs, platform) = do
  properties' <- contextProperties cs
  [ContextPlatform platform] @=? properties'
