module HOpenCL.Context_Test (tests) where

import Foreign.OpenCL.Bindings

import Test.HUnit hiding (Test, test)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework (testGroup, buildTest)

import Control.Monad (forM_, forM)

--------------------
--   Test suite   --
--------------------
tests = testGroup "Context"
        [ testCase "Obtain context(s)" testContext
        , testContextProps
        ]

testContextProps = buildTest $ do
  platforms <- getPlatformIDs
  devices <- mapM (getDeviceIDs [DeviceTypeAll]) platforms
  let pds = zip (map ContextPlatform platforms) devices
  cs <- forM pds $ \(p, ds) -> createContext ds [p] NoContextCallback
  return $ testGroup "Context properties"
     [ testCase "contextDevices"    $ forM_ (zip cs devices) testContextDevices
     , testCase "contextProperties" $ forM_ (zip cs platforms) testContextProperties
     ]

--------------------
-- Test functions --
--------------------

testContext = do
  platforms <- getPlatformIDs
  devices <- mapM (getDeviceIDs [DeviceTypeAll]) platforms
  let pds = zip (map ContextPlatform platforms) devices
  forM_ pds $ \(p, ds) -> createContext ds [p] NoContextCallback

testContextDevices (cs, devices) = do
  devices' <- contextDevices cs
  devices @=? devices'

testContextProperties (cs, platform) = do
  properties' <- contextProperties cs
  [ContextPlatform platform] @=? properties'
