module HOpenCL.Context_Test (tests) where

import Foreign.OpenCL.Bindings

import Test.HUnit hiding (Test, test)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework (Test, testGroup, buildTest)

import Control.Monad (forM_, forM, liftM)

import Test_Util (oneOfM, void)

--------------------
--   Test suite   --
--------------------
tests = testGroup "Context"
        [ testCase "Obtain context(s)" testContext
        , testContextProps
        ]

testContextProps = buildTest $ do
  platforms <- getPlatformIDs
  devices <- mapM (getDeviceIDs DeviceTypeAll) platforms
  let pds = zip (map ContextPlatform platforms) devices
  cs <- forM pds $ \(p, ds) -> createContext ds [p]
  return $ testGroup "Context properties"
     [ testCase "contextDevices"    $ forM_ (zip cs devices) testContextDevices
     , testCase "contextProperties" $ forM_ (zip cs platforms) testContextProperties
     ]

--------------------
-- Test functions --
--------------------

testContext = do
  platforms <- getPlatformIDs
  devices <- mapM (getDeviceIDs DeviceTypeAll) platforms
  let pds = zip (map ContextPlatform platforms) devices
  cs <- forM pds $ \(p, ds) -> createContext ds [p]
  return ()


testContextDevices (cs, devices) = do
  devices' <- contextDevices cs
  devices @=? devices'

testContextProperties (cs, platform) = do
  properties' <- contextProperties cs
  [ContextPlatform platform] @=? properties'
