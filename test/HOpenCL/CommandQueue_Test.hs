module HOpenCL.CommandQueue_Test (tests) where

import Foreign.OpenCL.Bindings

import Test.HUnit hiding (Test, test)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework (Test, testGroup, buildTest)

import Control.Monad (forM_, forM, liftM)

import Test_Util (oneOfM, void)

--------------------
--   Test suite   --
--------------------
tests = testGroup "CommandQueue"
        [ testCase "Obtain context(s)" testCommandQueue
--        , testCommandQueueProps
        ]

testCommandQueueProps = buildTest $ do
  platforms <- getPlatformIDs
  devices <- mapM (getDeviceIDs DeviceTypeAll) platforms
  let pds = zip (map ContextPlatform platforms) devices
  cs <- forM pds $ \(p, ds) -> createContext ds [p]
  let cds = [(c, d) | c <- cs, ds <- devices, d <- ds]
  -- queues <- forM (zip cs devices) $ \(c, ds) ->
  --             forM ds $ \d ->
  --               createCommandQueue c d [QueueOutOfOrderExecModeEnable]
  queues <- forM cds $ \(c, d) -> createCommandQueue c d [QueueProfilingEnable]
  return $ testGroup "CommandQueue properties"
     [ --testCase "commandQueueDevice"    $ forM_ (zip cs cds) testCommandQueueDevice
--     , testCase "contextProperties" $ forM_ (zip cs platforms) testCommandQueueProperties
     ]

--------------------
-- Test functions --
--------------------

testCommandQueue = do
  platforms <- getPlatformIDs
  devices <- mapM (getDeviceIDs DeviceTypeAll) platforms
  let pds = zip (map ContextPlatform platforms) devices
  cs <- forM pds $ \(p, ds) -> createContext ds [p]
  queues <- forM (zip cs devices) $ \(c, ds) ->
              forM ds $ \d ->
                createCommandQueue c d [QueueOutOfOrderExecModeEnable]
  return ()

-- testCommandQueueDevices (cs, devices) = do
--   devices' <- contextDevices cs
--   devices @=? devices'

-- testCommandQueueProperties (cs, platform) = do
--   properties' <- contextProperties cs
--   [CommandQueuePlatform platform] @=? properties'
