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
tests = testGroup "Command Queue"
        [ testCase "Obtain queue(s)" testCommandQueue
        , testCommandQueueProps
        ]

testCommandQueueProps = buildTest $ do
  platforms <- getPlatformIDs
  devices <- mapM (getDeviceIDs DeviceTypeAll) platforms
  let pds = zip (map ContextPlatform platforms) devices
  cs <- forM pds $ \(p, ds) -> createContext ds [p]
  let cds = [(c, d) | c <- cs, ds <- devices, d <- ds]
  let properties = [QueueProfilingEnable]
  queues <- forM cds $ \(c, d) -> createCommandQueue c d properties
  return $ testGroup "CommandQueue properties"
     [ testCase "queueContext"   $ forM_ (zip queues (map fst cds))
                                         testQueueContext
     , testCase "queueDevice"    $ forM_ (zip queues (map snd cds))
                                         testQueueDevice
     , testCase "queueProperties" $ forM_ queues
                                          (testQueueProperties properties)
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

testQueueContext (queue, context) = do
  context' <- queueContext queue
  context @=? context'

testQueueDevice (queue, device) = do
  device' <- queueDevice queue
  device @=? device'

testQueueProperties props queue = do
  props' <- queueProperties queue
  props @=? props'
