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
  queues <- forM cds $ \(c, d) -> createCommandQueue c d [QueueProfilingEnable]
  return $ testGroup "CommandQueue properties"
     [ testCase "commandQueueContext"   $ forM_ (zip queues (map fst cds))
                                                testCommandQueueContext
     , testCase "commandQueueDevice"    $ forM_ (zip queues (map snd cds))
                                                testCommandQueueDevice
     , testCase "contextProperties" $ forM_ queues
                                            (testCommandQueueProperties [QueueProfilingEnable])
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

testCommandQueueContext (queue, context) = do
  context' <- queueContext queue
  context @=? context'

testCommandQueueDevice (queue, device) = do
  device' <- queueDevice queue
  device @=? device'

testCommandQueueProperties props queue = do
  props' <- queueProperties queue
  props @=? props'
