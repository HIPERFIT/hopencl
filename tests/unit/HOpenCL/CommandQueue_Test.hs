module HOpenCL.CommandQueue_Test (tests) where

import Foreign.OpenCL.Bindings

import Test.HUnit hiding (Test, test)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework (testGroup, buildTest)

import Control.Monad (forM_, forM)

--------------------
--   Test suite   --
--------------------
tests = testGroup "CommandQueue"
        [ testCase "createCommandQueue" test_createCommandQueue
        , testCommandQueueProps
        ]

testCommandQueueProps = buildTest $ do
  platforms <- getPlatformIDs
  devices <- mapM (getDeviceIDs [DeviceTypeAll]) platforms
  let pds = zip (map ContextPlatform platforms) devices
  cs <- forM pds $ \(p, ds) -> createContext ds [p] NoContextCallback
  let cds = [(c, d) | c <- cs, ds <- devices, d <- ds]
  let properties = [QueueProfilingEnable]
  queues <- forM cds $ \(c, d) -> createCommandQueue c d properties
  return $ testGroup "CommandQueue property getters"
     [ testCase "queueContext"   $ forM_ (zip queues (map fst cds))
                                         test_queueContext
     , testCase "queueDevice"    $ forM_ (zip queues (map snd cds))
                                         test_queueDevice
     , testCase "queueProperties" $ forM_ queues
                                          (test_queueProperties properties)
     ]

--------------------
-- Test functions --
--------------------

test_createCommandQueue = do
  platforms <- getPlatformIDs
  devices <- mapM (getDeviceIDs [DeviceTypeAll]) platforms
  let pds = zip (map ContextPlatform platforms) devices
  cs <- forM pds $ \(p, ds) -> createContext ds [p] NoContextCallback
  forM_ (zip cs devices) $ \(c, ds) ->
    forM ds $ \d ->
      createCommandQueue c d [] -- [QueueOutOfOrderExecModeEnable]

test_queueContext (queue, context) = do
  context' <- queueContext queue
  context @=? context'

test_queueDevice (queue, device) = do
  device' <- queueDevice queue
  device @=? device'

test_queueProperties props queue = do
  props' <- queueProperties queue
  props @=? props'
