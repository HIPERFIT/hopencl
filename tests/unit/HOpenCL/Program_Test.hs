module HOpenCL.Program_Test (tests) where

import Foreign.OpenCL.Bindings

import Test.HUnit hiding (Test, test)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework (testGroup, buildTest)

import Control.Monad (forM_, forM)

--------------------
--   Test suite   --
--------------------
tests = testGroup "Program"
        [ testCase "Obtain program(s)" testProgram
        , testCase "Build program(s)" testBuildProgram
        , testProgramProps
        ]

kernel = "__kernel void vectorAdd(__global const float * a,"
      ++                         "__global const float * b, __global float * c) {"
      ++     "int nIndex = get_global_id(0);"
      ++     "c[nIndex] = a[nIndex] + b[nIndex];"
      ++ "};"

testProgramProps = buildTest $ do
  platforms <- getPlatformIDs
  devices <- mapM (getDeviceIDs [DeviceTypeAll]) platforms
  let pds = zip (map ContextPlatform platforms) devices
  cs <- forM pds $ \(p, ds) -> createContext ds [p] NoContextCallback
  programs <- forM cs (flip createProgram kernel)
  let progdevs = [(p,ds) | p <- programs, ds <- devices]
  let build_opts = ""
  mapM_ (($build_opts) . uncurry buildProgram) progdevs
  return $ testGroup "Program properties"
     [ testCase "programDevice"  $ forM_ (zip programs devices) testProgramDevices
     , testCase "programContext" $ forM_ (zip programs cs) testProgramContext
     , testCase "programSource"  $ forM_ programs testProgramSource
     ]

--------------------
-- Test functions --
--------------------

testProgram = do
  platforms <- getPlatformIDs
  devices <- mapM (getDeviceIDs [DeviceTypeAll]) platforms
  let pds = zip (map ContextPlatform platforms) devices
  cs <- forM pds $ \(p, ds) -> createContext ds [p] NoContextCallback
  forM_ cs (`createProgram` kernel)

testBuildProgram = do
  platforms <- getPlatformIDs
  devices <- mapM (getDeviceIDs [DeviceTypeAll]) platforms
  let pds = zip (map ContextPlatform platforms) devices
  cs <- forM pds $ \(p, ds) -> createContext ds [p] NoContextCallback
  programs <- forM cs (`createProgram` kernel)
  let progdevs = [(p,ds) | p <- programs, ds <- devices]
  let build_opts = ""
  mapM_ (($build_opts) . uncurry buildProgram) progdevs
  return ()

testProgramDevices (program, devices) = do
  devices' <- programDevices program
  devices @=? devices'

testProgramContext (program, context) = do
  context' <- programContext program
  context @=? context'

testProgramSource program = do
  source' <- programSource program
  kernel @=? source'

