module HOpenCL.Program_Test (tests) where

import Foreign.OpenCL.Bindings

import Test.HUnit hiding (Test, test)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework (testGroup, buildTest)
import Test_Util (listSame)

import Control.Monad (forM_, forM)

--------------------
--   Test suite   --
--------------------
tests = testGroup "Program"
        [ testCase "createProgram" test_createProgram
        , testCase "buildProgram"  test_buildProgram
        , testProgramProps
        ]

kernel_vectorAdd = 
         "__kernel void vectorAdd(__global const float * a,"
      ++                         "__global const float * b, __global float * c) {"
      ++     "int nIndex = get_global_id(0);"
      ++     "c[nIndex] = a[nIndex] + b[nIndex];"
      ++ "};"

testProgramProps = buildTest $ do
  platforms <- getPlatformIDs
  devices <- mapM (getDeviceIDs [DeviceTypeAll]) platforms
  let pds = zip (map ContextPlatform platforms) devices
  cs <- forM pds $ \(p, ds) -> createContext ds [p] NoContextCallback
  programs <- forM cs (flip createProgram kernel_vectorAdd)
  let progdevs = [(p,ds) | p <- programs, ds <- devices]
  let build_opts = ""
  mapM_ (($build_opts) . uncurry buildProgram) progdevs
  return $ testGroup "Property getters"
     [ testCase "programDevice"  $ forM_ (zip programs devices) test_programDevices
     , testCase "programContext" $ forM_ (zip programs cs) test_programContext
     , testCase "programSource"  $ forM_ programs test_programSource
     ]

--------------------
-- Test functions --
--------------------
test_createProgram = do
  platforms <- getPlatformIDs
  devices <- mapM (getDeviceIDs [DeviceTypeAll]) platforms
  let pds = zip (map ContextPlatform platforms) devices
  cs <- forM pds $ \(p, ds) -> createContext ds [p] NoContextCallback
  forM_ cs (`createProgram` kernel_vectorAdd)

test_buildProgram = do
  platforms <- getPlatformIDs
  devices <- mapM (getDeviceIDs [DeviceTypeAll]) platforms
  let pds = zip (map ContextPlatform platforms) devices
  cs <- forM pds $ \(p, ds) -> createContext ds [p] NoContextCallback
  programs <- forM cs (`createProgram` kernel_vectorAdd)
  let progdevs = [(p,ds) | p <- programs, ds <- devices]
  let build_opts = ""
  mapM_ (($build_opts) . uncurry buildProgram) progdevs
  return ()

test_programDevices (program, devices) = do
  devices' <- programDevices program
  listSame devices devices'
--  devices @=? devices'

test_programContext (program, context) = do
  context' <- programContext program
  context @=? context'

test_programSource program = do
  source' <- programSource program
  kernel_vectorAdd @=? source'

