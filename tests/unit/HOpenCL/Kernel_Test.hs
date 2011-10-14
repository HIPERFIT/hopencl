module HOpenCL.Kernel_Test (tests) where

import Foreign.OpenCL.Bindings

import Test.HUnit hiding (Test, test)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework (testGroup, buildTest)

import Control.Monad (forM)

import Test_Util

--------------------
--   Test suite   --
--------------------
tests = testGroup "Kernel"
        [ testCase "createKernel (simple)" test_createKernel_simple
        , testCase "createKernel (vectorAdd)" test_createKernel_vectorAdd
        , testKernelProps
        ]

----------------------
--   Test kernels   --
----------------------
kernel_vectorAdd = 
         "__kernel void vectorAdd(__global const float * a,"
      ++                         "__global const float * b, __global float * c) {"
      ++     "int nIndex = get_global_id(0);"
      ++     "c[nIndex] = a[nIndex] + b[nIndex];"
      ++ "};\n"

kernel_simple = 
         "__kernel void simple(const int a,"
      ++                       "const int b) {"
      ++ "}"

program = kernel_vectorAdd ++ kernel_simple

--------------------
-- Test functions --
--------------------
test_createKernel prog kernel_name = do
  platforms <- getPlatformIDs
  cs <- forM platforms $ \p -> 
          createContextFromType DeviceTypeAll [ContextPlatform p] NoContextCallback
  progs <- mapM (`createProgram` prog) cs
  devs <- mapM contextDevices cs
  let build_opts = ""
  mapM_ (($build_opts) . uncurry buildProgram) $ zip progs devs
  mapM_ (`createKernel` kernel_name) progs

test_createKernel_vectorAdd = test_createKernel program "vectorAdd"
test_createKernel_simple = test_createKernel program "simple"


testKernelProps = buildTest $ do
  platforms <- getPlatformIDs
  devices <- mapM (getDeviceIDs [DeviceTypeAll]) platforms
  let pds = zip (map ContextPlatform platforms) devices
  cs <- forM pds $ \(p, ds) -> createContext ds [p] NoContextCallback
  programs <- forM cs (`createProgram` program)
  let progdevs = [(p,ds) | p <- programs, ds <- devices]
  let build_opts = ""
  mapM_ (($build_opts) . uncurry buildProgram) progdevs
  kernels <- mapM (`createKernel` "vectorAdd") programs
  let kerneldevs = [(k,d) | k <- kernels, ds <- devices, d <- ds]
  return $ testGroup "Property getters"
     [ testCase "kernelContext"       $ mapM_ test_kernelContext (zip kernels cs) 
     , testCase "kernelFunctionName"  $ mapM_ test_kernelFunctionName (zip kernels $ repeat "vectorAdd") 
     , testCase "kernelNumArgs"       $ mapM_ test_kernelNumArgs (zip kernels $ repeat 3)
     , testCase "kernelWorkGroupSize" $ mapM_ test_kernelWorkGroupSize kerneldevs
     , testCase "kernelLocalMemSize"  $ mapM_ test_kernelLocalMemSize kerneldevs
     , testCase "kernelPrivateMemSize"$ mapM_ test_kernelPrivateMemSize kerneldevs
     , testCase "kernelPreferredWorkGroupSizeMultiple" $ mapM_ test_kernelPreferredWorkGroupSizeMultiple kerneldevs
     ]

test_kernelNumArgs (kernel, num) = do
  num' <- kernelNumArgs kernel
  num @=? num'

test_kernelFunctionName (kernel, name) = do
  name' <- kernelFunctionName kernel
  name @=? name'

test_kernelContext (kernel, context) = do
  context' <- kernelContext kernel
  context @=? context'

test_kernelWorkGroupSize = void . uncurry kernelWorkGroupSize
test_kernelLocalMemSize = void . uncurry kernelLocalMemSize
test_kernelPrivateMemSize = void . uncurry kernelPrivateMemSize
test_kernelPreferredWorkGroupSizeMultiple = void . uncurry kernelPreferredWorkGroupSizeMultiple

