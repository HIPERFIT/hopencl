module HOpenCL.MemoryObject_Test (tests) where

import Foreign.OpenCL.Bindings

import Test.HUnit hiding (Test, test)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework (testGroup, buildTest)

import Control.Monad (forM_, forM, liftM)

import Test_Util

--------------------
--   Test suite   --
--------------------
tests = testGroup "MemoryObject"
        [ testCase "mallocArray & free" test_mallocArray_free
        , testCase "allocaArray" test_allocaArray
        , testCase "newListArrayLen & peekListArray" test_newListArrayLen_peekListArray
        ]

list0 :: [ClInt]
list0 = [1..100]

--------------------
-- Test functions --
--------------------
test_mallocArray_free = do
  platforms <- getPlatformIDs
  cs <- forM platforms $ \p -> 
          createContextFromType DeviceTypeAll [ContextPlatform p] NoContextCallback
  forM_ cs $ \context -> do
    mobj <- mallocArray context [MemReadWrite] 100 :: IO (MemObject ClInt)
    free mobj

test_allocaArray = do
  platforms <- getPlatformIDs
  cs <- forM platforms $ \p -> 
          createContextFromType DeviceTypeAll [ContextPlatform p] NoContextCallback
  forM_ cs $ \context -> do
    let f :: MemObject ClInt -> IO ()
        f _ = return ()
    allocaArray context [] 1000 f

test_newListArrayLen_peekListArray = do
  platforms <- getPlatformIDs
  cs <- forM platforms $ \p -> 
          createContextFromType DeviceTypeAll [ContextPlatform p] NoContextCallback
  devs <- mapM (liftM head . contextDevices) cs
  forM_ (zip cs devs) $ \(context, device) -> do
    cq <- createCommandQueue context device []
    (mobj, len) <- newListArrayLen context list0
    list0' <- peekListArray cq len mobj
    list0 @=? list0'
