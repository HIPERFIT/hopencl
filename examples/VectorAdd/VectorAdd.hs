import Control.Applicative
import Foreign.OpenCL.Bindings
import System.Mem

n :: Int
n = 100

list0, list1 :: [Float]
list0 = [1..fromIntegral n]
list1 = reverse list0

main :: IO ()
main = do
  -- Set up OpenCL context
  platform <- head <$> getPlatformIDs
  device <- head <$> getDeviceIDs [DeviceTypeAll] platform
  context <- createContext [device] [ContextPlatform platform] NoContextCallback
  queue <- createCommandQueue context device [QueueOutOfOrderExecModeEnable]
  
  -- Create OpenCL Kernel
  prog <- createProgram context =<< readFile "vector_add.cl"
  buildProgram prog [device] ""
  kernel <- createKernel prog "vectorAdd"
  
  -- Allocate device memory
  array0 <- newListArray context list0
  array1 <- newListArray context list1
  out <- mallocArray context [MemWriteOnly] n :: IO (MemObject Float)
    -- The type of the allocated output can not be infered without
    -- parsing the C code, so we need to specify it manually
  
  -- Enqueue kernel
  setKernelArgs kernel [MObjArg array0, MObjArg array1, MObjArg out]
  event <- enqueueNDRangeKernel queue kernel [] [fromIntegral n] [] []
  
  -- Read result
  print =<< peekListArray queue n out
  free array0
  free array1
  free out
