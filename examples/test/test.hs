import Control.Monad
import Control.Applicative

import Foreign.OpenCL.Bindings
import Foreign.Storable

import qualified Data.ByteString as B

platformInfoFn = [
  platformName,
  platformVendor,
  platformVersion,
  platformProfile,
  platformExtensions]

deviceInfoFn = [
  deviceName,
  deviceVendor,
  deviceVersion,
  deviceDriverVersion,
  deviceProfile]

dim :: Int
dim = 100

array0, array1 :: [Float]
array0 = [1..fromIntegral dim]
array1 = reverse array0

main = do
  source <- readFile "kernel.cl"
  ps <- getPlatformIDs
  putStrLn $ (show $ length ps) ++ " platform(s) found:"
  sequence_ [info p >>= putStrLn . ("    " ++) |
               p <- ps,
               info <- platformInfoFn]
  devices <- liftM concat $ mapM (getDeviceIDs DeviceTypeAll) ps
  putStrLn $ (show $ length devices) ++ " devices(s) found:"
  sequence_ [info d >>= putStrLn . ("    " ++) |
               d <- devices,
               info <- deviceInfoFn]
  context <- createContext devices [ContextPlatform (head ps)]
  putStrLn "Context created"
  let device = head devices
  queue <- createCommandQueue context device [QueueOutOfOrderExecModeEnable]
  prog <- createProgram context source
  buildProgram prog [device] ""
  putStrLn "Program created and build"
  (devs, bins) <- unzip <$> programBinaries prog
  print devs
  print $ length bins
  B.writeFile "test1.clbin" $ head bins
  prog2 <- createProgramWithBinary context (zip devs bins)
  buildProgram prog2 [device] ""
  (devs2, bins2) <- unzip <$> programBinaries prog2
  print devs2
  print bins2
  B.writeFile "test2.clbin" $ head bins2
  kernel <- createKernel prog2 "vectorAdd"
  print "success"
  buffer0 <- newListArray context array0
  buffer1 <- newListArray context array1
  buffer_out <- mallocArray context [MemWriteOnly] dim
  setKernelArgs kernel [MObjArg buffer0, MObjArg buffer1, MObjArg buffer_out]
  enqueueNDRangeKernel queue kernel [] [fromIntegral dim] [] []
  list <- peekListArray queue dim buffer_out
  print (list :: [Float])
  putStrLn $ "Length: " ++ (show . length $ list)
  --putStrLn $ "Platforms: " ++ show platform
