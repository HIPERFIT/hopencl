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

main = do
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
  