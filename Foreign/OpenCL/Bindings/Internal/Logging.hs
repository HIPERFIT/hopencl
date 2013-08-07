{-# LANGUAGE CPP #-}

module Foreign.OpenCL.Bindings.Internal.Logging (debug) where

import System.IO


debug :: String -> IO ()

#ifdef HOPENCL_DEBUG
debug msg = hPutStrLn stderr $ "DEBUG: " ++ msg
#else
{-# INLINE debug #-}
debug _ = return ()
#endif
