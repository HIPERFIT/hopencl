module Foreign.OpenCL.Bindings.Internal.Error (
    checkClError, checkClError_, 
    checkClError5, checkClError6,
    throwError, assert
  )
where

import Foreign.OpenCL.Bindings.Internal.Types

import Control.Exception hiding (assert)

-- | Check for errors and annotate the error with a location
checkClError :: String -> ClInt -> IO ClInt
checkClError str errcode =
  case decodeError errcode of
    Success -> return errcode
    err -> throwIO (ClException err (Just str))

-- | Check for errors and annotate the error with a location
checkClError_ :: String -> ClInt -> IO ()
checkClError_ str errcode = checkClError str errcode >> return ()

-- | Check for errors when invoking a 5 argument function, an annotate
-- the error with a location.
checkClError5 :: String -> (a -> b -> c -> d -> e -> IO ClInt)
                        -> (a -> b -> c -> d -> e -> IO ClInt)
checkClError5 msg fn = \a0 a1 a2 a3 a4 -> checkClError msg =<< fn a0 a1 a2 a3 a4

-- | Check for errors when invoking a 6 argument function, an annotate
-- the error with a location.
checkClError6 :: String -> (a -> b -> c -> d -> e -> f -> IO ClInt)
                        -> (a -> b -> c -> d -> e -> f -> IO ClInt)
checkClError6 msg fn = \a0 a1 a2 a3 a4 a5 -> checkClError msg =<< fn a0 a1 a2 a3 a4 a5

decodeError :: ClInt -> ClError
decodeError = toEnum . fromIntegral

throwError :: String -> IO ()
throwError str = throwIO $ ClException Success (Just str)

assert :: Bool -> String -> IO ()
assert True _    = return ()
assert False str = throwError str