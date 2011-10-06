{-# LANGUAGE ForeignFunctionInterface, DeriveDataTypeable #-}

#include <CL/cl.h>
#include <cl_enums.h>

module Foreign.OpenCL.Bindings.Error (
    ClException(..), ClError(..),
    checkClError, checkClError_, throwError, assert
  )
where

{# import Foreign.OpenCL.Bindings.Types #} (ClInt)

import Control.Exception hiding (assert)
import Data.Typeable

{#enum ClError {} deriving (Show, Eq) #}

data ClException = ClException ClError (Maybe String)
     deriving Typeable

instance Exception ClException

instance Show ClException where
  show (ClException error (Just loc)) =
    "OpenCL Exception: " ++ show error ++ " occurred in call to: " ++ loc
  show (ClException error Nothing) =
    "OpenCL Exception: " ++ show error

-- ^ Check error an annotate it with a location
checkClError :: String -> ClInt -> IO ClInt
checkClError str errcode =
  case decodeError errcode of
    Success -> return errcode
    error -> throwIO (ClException error (Just str))
    

-- ^ Check error an annotate it with a location
checkClError_ :: String -> ClInt -> IO ()
checkClError_ str errcode = checkClError str errcode >> return ()



decodeError :: ClInt -> ClError
decodeError = toEnum . fromIntegral

-- TODO: Implement properly by extending data type
throwError :: String -> IO ()
throwError str = throwIO $ ClException Success (Just str)

assert :: Bool -> String -> IO ()
assert True _    = return ()
assert False str = throwError str