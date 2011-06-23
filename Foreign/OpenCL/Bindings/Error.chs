{-# LANGUAGE ForeignFunctionInterface, DeriveDataTypeable #-}

#include <CL/cl.h>
#include <cl_enums.h>

module Foreign.OpenCL.Bindings.Error (
    OpenCLException(..), ClError(..),
    checkError, checkErrorA,
    throwError, assert
  )
where

{# import Foreign.OpenCL.Bindings.Types #}

import Control.Exception hiding (assert)
import Data.Typeable

{#enum ClError {} deriving (Show, Eq) #}

data OpenCLException = OpenCLException ClError (Maybe String)
     deriving Typeable

instance Exception OpenCLException

instance Show OpenCLException where
  show (OpenCLException error (Just loc)) =
    "OpenCL Exception: " ++ show error ++ " occurred in call to: " ++ loc
  show (OpenCLException error Nothing) =
    "OpenCL Exception: " ++ show error

checkError :: ClInt -> IO ()
checkError errcode =
  case decodeError errcode of
    ClSuccess -> return ()
    error -> throwIO (OpenCLException error Nothing)

-- ^ Check error an annotate it with a location
checkErrorA :: String -> ClInt -> IO ()
checkErrorA str errcode =
  case decodeError errcode of
    ClSuccess -> return ()
    error -> throwIO (OpenCLException error (Just str))


decodeError :: ClInt -> ClError
decodeError = toEnum . fromIntegral

-- TODO: Implement properly by extending data type
throwError :: String -> IO ()
throwError str = throwIO $ OpenCLException ClSuccess (Just str)

assert :: Bool -> String -> IO ()
assert True _    = return ()
assert False str = throwError str