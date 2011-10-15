{-# LANGUAGE ForeignFunctionInterface, DeriveDataTypeable #-}
-- |
-- Module      : Foreign.OpenCL.Bindings.Error
-- Copyright   : (c) 2011, Martin Dybdal
-- License     : BSD3
-- 
-- Maintainer  : Martin Dybdal <dybber@dybber.dk>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Foreign.OpenCL.Bindings.Error (
    ClException(..), ClError(..),
    checkClError, checkClError_, 
    checkClError5, checkClError6,
    throwError, assert
  )
where

#include <CL/cl.h>
#include <cl_enums.h>
  
{# import Foreign.OpenCL.Bindings.Internal.Types #} (ClInt)

import Control.Exception hiding (assert)
import Data.Typeable

{#enum ClError {} deriving (Show, Eq) #}

data ClException = ClException ClError (Maybe String)
     deriving Typeable

instance Exception ClException

instance Show ClException where
  show (ClException err (Just loc)) =
    "OpenCL Exception: " ++ show err ++ " occurred in call to: " ++ loc
  show (ClException err Nothing) =
    "OpenCL Exception: " ++ show err

-- ^ Check error an annotate it with a location
checkClError :: String -> ClInt -> IO ClInt
checkClError str errcode =
  case decodeError errcode of
    Success -> return errcode
    err -> throwIO (ClException err (Just str))
    

-- ^ Check error an annotate it with a location
checkClError_ :: String -> ClInt -> IO ()
checkClError_ str errcode = checkClError str errcode >> return ()

checkClError5 :: String -> (a -> b -> c -> d -> e -> IO ClInt)
                        -> (a -> b -> c -> d -> e -> IO ClInt)
checkClError5 msg fn = \a0 a1 a2 a3 a4 -> checkClError msg =<< fn a0 a1 a2 a3 a4

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