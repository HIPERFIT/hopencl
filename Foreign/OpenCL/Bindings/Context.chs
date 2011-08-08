{-# LANGUAGE ForeignFunctionInterface #-}
#include <CL/cl.h>

module Foreign.OpenCL.Bindings.Context (
   createContext , contextDevices, contextProperties
  ) where

import Foreign
import Foreign.C.Types
import Foreign.Ptr

{#import Foreign.OpenCL.Bindings.Types #}
{#import Foreign.OpenCL.Bindings.Error #}
{#import Foreign.OpenCL.Bindings.Finalizers #}

-- |Create a new context
--
-- We do not currently support retrieving error reports through the
-- callback function. The function should also be changed such that a
-- list of ContextProperties can be supplied
createContext :: [DeviceID] -- ^Devices to include in this context
              -> IO Context -- ^The newly created context
createContext devs = let ndev = length devs in
   withArray devs $ \devp ->
   alloca $ \ep -> do
      ctx <- clCreateContext_ nullPtr (fromIntegral ndev) devp nullFunPtr nullPtr ep
      checkErrorA "clCreateContext" =<< peek ep
      attachContextFinalizer ctx

-- TODO
contextDevices :: Context -> IO [DeviceID]
contextDevices context = error "Retrieving devices attached to a context is still not supported."

contextProperties :: Context -> IO [ContextProperties]
contextProperties context = error "Retrieving the context properties is still not supported."

-- C interfacing functions
clCreateContext_ = {#call unsafe clCreateContext #}

clGetContextInfo_ context name size value size_ret =
  do errcode <- {#call unsafe clGetContextInfo #} context name size value size_ret
     checkErrorA "clGetContextInfo" errcode
     return errcode
