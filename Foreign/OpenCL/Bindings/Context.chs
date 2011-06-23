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
createContext :: [Device] -- ^Devices to include in this context
              -> IO ClContext -- ^The newly created context
createContext devs = let ndev = length devs in
   withForeignPtrs devs $ \ptrs ->
   withArray ptrs $ \devp ->
   alloca $ \ep -> do
      ctx <- clCreateContext_ nullPtr (fromIntegral ndev) devp nullFunPtr nullPtr ep
      checkErrorA "createContext" =<< peek ep
      return ctx

-- TODO
contextDevices :: Context -> IO [Device]
contextDevices context = error "Retrieving devices attached to a context is still not supported."

contextProperties :: Context -> IO [ContextProperties]
contextProperties context = error "Retrieving the context properties is still not supported."

-- C interfacing functions
clCreateContext_ = {#call unsafe clCreateContext #}

clGetContextInfo_ context name size value size_ret =
  do errcode <- {#call unsafe clGetContextInfo #} context name size value size_ret
     checkErrorA "clGetContextInfo" errcode
     return errcode
