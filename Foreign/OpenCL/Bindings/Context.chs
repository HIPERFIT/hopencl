{-# LANGUAGE ForeignFunctionInterface #-}
#include <CL/cl.h>

module Foreign.OpenCL.Bindings.Context (
   createContext , contextDevices, contextProperties
  ) where

import Control.Monad

import Foreign
import Foreign.C.Types
import Foreign.Ptr

{# import Foreign.OpenCL.Bindings.Types #}
{# import Foreign.OpenCL.Bindings.Error #}
{# import Foreign.OpenCL.Bindings.Finalizers #}

import Foreign.OpenCL.Bindings.Util

-- |Create a new context
--
-- We do not currently support retrieving error reports through the
-- callback function.
createContext :: [DeviceID] -- ^Devices to include in this context
              -> [ContextProperties]
              -> IO Context -- ^The newly created context
createContext devs props =
  withArray0 0 (flattenProps props) $ \pps ->
  withArray devs $ \devp ->
  alloca $ \ep -> do
    ctx <- clCreateContext_ pps (fromIntegral ndev) devp nullFunPtr nullPtr ep
    checkErrorA "clCreateContext" =<< peek ep
    attachContextFinalizer ctx
       where
         ndev = length devs
         flattenProps = concatMap flatten'
         flatten' (ContextPlatform p) = [ fromIntegral $ fromEnum ClContextPlatform
                                        , fromIntegral $ ptrToWordPtr p]

contextDevices :: Context -> IO [DeviceID]
contextDevices context =
  withForeignPtr context $ \ctx ->
    getInfo (clGetContextInfo_ ctx) ContextDevices

-- | Obtain the context properties associated with a device
contextProperties :: Context -> IO [ContextProperties]
contextProperties context =
   withForeignPtr context $ \ctx ->
   alloca $ \sp -> do
      -- Get the size of the context properties region.
      clGetContextInfo_ ctx (fromIntegral $ fromEnum ContextProperties) 0 nullPtr sp
      -- Allocate some space for the properties
      size <- peek sp
      allocaBytes (fromIntegral size) $ \pp -> do
         clGetContextInfo_ ctx (fromIntegral $ fromEnum ContextProperties) (fromIntegral size) pp sp
         size' <- peek sp
         when (size /= fromIntegral size') $ error "Context properties size mismatch"
         let n = fromIntegral size `div` sizeOf (undefined :: {#type cl_context_properties #})
         return . assembleProps =<< peekArray n (castPtr pp)
      where
        assembleProps :: [Int] -> [ContextProperties]
        assembleProps [] = error "Unexpected end of context property list"
        assembleProps (x:xs) | x == 0 = []
        assembleProps (x:xs) = let (y, xs') = assemble (toEnum x) xs
                               in y : assembleProps xs'
        assemble ClContextPlatform (x:xs') =
          let p = ContextPlatform . wordPtrToPtr $ fromIntegral x
          in (p, xs')

-- C interfacing functions
clCreateContext_ = {#call unsafe clCreateContext #}

clGetContextInfo_ context name size value size_ret =
  do errcode <- {#call unsafe clGetContextInfo #} context name size value size_ret
     checkErrorA "clGetContextInfo" errcode
     return errcode
