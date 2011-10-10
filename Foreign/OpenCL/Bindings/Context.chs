{-# LANGUAGE ForeignFunctionInterface, GADTs #-}
-- |
-- Module      : Foreign.OpenCL.Bindings.Context
-- Copyright   : (c) 2011, Martin Dybdal
-- License     : BSD3
-- 
-- Maintainer  : Martin Dybdal <dybber@dybber.dk>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- 
-- OpenCL bindings for contexts. Contexts are used by the OpenCL
-- runtime for managing objects such as command-queues, memory,
-- program and kernel objects and for executing kernels on one or more
-- devices specified in the context. See section 4.3 in the OpenCL
-- specification

module Foreign.OpenCL.Bindings.Context (
   createContext , createContextFromType, contextDevices, contextProperties, ContextCallback(..)
  ) where

#include <CL/cl.h>

import Control.Monad

import Foreign
import Foreign.C.Types
import Foreign.C.String

{# import Foreign.OpenCL.Bindings.Error #}
{# import Foreign.OpenCL.Bindings.Internal.Types #}
{# import Foreign.OpenCL.Bindings.Internal.Finalizers #}
import Foreign.OpenCL.Bindings.Internal.Util

-- | Used to specify the callback notification function that will
-- receive reports on errors in a context.
data ContextCallback a where
  NoContextCallback :: ContextCallback ()
  ContextCallback :: Storable a => a ->  (String -> a -> IO ()) -> ContextCallback a

-- |Create a new context that includes the given devices.
--
-- Information on errors that occur in the context are reported to the
-- optional callback function. The callback function may be called
-- asynchronously by the OpenCL implementation.
createContext :: [DeviceID] -- ^Devices to include in this context
              -> [ContextProperties] -- ^ Properties to set for the context
              -> ContextCallback a   -- ^ A callback notification function for error-reporting 
              -> IO Context -- ^The newly created context
createContext devs props callback =
  withArray0 0 (flattenContextProps props) $ \pps ->
  withArray devs $ \devp ->
  alloca $ \ep -> do
    let ndev = fromIntegral $ length devs
    ctx <- case callback of
             NoContextCallback -> 
               {#call clCreateContext #} pps ndev devp nullFunPtr nullPtr ep
             ContextCallback user_data fn -> 
               with user_data $ \user_data_ptr -> do
                 let ud_ptr = castPtr user_data_ptr :: Ptr ()
                 cb_funptr <- mkCallback fn
                 {#call clCreateContext #} pps ndev devp cb_funptr ud_ptr ep
    checkClError_ "clCreateContext" =<< peek ep
    attachContextFinalizer ctx

-- |Create a new context from a device type that identifies the
-- specific device(s) to include in the context.  
--
-- Information on errors that occur in the context are reported to the
-- optional callback function. The callback function may be called
-- asynchronously by the OpenCL implementation.
createContextFromType :: DeviceType -- ^Device type that identifies
                                    -- the individial device(s) to
                                    -- include in this context
                      -> [ContextProperties] -- ^ Properties to set for the context
                      -> ContextCallback a   -- ^ A callback notification function for error-reporting 
                      -> IO Context -- ^The newly created context
createContextFromType devtype properties callback =
  withArray0 0 (flattenContextProps properties) $ \props ->
  alloca $ \ep -> do
    let typ_num = (fromIntegral $ fromEnum devtype)
    ctx <- case callback of 
             NoContextCallback -> {#call clCreateContextFromType #} 
                                     props typ_num
                                     nullFunPtr nullPtr ep
             ContextCallback user_data fn -> 
               with user_data $ \user_data_ptr -> do
                 let ud_ptr = castPtr user_data_ptr :: Ptr ()
                 cb_funptr <- mkCallback fn
                 {#call clCreateContextFromType #} props typ_num cb_funptr ud_ptr ep
    checkClError_ "clCreateContextFromType" =<< peek ep
    attachContextFinalizer ctx

mkCallback :: Storable a 
           => (String -> a -> IO ())
           -> IO (FunPtr (Ptr CChar -> Ptr () -> CULong -> Ptr () -> IO ()))
mkCallback fn = wrapCallback $ 
  \errinfo _ _ user_data_ptr -> do
     err_str <- peekCAString errinfo
     user_data <- peek (castPtr user_data_ptr)
     fn err_str user_data  


foreign import ccall "wrapper" wrapCallback :: 
                (Ptr CChar -> Ptr () -> CULong -> Ptr () -> IO ())
  -> IO (FunPtr (Ptr CChar -> Ptr () -> CULong -> Ptr () -> IO ()))


flattenContextProps :: Num a => [ContextProperties] -> [a]
flattenContextProps = concatMap flatten
  where 
    flatten (ContextPlatform p) = [ fromIntegral $ fromEnum ClContextPlatform
                                  , fromIntegral $ ptrToWordPtr p]

-- | Obtain the devices included in the context
contextDevices :: Context -> IO [DeviceID]
contextDevices context =
  withForeignPtr context $ \ctx ->
    getInfo (clGetContextInfo_ ctx) ContextDevices

-- | Obtain the context properties defined for a context.
contextProperties :: Context -> IO [ContextProperties]
contextProperties context =
   withForeignPtr context $ \ctx ->
   alloca $ \sp -> do
      -- Get the size of the context properties region.
      _ <- clGetContextInfo_ ctx (fromIntegral $ fromEnum ContextProperties) 0 nullPtr sp
      -- Allocate some space for the properties
      size <- peek sp
      allocaBytes (fromIntegral size) $ \pp -> do
         _ <- clGetContextInfo_ ctx (fromIntegral $ fromEnum ContextProperties) (fromIntegral size) pp sp
         size' <- peek sp
         when (size /= fromIntegral size') $ error "Context properties size mismatch"
         let n = fromIntegral size `div` sizeOf (undefined :: {#type cl_context_properties #})
         return . assembleProps =<< peekArray n (castPtr pp)
      where
        assembleProps :: [Int] -> [ContextProperties]
        assembleProps [] = error "Unexpected end of context property list"
        assembleProps (x:_) | x == 0 = []
        assembleProps (x:xs) = let (y, xs') = assemble (toEnum x) xs
                               in y : assembleProps xs'
        assemble ClContextPlatform (x:xs') =
          let p = ContextPlatform . wordPtrToPtr $ fromIntegral x
          in (p, xs')
            -- TODO check non-exhaustive pattern warning

-- C interfacing functions
clGetContextInfo_ :: Ptr CContext -> CUInt -> CULong -> Ptr () -> Ptr CULong -> IO CInt
clGetContextInfo_ context name size value size_ret =
  checkClError "clGetContextInfo" =<< 
    {#call unsafe clGetContextInfo #} context name size value size_ret

