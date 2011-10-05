{-# LANGUAGE ForeignFunctionInterface #-}
#include <CL/cl.h>

module Foreign.OpenCL.Bindings.Context (
   createContext , contextDevices, contextProperties
  ) where

import Control.Monad

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr

{# import Foreign.OpenCL.Bindings.Types #}
{# import Foreign.OpenCL.Bindings.Error #}
{# import Foreign.OpenCL.Bindings.Finalizers #}

import Foreign.OpenCL.Bindings.Util

-- |Create a new context that includes the given devices
--
-- We do not currently support retrieving error reports through the
-- callback function.
createContext :: [DeviceID] -- ^Devices to include in this context
              -> [ContextProperties]
              -> IO Context -- ^The newly created context
createContext devs props =
  withArray0 0 (flattenContextProps props) $ \pps ->
  withArray devs $ \devp ->
  alloca $ \ep -> do
    ctx <- {#call clCreateContext #} pps ndev devp nullFunPtr nullPtr ep
    checkErrorA "clCreateContext" =<< peek ep
    attachContextFinalizer ctx
       where
         ndev = fromIntegral $ length devs

-- |Create a new context that includes the given devices, and where errors are
--
-- We do not currently support retrieving error reports through the
-- callback function.
createContextWithCallback :: Storable a
                          => [DeviceID] -- ^Devices to include in this context
                          -> [ContextProperties]
                          -> a
                          -> (String -> a -> IO ())
                          -> IO Context -- ^The newly created context
createContextWithCallback devs props user_data callbackfn =
  withArray0 0 (flattenContextProps props) $ \pps ->
  withArray devs $ \devp ->
  with user_data $ \user_data_ptr ->
  alloca $ \ep -> do
    let ud_ptr = castPtr user_data_ptr :: Ptr ()
    cb_funptr <- wrapCallback callback
    ctx <- {#call clCreateContext #} pps ndev devp cb_funptr ud_ptr ep
    checkErrorA "clCreateContext" =<< peek ep
    attachContextFinalizer ctx
       where
         ndev = fromIntegral $ length devs
         callback errinfo _ _ user_data_ptr = do
           err_str <- peekCAString errinfo
           user_data <- peek (castPtr user_data_ptr)
           callbackfn err_str user_data
           
foreign import ccall "wrapper" wrapCallback :: 
                (Ptr CChar -> Ptr () -> CULong -> Ptr () -> IO ())
  -> IO (FunPtr (Ptr CChar -> Ptr () -> CULong -> Ptr () -> IO ()))


-- |Create a new context including devices of a given type.
--
-- We do not currently support retrieving error reports through the
-- callback function.
createContextFromType :: DeviceType -- ^Device type that identifies
                                    -- the individial device(s) to
                                    -- include in this context
                      -> [ContextProperties]
                      -> IO Context -- ^The newly created context
createContextFromType devtype props =
  withArray0 0 (flattenContextProps props) $ \props ->
  alloca $ \ep -> do
    ctx <- {#call unsafe clCreateContextFromType #} 
              props
              (fromIntegral $ fromEnum devtype)
              nullFunPtr nullPtr ep
    checkErrorA "clCreateContextFromType" =<< peek ep
    attachContextFinalizer ctx

flattenContextProps :: Num a => [ContextProperties] -> [a]
flattenContextProps = concatMap flatten
  where 
    flatten (ContextPlatform p) = [ fromIntegral $ fromEnum ClContextPlatform
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
clGetContextInfo_ :: Ptr CContext -> CUInt -> CULong -> Ptr () -> Ptr CULong -> IO CInt
clGetContextInfo_ context name size value size_ret =
  do errcode <- {#call unsafe clGetContextInfo #} context name size value size_ret
     checkErrorA "clGetContextInfo" errcode
     return errcode
