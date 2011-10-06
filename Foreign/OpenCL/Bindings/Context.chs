{-# LANGUAGE ForeignFunctionInterface, GADTs #-}
#include <CL/cl.h>

module Foreign.OpenCL.Bindings.Context (
   createContext , createContextFromType, contextDevices, contextProperties, ContextCallback(..)
  ) where

import Control.Monad

import Foreign
import Foreign.C.Types
import Foreign.C.String

{# import Foreign.OpenCL.Bindings.Types #}
{# import Foreign.OpenCL.Bindings.Error #}
{# import Foreign.OpenCL.Bindings.Finalizers #}

import Foreign.OpenCL.Bindings.Util

data ContextCallback a where
  NoContextCallback :: ContextCallback ()
  ContextCallback :: Storable a => a ->  (String -> a -> IO ()) -> ContextCallback a

-- |Create a new context that includes the given devices, and where errors are
--
-- We do not currently support retrieving error reports through the
-- callback function.
createContext :: [DeviceID] -- ^Devices to include in this context
              -> [ContextProperties]
              -> ContextCallback a
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

-- |Create a new context including devices of a given type.
--
-- We do not currently support retrieving error reports through the
-- callback function.
createContextFromType :: DeviceType -- ^Device type that identifies
                                    -- the individial device(s) to
                                    -- include in this context
                      -> [ContextProperties]
                      -> ContextCallback a
                      -> IO Context -- ^The newly created context
createContextFromType devtype props callback =
  withArray0 0 (flattenContextProps props) $ \props ->
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
        assembleProps (x:xs) | x == 0 = []
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

