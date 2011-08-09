{-# LANGUAGE ForeignFunctionInterface #-}
#include <CL/cl.h>

module Foreign.OpenCL.Bindings.MemoryObject (
   mallocArray, allocaArray, free,
--   peekArray, peekListArray,
--   pokeArray, pokeListArray
--  newListArray, newListArrayLen,
--  withListArray, withListArrayLen
   ) where

import Control.Exception

import Foreign.Storable
import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr
import qualified Foreign.Marshal as F

{#import Foreign.OpenCL.Bindings.Types #}
{#import Foreign.OpenCL.Bindings.Error #}
{#import Foreign.OpenCL.Bindings.Finalizers #}

import Foreign.OpenCL.Bindings.Util

-- | Type representing OpenCL memory objects
data MemObject a = MemObject
   { memobjPtr :: ClMem -- ^Pointer to the underlying memory object
   }

createBuffer :: Context -> [MemFlags] -> Int -> Ptr () -> IO (MemObject a)
createBuffer context flags n value_ptr =
  withForeignPtr context $ \ctx ->
  F.alloca $ \ep -> do
    memobj <- clCreateBuffer ctx (enumToBitfield flags) (fromIntegral n) value_ptr ep
    checkErrorA "clCreateBuffer" =<< peek ep
    return $ MemObject memobj

mallocArray :: Storable a => Context -> [MemFlags] -> Int -> IO (MemObject a)
mallocArray context flags n = doMalloc undefined
  where doMalloc :: Storable a' => a' -> IO (MemObject a')
        doMalloc x = createBuffer context flags (n * sizeOf x) nullPtr

allocaArray :: Storable a =>
               Context -> [MemFlags] ->
               Int -> (MemObject a -> IO b) -> IO b
allocaArray context flags n = bracket (mallocArray context flags n) free

free :: MemObject a -> IO ()
free dp = do err <- clReleaseMemObject_ (memobjPtr dp)
             checkErrorA "clReleaseMemObject" err
-- TODO
-- memobjType :: MemObject a -> IO MemObjectType
-- memobjType memobj = do
--    typ <- (getMemObjectInfo memobj clMemObjectType :: IO CLMemObjectType)
--    return $
--       case typ of
--          (#const CL_MEM_OBJECT_BUFFER) -> MemObjectBuffer
--          (#const CL_MEM_OBJECT_IMAGE2D) -> MemObjectImage2D
--          (#const CL_MEM_OBJECT_IMAGE3D) -> MemObjectImage3D
--          _ -> error "Illegal memory cache type"

-- memobjFlags :: MemObject a -> IO [MemObjectFlag]
-- memobjFlags memobj = do
--    cap <- (getMemObjectInfo memobj clMemObjectFlags :: IO CLMemObjectFlags)
--    return . filter ((/=) 0 . (.&.) cap . unMemObjectFlag)
--       $ [MemObjectReadWrite, MemObjectWriteOnly, MemObjectReadOnly,
--          MemObjectUseHostPtr, MemObjectAllocHostPtr, MemObjectCopyHostPtr]

memobjSize :: MemObject a -> IO CSize
memobjSize memobj = getMemObjectInfo memobj MemSize

memobjHostPtr :: MemObject a -> IO (Ptr ())
memobjHostPtr memobj = getMemObjectInfo memobj MemHostPtr

memobjMapCount :: MemObject a -> IO Int
memobjMapCount memobj = getMemObjectInfo memobj MemMapCount

memobjContext :: MemObject a -> IO Context
memobjContext memobj = attachContextFinalizer =<< getMemObjectInfo memobj MemContext


getMemObjectInfo memobj = getInfo $ clGetMemObjectInfo (memobjPtr memobj)


-- C interfacing functions
clCreateBuffer_ = {#call unsafe clCreateBuffer #}
clReleaseMemObject_ = {#call unsafe clReleaseMemObject #}



clGetMemObjectInfo_ memobj name size value size_ret =
  do err <- {#call unsafe clGetMemObjectInfo #} memobj name size value size_ret
     checkErrorA "clGetMemObjectInfo" err
     return err

