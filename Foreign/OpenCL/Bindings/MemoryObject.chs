{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- Module      : Foreign.OpenCL.Bindings.MemoryObject
-- Copyright   : (c) 2011, Martin Dybdal
-- License     : BSD3
-- 
-- Maintainer  : Martin Dybdal <dybber@dybber.dk>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- 
-- OpenCL bindings for manipulating memory objects.
-- See section 5.2 in the OpenCL specification

module Foreign.OpenCL.Bindings.MemoryObject (
   mallocArray, allocaArray, free,
   peekArray, peekListArray,
   pokeArray, pokeListArray,
   newListArray, newListArrayLen,
   withListArray, withListArrayLen,
   
   enqueueCopyBuffer,
   
   memobjType, memobjFlags, memobjSize, 
   memobjHostPtr, memobjMapCount, memobjContext
   ) where

#include <CL/cl.h>

import Control.Exception
import Control.Monad
import Data.Bits

import Foreign.Storable
import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr
import qualified Foreign.Marshal as F

{# import Foreign.OpenCL.Bindings.Error #}
{# import Foreign.OpenCL.Bindings.Internal.Types #}
{# import Foreign.OpenCL.Bindings.Internal.Finalizers #}
import Foreign.OpenCL.Bindings.Internal.Util

createBuffer :: Context -> [MemFlags] -> Int -> Ptr a -> IO (MemObject a)
createBuffer context flags n value_ptr =
  withForeignPtr context $ \ctx ->
  F.alloca $ \ep -> do
    memobj <- {#call unsafe clCreateBuffer #}
                  ctx (enumToBitfield flags) (fromIntegral n)
                  (castPtr value_ptr) ep
    checkClError_ "clCreateBuffer" =<< peek ep
    return $ MemObject memobj

-- | Allocates a device memory object.
mallocArray :: Storable a 
            => Context -- ^ The 'Context' to which the 'MemObject' should be associated.
            -> [MemFlags] -- ^ A list of 'MemFlags' determining permissions etc. for the 'MemObject'
            -> Int -- ^ The number of elements to allocate memory for.
            -> IO (MemObject a) -- ^ The type (and there by size) of elements is determined by the result type.
mallocArray context flags n = doMalloc undefined
  where doMalloc :: Storable a' => a' -> IO (MemObject a')
        doMalloc x = createBuffer context flags (n * sizeOf x) nullPtr

-- | Allocates a device memory object temporarily, and makes it
-- available for the argument function.
allocaArray :: Storable a 
            => Context -- ^ The 'Context' to which the 'MemObject' should be associated.
            -> [MemFlags] -- ^ A list of 'MemFlags' determining permissions etc. for the 'MemObject'
            -> Int -- ^ The number of elements to allocate memory for.
            -> (MemObject a -> IO b) -- ^ The function where the 'MemObject' should be available
            -> IO b
allocaArray context flags n = bracket (mallocArray context flags n) free

-- | Deallocates a memory object.
free :: MemObject a -> IO ()
free dp = do err <- {#call unsafe clReleaseMemObject #} (memobjPtr dp)
             checkClError_ "clReleaseMemObject" err

-- | Moves the content of a memory object from device to host exposing
-- it as C array.
peekArray :: Storable a 
          => CommandQueue -- ^ The 'CommandQueue' in which this action should be queued
          -> Int -- ^ The offset inside the 'MemObject' where the
                 -- first element should be read. (In number of
                 -- elements, not bytes)
          -> Int -- ^ The number of elements to read.
          -> MemObject a -- ^ The 'MemObject' to read from.
          -> Ptr a -- ^ A pointer where the elements read should be stored.
          -> IO ()
peekArray queue offset n mobj ptr = doPeek undefined mobj >> return ()
  where
    doPeek :: Storable a' => a' -> MemObject a' -> IO Event
    doPeek x _ = enqueueReadBuffer queue mobj True
                                   (fromIntegral $ offset * sizeOf x)
                                   (fromIntegral $ n * sizeOf x)
                                   ptr
                                   []

-- | Moves the content of a memory object from device to host exposing
-- it as Haskell List.
peekListArray :: Storable a 
              => CommandQueue 
              -> Int -- ^ The number of elements to read.
              -> MemObject a 
              -> IO [a]
peekListArray queue n mobj =
  F.allocaArray n $ \p -> do
    peekArray   queue 0 n mobj p
    F.peekArray n p

-- | Moves a host side C array to a device-side memory object
pokeArray :: Storable a 
          => CommandQueue 
          -> Int -- ^ The offset inside the 'MemObject' where the
                 -- first element should be written. (In number of
                 -- elements, not bytes)
          -> Int -- ^ The number of elements to write.
          -> Ptr a -- ^ A pointer where the elements should be read from
          -> MemObject a -- ^ The 'MemObject' to write the elements to
          -> IO ()
pokeArray queue offset n ptr mobj = doPoke undefined mobj >> return ()
  where
    doPoke :: Storable a' => a' -> MemObject a' -> IO Event
    doPoke x _ = enqueueWriteBuffer queue mobj True
                                    (fromIntegral $ offset * s)
                                    (fromIntegral $ n * s)
                                    ptr
                                    []
      where s = sizeOf x

-- | Moves a host side Haskell list to a device-side memory object
pokeListArray :: Storable a => CommandQueue -> [a] -> MemObject a -> IO ()
pokeListArray queue xs mobj = F.withArrayLen xs $ \len p -> pokeArray queue 0 len p mobj

-- | Create a memory object containing the specified list of elements,
-- returning the memory object together with the number of elements in
-- the list.
newListArrayLen :: Storable a => Context -> [a] -> IO (MemObject a, Int)
newListArrayLen context ys = create undefined ys
  where
    create :: Storable a' => a' -> [a'] -> IO (MemObject a', Int)
    create x xs = F.withArrayLen xs $ \len p -> do
                    mobj <- createBuffer context [MemCopyHostPtr] (sizeOf x * len) p
                    return (mobj, len)

-- | Create a memory object containing the specified list of elements.
newListArray :: Storable a => Context -> [a] -> IO (MemObject a)
newListArray context xs = fst `fmap` newListArrayLen context xs

withListArray :: Storable a => Context -> [a] -> (MemObject a -> IO b) -> IO b
withListArray context xs = withListArrayLen context xs . const

withListArrayLen :: Storable a => Context -> [a] -> (Int -> MemObject a -> IO b) -> IO b
withListArrayLen context xs f =
  bracket (newListArrayLen context xs) (free . fst) (uncurry . flip $ f)
--
-- XXX: Will this attempt to double-free the device array on error (together
-- with newListArrayLen)?
--

-- TODO implement copyArray using enqueueCopyBuffer


enqueueReadBuffer :: CommandQueue -> (MemObject a) -> Bool
                  -> ClSize -> ClSize -> Ptr a
                  -> [Event] -> IO Event
enqueueReadBuffer q memobj doblock offset cb ptr event_wait_list =
  withForeignPtr q $ \queue ->
  withForeignPtrs event_wait_list $ \event_ptrs ->
  withArrayNullLen event_ptrs $ \n event_array -> do
  F.alloca $ \event -> do
    checkClError_ "clEnqueueReadBuffer" =<< 
      {#call unsafe clEnqueueReadBuffer #} 
          queue (memobjPtr memobj) (toOCLBool doblock) offset
          cb (castPtr ptr) (fromIntegral n) event_array event
    attachEventFinalizer =<< peek event

enqueueWriteBuffer :: Storable a => CommandQueue -> (MemObject a) -> Bool
                   -> ClSize -> ClSize -> Ptr a
                   -> [Event] -> IO Event
enqueueWriteBuffer q memobj doblock offset cb ptr event_wait_list =
  withForeignPtr q $ \queue ->
  withForeignPtrs event_wait_list $ \event_ptrs ->
  withArrayNullLen event_ptrs $ \n event_array -> do
  F.alloca $ \event -> do
    checkClError_ "clEnqueueWriteBuffer" =<< 
      {#call unsafe clEnqueueWriteBuffer #}
          queue (memobjPtr memobj) (toOCLBool doblock) offset
          cb (castPtr ptr) (fromIntegral n) event_array event
    attachEventFinalizer =<< peek event

enqueueCopyBuffer :: CommandQueue -> (MemObject a) -> (MemObject a)
                  -> ClSize -> ClSize -> ClSize
                  -> [Event] -> IO Event
enqueueCopyBuffer q memobjSrc memobjDst offsetSrc offsetDst cb event_wait_list =
  withForeignPtr q $ \queue ->
  withForeignPtrs event_wait_list $ \event_ptrs ->
  withArrayNullLen event_ptrs $ \n event_array -> do
  F.alloca $ \event -> do
    checkClError_ "clEnqueueCopyBuffer" =<< 
      {#call unsafe clEnqueueCopyBuffer #}
          queue (memobjPtr memobjSrc) (memobjPtr memobjDst)
          offsetSrc offsetDst
          cb (fromIntegral n) event_array event
    attachEventFinalizer =<< peek event


-- | The type of a memory object
memobjType :: MemObject a -> IO MemObjectType
memobjType memobj = liftM toEnum $ getMemObjectInfo memobj MemType

-- | The flags specified when a memory object was allocated
memobjFlags :: MemObject a -> IO [MemFlags]
memobjFlags memobj = do
   flags <- (getMemObjectInfo memobj MemFlags)
   return . filter ((/=0) . (.&.) flags . fromEnum)
      $ [MemReadWrite, MemWriteOnly, MemReadOnly,
         MemUseHostPtr, MemAllocHostPtr, MemCopyHostPtr]

-- | The size of a memory object
memobjSize :: MemObject a -> IO CSize
memobjSize memobj = getMemObjectInfo memobj MemSize

memobjHostPtr :: MemObject a -> IO (Ptr ())
memobjHostPtr memobj = getMemObjectInfo memobj MemHostPtr

memobjMapCount :: MemObject a -> IO Int
memobjMapCount memobj = getMemObjectInfo memobj MemMapCount

-- | The 'Context' this memory object is associated with.
memobjContext :: MemObject a -> IO Context
memobjContext memobj = attachContextFinalizer =<< getMemObjectInfo memobj MemContext


-- C interfacing functions
getMemObjectInfo memobj = getInfo $ clGetMemObjectInfo_ (memobjPtr memobj)
  where
    clGetMemObjectInfo_ mobj name size value size_ret =
      checkClError "clGetMemObjectInfo" =<< 
        {#call unsafe clGetMemObjectInfo #} mobj name size value size_ret
