{-# LANGUAGE ForeignFunctionInterface #-}
#include <CL/cl.h>

module Foreign.OpenCL.Bindings.MemoryObject (
   MemObject(..),
   mallocArray, allocaArray, free,
   peekArray, peekListArray,
   pokeArray, pokeListArray,
   newListArray, newListArrayLen,
   withListArray, withListArrayLen,
   
   enqueueCopyBuffer,
   
   memobjType, memobjFlags, memobjSize, 
   memobjHostPtr, memobjMapCount, memobjContext
   ) where

import Control.Exception
import Control.Monad
import Data.Bits

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

createBuffer :: Context -> [MemFlags] -> Int -> Ptr a -> IO (MemObject a)
createBuffer context flags n value_ptr =
  withForeignPtr context $ \ctx ->
  F.alloca $ \ep -> do
    memobj <- {#call unsafe clCreateBuffer #}
                  ctx (enumToBitfield flags) (fromIntegral n)
                  (castPtr value_ptr) ep
    checkClError_ "clCreateBuffer" =<< peek ep
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
free dp = do err <- {#call unsafe clReleaseMemObject #} (memobjPtr dp)
             checkClError_ "clReleaseMemObject" err

peekArray :: Storable a => CommandQueue -> Int -> Int -> MemObject a -> Ptr a -> IO ()
peekArray queue offset n mobj ptr = doPeek undefined mobj >> return ()
  where
    doPeek :: Storable a' => a' -> MemObject a' -> IO Event
    doPeek x _ = enqueueReadBuffer queue mobj True
                                   (fromIntegral $ offset * sizeOf x)
                                   (fromIntegral $ n * sizeOf x)
                                   ptr
                                   []

peekListArray :: Storable a => CommandQueue -> Int -> MemObject a -> IO [a]
peekListArray queue n mobj =
  F.allocaArray n $ \p -> do
    peekArray   queue 0 n mobj p
    F.peekArray n p

pokeArray :: Storable a => CommandQueue -> Int -> Int -> Ptr a -> MemObject a -> IO ()
pokeArray queue offset n ptr mobj = doPoke undefined mobj >> return ()
  where
    doPoke :: Storable a' => a' -> MemObject a' -> IO Event
    doPoke x _ = enqueueWriteBuffer queue mobj True
                                    (fromIntegral $ offset * s)
                                    (fromIntegral $ n * s)
                                    ptr
                                    []
      where s = sizeOf x

pokeListArray :: Storable a => CommandQueue -> [a] -> MemObject a -> IO ()
pokeListArray queue xs mobj = F.withArrayLen xs $ \len p -> pokeArray queue 0 len p mobj

newListArrayLen :: Storable a => Context -> [a] -> IO (MemObject a, Int)
newListArrayLen context xs = create undefined xs
  where
    create :: Storable a' => a' -> [a'] -> IO (MemObject a', Int)
    create x xs = F.withArrayLen xs $ \len p -> do
                    mobj <- createBuffer context [MemCopyHostPtr] (sizeOf x * len) p
                    return (mobj, len)

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
enqueueReadBuffer q memobj block offset cb ptr event_wait_list =
  withForeignPtr q $ \queue ->
  withForeignPtrs event_wait_list $ \event_ptrs ->
  withArrayNullLen event_ptrs $ \n event_array -> do
  F.alloca $ \event -> do
    checkClError_ "clEnqueueReadBuffer" =<< 
      {#call unsafe clEnqueueReadBuffer #} 
          queue (memobjPtr memobj) (toOCLBool block) offset
          cb (castPtr ptr) (fromIntegral n) event_array event
    attachEventFinalizer =<< peek event

enqueueWriteBuffer :: Storable a => CommandQueue -> (MemObject a) -> Bool
                   -> ClSize -> ClSize -> Ptr a
                   -> [Event] -> IO Event
enqueueWriteBuffer q memobj block offset cb ptr event_wait_list =
  withForeignPtr q $ \queue ->
  withForeignPtrs event_wait_list $ \event_ptrs ->
  withArrayNullLen event_ptrs $ \n event_array -> do
  F.alloca $ \event -> do
    checkClError_ "clEnqueueWriteBuffer" =<< 
      {#call unsafe clEnqueueWriteBuffer #}
          queue (memobjPtr memobj) (toOCLBool block) offset
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

memobjType :: MemObject a -> IO MemObjectType
memobjType memobj = liftM toEnum $ getMemObjectInfo memobj MemType

memobjFlags :: MemObject a -> IO [MemFlags]
memobjFlags memobj = do
   flags <- (getMemObjectInfo memobj MemFlags)
   return . filter ((/=0) . (.&.) flags . fromEnum)
      $ [MemReadWrite, MemWriteOnly, MemReadOnly,
         MemUseHostPtr, MemAllocHostPtr, MemCopyHostPtr]

memobjSize :: MemObject a -> IO CSize
memobjSize memobj = getMemObjectInfo memobj MemSize

memobjHostPtr :: MemObject a -> IO (Ptr ())
memobjHostPtr memobj = getMemObjectInfo memobj MemHostPtr

memobjMapCount :: MemObject a -> IO Int
memobjMapCount memobj = getMemObjectInfo memobj MemMapCount

memobjContext :: MemObject a -> IO Context
memobjContext memobj = attachContextFinalizer =<< getMemObjectInfo memobj MemContext


-- C interfacing functions
getMemObjectInfo memobj = getInfo $ clGetMemObjectInfo_ (memobjPtr memobj)
  where
    clGetMemObjectInfo_ memobj name size value size_ret =
      checkClError "clGetMemObjectInfo" =<< 
        {#call unsafe clGetMemObjectInfo #} memobj name size value size_ret
