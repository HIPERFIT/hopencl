{-# LANGUAGE ForeignFunctionInterface #-}
#include <CL/cl.h>

module Foreign.OpenCL.Bindings.Synchronization (
  enqueueBarrier, enqueueMarker
  , enqueueWaitForEvents, waitForEvents
  , flush , finish
  ) where

import Foreign
import Foreign.C.Types

{#import Foreign.OpenCL.Bindings.Types #}
{#import Foreign.OpenCL.Bindings.Error #}
{#import Foreign.OpenCL.Bindings.Finalizers #}

import Foreign.OpenCL.Bindings.Util

enqueueMarker :: CommandQueue -> IO Event
enqueueMarker queue =
  withForeignPtr queue $ \queue_ptr ->
  alloca $ \evptr -> do
    checkClError_ "clEnqueueMarker" =<< 
      {# call unsafe clEnqueueMarker #} queue_ptr evptr
    attachEventFinalizer =<< peek evptr

enqueueBarrier :: CommandQueue -> IO ()
enqueueBarrier queue =
  withForeignPtr queue $ \queue_ptr ->
    checkClError_ "clEnqueueBarrier" =<< 
      {# call unsafe clEnqueueBarrier #} queue_ptr

enqueueWaitForEvents :: CommandQueue -> [Event] -> IO ()
enqueueWaitForEvents queue events =
  withForeignPtr queue $ \queue_ptr ->
  withForeignPtrs events $ \event_ptrs ->
  withArrayLen event_ptrs $ \n event_array ->
    checkClError_ "clEnqueueWaitForEvents" =<< 
      {# call unsafe clEnqueueWaitForEvents #} queue_ptr (fromIntegral n) event_array


waitForEvents :: [Event] -> IO ()
waitForEvents events =
  withForeignPtrs events $ \event_ptrs ->
  withArrayLen event_ptrs $ \n event_array -> do
    checkClError_ "clWaitForEvents" =<<
      {# call unsafe clWaitForEvents #} (fromIntegral n) event_array

-- It is probably misleading that this function resides in the
-- Synchronization module, as it returns immediately
flush :: CommandQueue -> IO ()
flush queue = 
  withForeignPtr queue $ \queue_ptr -> 
    checkClError_ "clFlush" =<< {# call unsafe clFlush #} queue_ptr

finish :: CommandQueue -> IO ()
finish queue = 
  withForeignPtr queue $ \queue_ptr -> 
    checkClError_ "clFinish" =<< {# call unsafe clFinish #} queue_ptr