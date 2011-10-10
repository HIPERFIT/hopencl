{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- Module      : Foreign.OpenCL.Bindings.Synchronization
-- Copyright   : (c) 2011, Martin Dybdal
-- License     : BSD3
-- 
-- Maintainer  : Martin Dybdal <dybber@dybber.dk>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- 
-- Synchronization of command queues. See also
-- 'Foreign.OpenCL.Bindings.Event.waitForEvents'.

module Foreign.OpenCL.Bindings.Synchronization (
  enqueueBarrier, enqueueMarker
  , enqueueWaitForEvents, finish
  ) where

#include <CL/cl.h>

import Foreign
import Foreign.C.Types

{# import Foreign.OpenCL.Bindings.Error #}
{# import Foreign.OpenCL.Bindings.Internal.Types #}
{# import Foreign.OpenCL.Bindings.Internal.Finalizers #}
import Foreign.OpenCL.Bindings.Internal.Util

-- | Enqueues a marker into a command queue. This returns an 'Event'
-- which can be waited on, and will be fired as soon as all actions
-- enqueued before the marker has been completed.
enqueueMarker :: CommandQueue -> IO Event
enqueueMarker queue =
  withForeignPtr queue $ \queue_ptr ->
  alloca $ \evptr -> do
    checkClError_ "clEnqueueMarker" =<< 
      {# call unsafe clEnqueueMarker #} queue_ptr evptr
    attachEventFinalizer =<< peek evptr

-- | Enqueues a barrier. This is a synchronization point, that ensures
-- that all commands enqueued to the queue is completed before any
-- commands after the barrier is started.
-- This is only useful when using an out-of-order command queue.
enqueueBarrier :: CommandQueue -> IO ()
enqueueBarrier queue =
  withForeignPtr queue $ \queue_ptr ->
    checkClError_ "clEnqueueBarrier" =<< 
      {# call unsafe clEnqueueBarrier #} queue_ptr

-- | Enqueues a wait for a specific event or a list of events to
-- complete before any future commands queued in the command-queue are
-- executed.
enqueueWaitForEvents :: CommandQueue -> [Event] -> IO ()
enqueueWaitForEvents queue events =
  withForeignPtr queue $ \queue_ptr ->
  withForeignPtrs events $ \event_ptrs ->
  withArrayLen event_ptrs $ \n event_array ->
    checkClError_ "clEnqueueWaitForEvents" =<< 
      {# call unsafe clEnqueueWaitForEvents #} queue_ptr (fromIntegral n) event_array

-- | Blocks until all previously queued OpenCL commands in
-- command_queue are issued to the associated device and have
-- completed. 'finish' does not return until all queued commands in
-- command_queue have been processed and completed. 'finish' is also a
-- synchronization point.
finish :: CommandQueue -> IO ()
finish queue = 
  withForeignPtr queue $ \queue_ptr -> 
    checkClError_ "clFinish" =<< {# call unsafe clFinish #} queue_ptr