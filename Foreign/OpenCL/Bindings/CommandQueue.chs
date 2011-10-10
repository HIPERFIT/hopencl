{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- Module      : Foreign.OpenCL.Bindings.CommandQueue
-- Copyright   : (c) 2011, Martin Dybdal
-- License     : BSD3
-- 
-- Maintainer  : Martin Dybdal <dybber@dybber.dk>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- 
-- OpenCL bindings for command-queues. Command Queues are used to
-- schedule operations on memory, program and kernel objects. Multiple
-- command queues can be used when synchronization between individual
-- tasks are not desired.
-- See section 5.1 in the OpenCL specification

module Foreign.OpenCL.Bindings.CommandQueue (
   createCommandQueue, queueContext, queueDevice, queueProperties, flush
  ) where

#include <CL/cl.h>

import Control.Applicative

import Foreign
import Foreign.C.Types

{# import Foreign.OpenCL.Bindings.Error #}
{# import Foreign.OpenCL.Bindings.Internal.Types #}
{# import Foreign.OpenCL.Bindings.Internal.Finalizers #}
import Foreign.OpenCL.Bindings.Internal.Util

-- |Create a new 'CommandQueue'
createCommandQueue :: Context 
                       -- ^ The 'Context' to which the 'CommandQueue'
                       -- should be associated
                   -> DeviceID 
                       -- ^ The 'DeviceID' that should process the
                       -- operations enqueued to the 'CommandQueue'
                   -> [CommandQueueProperties] 
                       -- ^ Can be used to specify out-of-order
                       -- execution and enable profiling
                   -> IO CommandQueue
createCommandQueue ctx dev props =
   withForeignPtr ctx $ \ctx_ptr ->
   alloca $ \ep -> do
      queue <- {#call unsafe clCreateCommandQueue #} ctx_ptr dev (enumToBitfield props) ep
      checkClError_ "clCreateCommandQueue" =<< peek ep
      attachCommandQueueFinalizer queue

-- | The Context to which this CommandQueue is associated
queueContext :: CommandQueue -> IO Context
queueContext queue = attachContextFinalizer =<< getCommandQueueInfo queue QueueContext

-- | The DeviceID of the device to which this CommandQueue is associated
queueDevice :: CommandQueue -> IO DeviceID
queueDevice queue = getCommandQueueInfo queue QueueDevice

-- | The list of CommandQueueProperties specified when this
-- CommandQueue was created.
queueProperties :: CommandQueue -> IO [CommandQueueProperties]
queueProperties queue =
   enumFromBitfield queue_props <$> (getCommandQueueInfo queue QueueProperties :: IO CInt)
 where
   queue_props = [QueueOutOfOrderExecModeEnable,
                  QueueProfilingEnable]

-- | Issues all previously queued OpenCL commands in command_queue to
-- the device associated with command_queue. 'flush' only guarantees
-- that all queued commands to command_queue get issued to the
-- appropriate device. There is no guarantee that they will be
-- complete after 'flush' returns.
flush :: CommandQueue -> IO ()
flush queue = 
  withForeignPtr queue $ \queue_ptr -> 
    checkClError_ "clFlush" =<< {# call unsafe clFlush #} queue_ptr

-- C interfacing functions
getCommandQueueInfo queue info =
    withForeignPtr queue $ \queue_ptr ->
    getInfo (clGetCommandQueueInfo_ queue_ptr) info
 where
   clGetCommandQueueInfo_ q name size value size_ret =
     checkClError "clGetCommandQueueInfo" =<< 
       {#call unsafe clGetCommandQueueInfo #} q name size value size_ret

