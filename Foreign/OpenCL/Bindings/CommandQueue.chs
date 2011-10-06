{-# LANGUAGE ForeignFunctionInterface #-}
#include <CL/cl.h>

module Foreign.OpenCL.Bindings.CommandQueue (
   createCommandQueue, queueContext, queueDevice, queueProperties
  ) where

import Control.Applicative

import Foreign
import Foreign.C.Types

{#import Foreign.OpenCL.Bindings.Types #}
{#import Foreign.OpenCL.Bindings.Error #}
{#import Foreign.OpenCL.Bindings.Finalizers #}

import Foreign.OpenCL.Bindings.Util

-- |Create a new commandqueue for a given device and context
--
createCommandQueue :: Context
              -> DeviceID
              -> [CommandQueueProperties]
              -> IO CommandQueue -- ^The newly created context
createCommandQueue ctx dev props =
   withForeignPtr ctx $ \ctx_ptr ->
   alloca $ \ep -> do
      ctx <- clCreateCommandQueue_ ctx_ptr dev (enumToBitfield props) ep
      checkClError_ "clCreateCommandQueue" =<< peek ep
      attachCommandQueueFinalizer ctx

getCommandQueueInfo queue info =
    withForeignPtr queue $ \queue_ptr ->
    getInfo (clGetCommandQueueInfo_ queue_ptr) info

queueContext :: CommandQueue -> IO Context
queueContext queue = attachContextFinalizer =<< getCommandQueueInfo queue QueueContext

queueDevice :: CommandQueue -> IO DeviceID
queueDevice queue = getCommandQueueInfo queue QueueDevice

queueProperties :: CommandQueue -> IO [CommandQueueProperties]
queueProperties queue =
   enumFromBitfield queue_props <$> (getCommandQueueInfo queue QueueProperties :: IO CInt)
 where
   queue_props = [QueueOutOfOrderExecModeEnable,
                  QueueProfilingEnable]

-- C interfacing functions
clCreateCommandQueue_ = {#call unsafe clCreateCommandQueue #}

clGetCommandQueueInfo_ queue name size value size_ret =
  checkClError "clGetCommandQueueInfo" =<< 
    {#call unsafe clGetCommandQueueInfo #} queue name size value size_ret

