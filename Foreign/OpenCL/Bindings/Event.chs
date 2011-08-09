{-# LANGUAGE ForeignFunctionInterface #-}
#include <CL/cl.h>

module Foreign.OpenCL.Bindings.Event (
   createUserEvent,

   eventCommandQueue, eventContext, eventCommandType
  ) where

import Control.Monad

import Foreign
import Foreign.C.Types
import Foreign.Ptr

{#import Foreign.OpenCL.Bindings.Types #}
{#import Foreign.OpenCL.Bindings.Error #}
{#import Foreign.OpenCL.Bindings.Finalizers #}

import Foreign.OpenCL.Bindings.Util

-- |Create a new user event
createUserEvent :: Context
                -> IO Event -- ^The newly created event
createUserEvent context =
  withForeignPtr context $ \ctx ->
  alloca $ \ep -> do
    event <- clCreateUserEvent_ ctx ep
    checkErrorA "clCreateUserEvent" =<< peek ep
    attachEventFinalizer event

getEventInfo event info =
    withForeignPtr event $ \event_ptr ->
    getInfo (clGetEventInfo_ event_ptr) info


eventCommandQueue :: Event -> IO CommandQueue
eventCommandQueue ev = attachCommandQueueFinalizer =<< getEventInfo ev EventCommandQueue

eventContext :: Event -> IO Context
eventContext ev = attachContextFinalizer =<< getEventInfo ev EventContext

eventCommandType :: Event -> IO CommandType
eventCommandType ev = liftM toEnum $ getEventInfo ev EventCommandType

-- C interfacing functions
clCreateUserEvent_ = {#call unsafe clCreateUserEvent #}

clGetEventInfo_ event name size value size_ret =
  do errcode <- {#call unsafe clGetEventInfo #} event name size value size_ret
     checkErrorA "clGetEventInfo" errcode
     return errcode
