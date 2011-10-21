{-# LANGUAGE ForeignFunctionInterface, CPP #-}
-- |
-- Module      : Foreign.OpenCL.Bindings.Event
-- Copyright   : (c) 2011, Martin Dybdal
-- License     : BSD3
-- 
-- Maintainer  : Martin Dybdal <dybber@dybber.dk>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- 
-- OpenCL event handling.

module Foreign.OpenCL.Bindings.Event (
   createUserEvent, waitForEvents,

   eventCommandQueue, eventContext, eventCommandType,
   
   setEventCompleteCallback, setUserEventStatus
  ) where

#include <CL/cl.h>

import Control.Monad

import Foreign
import Foreign.C.Types

{# import Foreign.OpenCL.Bindings.Internal.Types #}
{# import Foreign.OpenCL.Bindings.Internal.Finalizers #}
import Foreign.OpenCL.Bindings.Internal.Error
import Foreign.OpenCL.Bindings.Internal.Util

-- |Create a new user event
createUserEvent :: Context
                -> IO Event -- ^The newly created event
createUserEvent context =
  withForeignPtr context $ \ctx ->
  alloca $ \ep -> do
    event <- {#call unsafe clCreateUserEvent #} ctx ep
    checkClError_ "clCreateUserEvent" =<< peek ep
    attachFinalizer event

getEventInfo event info =
    withForeignPtr event $ \event_ptr ->
    getInfo (clGetEventInfo_ event_ptr) info

eventCommandQueue :: Event -> IO CommandQueue
eventCommandQueue ev = 
  getEventInfo ev EventCommandQueue >>= attachRetainFinalizer

eventContext :: Event -> IO Context
eventContext ev = 
  getEventInfo ev EventContext >>= attachRetainFinalizer

eventCommandType :: Event -> IO CommandType
eventCommandType ev = liftM toEnum $ getEventInfo ev EventCommandType

-- ^ The OpenCL standard version 1.1 (page 146) mentions CL_COMPLETE
-- as the only command execution callback types where a callback can
-- be registered. This is the callback set by this function.
setEventCompleteCallback :: Storable a => Event -> a -> (CommandExecStatus -> a -> IO ()) -> IO ()
setEventCompleteCallback event user_data callbackfn =
  withForeignPtr event $ \event_ptr ->
  with user_data $ \user_data_ptr -> do
    let ud_ptr = castPtr user_data_ptr :: Ptr ()
    cb_ptr <- wrapCallback callback
    err <- {# call clSetEventCallback #} event_ptr (fromIntegral $ fromEnum Complete) cb_ptr ud_ptr
    checkClError_ "clSetEventCallback" err
      where
        -- We throw away the Event, as the event should be stored in the
        -- closure of the callback function if necessary
        callback :: Ptr CEvent -> CInt -> Ptr () -> IO ()
        callback _ status user_data_ptr = do
          udata <- peek (castPtr user_data_ptr)
          callbackfn (toEnum $ fromIntegral status) udata
      
-- TODO the second argument should be specified through a datatype
setUserEventStatus :: Event -> Int -> IO ()
setUserEventStatus event execution_status =
  withForeignPtr event $ \event_ptr -> do
    err <- {# call unsafe clSetUserEventStatus #} event_ptr (fromIntegral execution_status)
    checkClError_ "clSetUserEventStatus" err
  
foreign import CALLCONV "wrapper" wrapCallback :: 
                (Ptr CEvent -> CInt -> Ptr () -> IO ())
  -> IO (FunPtr (Ptr CEvent -> CInt -> Ptr () -> IO ()))


-- | Waits on the host thread for commands identified by event objects
-- in event_list to complete. A command is considered complete if its
-- execution status is CL_COMPLETE or a negative value.
waitForEvents :: [Event] -> IO ()
waitForEvents events =
  withForeignPtrs events $ \event_ptrs ->
  withArrayLen event_ptrs $ \n event_array -> do
    checkClError_ "clWaitForEvents" =<<
      {# call unsafe clWaitForEvents #} (fromIntegral n) event_array

-- C interfacing functions
clGetEventInfo_ = 
  checkClError5 "clGetEventInfo" {#call unsafe clGetEventInfo #}

