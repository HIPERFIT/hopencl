{-# LANGUAGE ForeignFunctionInterface #-}
#include <CL/cl.h>

module Foreign.OpenCL.Bindings.Event (
   createUserEvent,

   eventCommandQueue, eventContext, eventCommandType,
   
   setEventCompleteCallback, setUserEventStatus, wrapCallback
  ) where

import Control.Monad

import Foreign
import Foreign.C.Types

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
    checkClError_ "clCreateUserEvent" =<< peek ep
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
          user_data <- peek (castPtr user_data_ptr)
          callbackfn (toEnum $ fromIntegral status) user_data
      
-- TODO the second argument should be specified through a datatype
setUserEventStatus :: Event -> Int -> IO ()
setUserEventStatus event execution_status =
  withForeignPtr event $ \event_ptr -> do
    err <- {# call unsafe clSetUserEventStatus #} event_ptr (fromIntegral execution_status)
    checkClError_ "clSetUserEventStatus" err
  
foreign import ccall "wrapper" wrapCallback :: 
                (Ptr CEvent -> CInt -> Ptr () -> IO ())
  -> IO (FunPtr (Ptr CEvent -> CInt -> Ptr () -> IO ()))


-- C interfacing functions
clCreateUserEvent_ = {#call unsafe clCreateUserEvent #}

clGetEventInfo_ event name size value size_ret =
  checkClError "clGetEventInfo" =<<
    {#call unsafe clGetEventInfo #} event name size value size_ret

