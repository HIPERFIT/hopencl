 {-# LANGUAGE EmptyDataDecls, DeriveDataTypeable #-}
-- |
-- Module      : Foreign.OpenCL.Bindings.Internal.Types
-- Copyright   : (c) 2011, Martin Dybdal
-- License     : BSD3
-- 
-- Maintainer  : Martin Dybdal <dybber@dybber.dk>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Foreign.OpenCL.Bindings.Internal.Types (
  CPlatformID, CDeviceID, CContext, CCommandQueue, CProgram, CKernel, CEvent, CSampler,
  ClContext, ClCommandQueue, ClProgram, ClKernel, ClEvent, ClSampler, ClMem,

  PlatformID, DeviceID, Context, CommandQueue, Program, Kernel, Event, Sampler, MemObject(..),

  ClChar, ClUChar, ClShort, ClUShort, ClInt, ClUInt, ClLong, ClULong, ClHalf,
  ClFloat, ClDouble,

  ClBitfield, ClBool, ClSize,

  clFalse, clTrue, toOCLBool,
  
  ClException(..), ClError(..),

  PlatformInfo(..), ContextProperties(..), ClContextProperties(..), ContextInfo(..),
  DeviceType(..), DeviceInfo(..), DeviceFPConfig(..), DeviceMemCacheType(..),
  DeviceLocalMemType(..), DeviceExecCapabilities(..),
  CommandQueueProperties(..), CommandQueueInfo(..), CommandExecStatus(..),
  ProgramInfo(..), ProgramBuildInfo(..), KernelInfo(..), KernelWorkGroupInfo(..),
  EventInfo(..), CommandType(..),
  MemFlags(..), MemInfo(..), MemObjectType(..),
)
where

#ifdef __APPLE__
#include <OpenCL/cl.h>
#else
#include <CL/cl.h>
#endif

#include <cl_enums.h>
  
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types

import Control.Exception hiding (assert)
import Data.Typeable

-- Abstract types
data CPlatformID
data CDeviceID
data CContext
data CCommandQueue
data CMem
data CProgram
data CKernel
data CEvent
data CSampler

-- | Type representing OpenCL memory objects
data MemObject a = MemObject
   { memobjPtr :: ClMem -- ^Pointer to the underlying memory object
   }

{#pointer cl_platform_id as PlatformID -> CPlatformID #}
{#pointer cl_device_id as DeviceID -> CDeviceID #}
{#pointer cl_context as ClContext -> CContext #}
{#pointer cl_command_queue as ClCommandQueue -> CCommandQueue #}
{#pointer cl_mem as ClMem -> CMem #}
{#pointer cl_program as ClProgram -> CProgram #}
{#pointer cl_kernel as ClKernel -> CKernel #}
{#pointer cl_event as ClEvent -> CEvent #}
{#pointer cl_sampler as ClSampler -> CSampler #}

{#pointer cl_context as Context foreign -> CContext #}
{#pointer cl_command_queue as CommandQueue foreign -> CCommandQueue #}
{#pointer cl_program as Program foreign -> CProgram #}
{#pointer cl_kernel as Kernel foreign -> CKernel #}
{#pointer cl_event as Event foreign -> CEvent #}
{#pointer cl_sampler as Sampler foreign -> CSampler #}


-- Integral types
type ClChar = {#type cl_char #}
type ClUChar = {#type cl_uchar #}
type ClShort = {#type cl_short #}
type ClUShort = {#type cl_ushort #}
type ClInt = {#type cl_int #}
type ClUInt = {#type cl_uint #}
type ClLong = {#type cl_long #}
type ClULong = {#type cl_ulong #}
type ClHalf = {#type cl_half #}

-- Floating points
type ClFloat = {#type cl_float #}
type ClDouble = {#type cl_double #}

-- Other
type ClSize = {#type size_t #}
type ClBitfield = {#type cl_bitfield #}

-- Boolean values
{#enum ClBool {} deriving (Show, Eq) #}
clFalse, clTrue :: ClUInt
clFalse = fromIntegral $ fromEnum ClFalse
clTrue = fromIntegral $ fromEnum ClTrue

toOCLBool :: Bool -> ClUInt
toOCLBool True = clTrue
toOCLBool False = clFalse

-- Errors
{# enum ClError {} deriving (Show, Eq) #}

data ClException = ClException ClError (Maybe String)
     deriving Typeable

instance Exception ClException

instance Show ClException where
  show (ClException err (Just loc)) =
    "OpenCL Exception: " ++ show err ++ " occurred in call to: " ++ loc
  show (ClException err Nothing) =
    "OpenCL Exception: " ++ show err



-- Platform
{#enum PlatformInfo {} deriving (Show, Eq) #}

-- Contexts
{#enum ClContextProperties {} deriving (Show, Eq) #}

-- wrapper that contains the actual PlatformID
data ContextProperties = ContextPlatform PlatformID
                         deriving (Show,Eq)

{#enum ContextInfo {} deriving (Show, Eq) #}

-- Devices
{#enum DeviceType {} deriving (Show, Eq) #}
{#enum DeviceInfo {} deriving (Show, Eq) #}
{#enum DeviceFPConfig {} deriving (Show, Eq) #}
{#enum DeviceMemCacheType {} deriving (Show, Eq) #}
{#enum DeviceLocalMemType {} deriving (Show, Eq) #}
{#enum DeviceExecCapabilities {} deriving (Show, Eq) #}

-- Command Queue
{#enum CommandQueueInfo {} deriving (Show, Eq) #}
{#enum CommandQueueProperties {} deriving (Show, Eq) #}
{#enum ClCommandExecStatus {} deriving (Show, Eq) #}

-- wrapper that supports errornous states 
-- See page 145 in the OpenCL specification, v1.1

data CommandExecStatus = Complete
                       | Running
                       | Submitted
                       | Queued
                       | Error Int
                       deriving (Show,Eq)
instance Enum CommandExecStatus where
  fromEnum Complete = fromEnum ClComplete
  fromEnum Running = fromEnum ClRunning
  fromEnum Submitted = fromEnum ClSubmitted
  fromEnum Queued = fromEnum ClQueued
  fromEnum (Error n) = n

  toEnum 0 = Complete
  toEnum 1 = Running
  toEnum 2 = Submitted
  toEnum 3 = Queued
  toEnum n | n < 0 = Error n
           | otherwise = error ("CommandExecStatus.toEnum: Cannot match " ++ show n)

-- Program objects
{#enum ProgramInfo {} deriving (Show, Eq) #}
{#enum ProgramBuildInfo {} deriving (Show, Eq) #}
{#enum BuildStatus {} deriving (Show, Eq) #}

-- Kernels
{#enum KernelInfo {} deriving (Show, Eq) #}
{#enum KernelWorkGroupInfo {} deriving (Show, Eq) #}

-- Memory Objects
{#enum MemFlags {} deriving (Show, Eq) #}
{#enum MemObjectType {} deriving (Show, Eq) #}
{#enum MemInfo {} deriving (Show, Eq) #}
{#enum BufferCreateType {} deriving (Show, Eq) #}

-- Events
{#enum EventInfo {} deriving (Show, Eq) #}
{#enum CommandType {} deriving (Show, Eq) #}

-- Images
{#enum ClChannelOrder {} deriving (Show, Eq) #}
{#enum ClChannelType {} deriving (Show, Eq) #}
{#enum ImageInfo {} deriving (Show, Eq) #}

-- Sampler
{#enum SamplerInfo {} deriving (Show, Eq) #}
{#enum FilterMode {} deriving (Show, Eq) #}
{#enum AddressingMode {} deriving (Show, Eq) #}

-- Other
{#enum MapFlags {} deriving (Show, Eq) #}
{#enum ProfilingInfo {} deriving (Show, Eq) #}

