 {-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

#include <CL/cl.h>
#include <cl_enums.h>

module Foreign.OpenCL.Bindings.Types (
  CPlatformID, CDeviceID, CContext, CCommandQueue, CProgram, CKernel, CEvent, CSampler,
  ClContext, ClCommandQueue, ClProgram, ClKernel, ClEvent, ClSampler, ClMem,

  PlatformID, DeviceID, Context, CommandQueue, Program, Kernel, Event, Sampler,

  ClChar, ClUChar, ClShort, ClUShort, ClInt, ClUInt, ClLong, ClULong, ClHalf,
  ClFloat, ClDouble,

  ClBitfield, ClBool, ClSize,

  clFalse, clTrue, toOCLBool,

  PlatformInfo(..), ContextProperties(..), ClContextProperties(..), ContextInfo(..),
  DeviceType(..), DeviceInfo(..), DeviceFPConfig(..), DeviceMemCacheType(..),
  DeviceLocalMemType(..), DeviceExecCapabilities(..),
  CommandQueueProperties(..), CommandQueueInfo(..),
  ProgramInfo(..), ProgramBuildInfo(..), KernelInfo(..), KernelWorkGroupInfo(..),
  EventInfo(..), CommandType(..),
  MemFlags(..), MemInfo(..), MemObjectType(..),
)
where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types

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

toOCLBool True = clTrue
toOCLBool False = clFalse


-- Platform
{#enum PlatformInfo {} deriving (Show, Eq) #}

-- Contexts
data ContextProperties = ContextPlatform PlatformID
                         deriving (Show,Eq)

{#enum ClContextProperties {} deriving (Show, Eq) #}


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

-- Program objects
{#enum ProgramInfo {} deriving (Show, Eq) #}
{#enum ProgramBuildInfo {} deriving (Show, Eq) #}
{#enum ClBuildStatus {} deriving (Show, Eq) #}

-- Kernels
{#enum KernelInfo {} deriving (Show, Eq) #}
{#enum KernelWorkGroupInfo {} deriving (Show, Eq) #}

-- Memory Objects
{#enum MemFlags {} deriving (Show, Eq) #}
{#enum MemObjectType {} deriving (Show, Eq) #}
{#enum MemInfo {} deriving (Show, Eq) #}
{#enum ClBufferCreateType {} deriving (Show, Eq) #}

-- Events
{#enum EventInfo {} deriving (Show, Eq) #}
{#enum CommandType {} deriving (Show, Eq) #}

-- Images
{#enum ClChannelOrder {} deriving (Show, Eq) #}
{#enum ClChannelType {} deriving (Show, Eq) #}
{#enum ClImageInfo {} deriving (Show, Eq) #}

-- Sampler
{#enum ClSamplerInfo {} deriving (Show, Eq) #}
{#enum ClFilterMode {} deriving (Show, Eq) #}
{#enum ClAddressingMode {} deriving (Show, Eq) #}

-- Other
{#enum ClMapFlags {} deriving (Show, Eq) #}
{#enum ClProfilingInfo {} deriving (Show, Eq) #}

