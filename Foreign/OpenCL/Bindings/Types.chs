-- |
-- Module      : Foreign.OpenCL.Bindings.Types
-- Copyright   : (c) 2011, Martin Dybdal
-- License     : BSD3
-- 
-- Maintainer  : Martin Dybdal <dybber@dybber.dk>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- 
-- OpenCL types

module Foreign.OpenCL.Bindings.Types (
  PlatformID, DeviceID, Context, CommandQueue, Program, Kernel, Event, Sampler, MemObject,

  ClChar, ClUChar, ClShort, ClUShort, ClInt, ClUInt, ClLong, ClULong, ClHalf,
  ClFloat, ClDouble,

  ClBitfield, ClBool, ClSize,

  clFalse, clTrue, toOCLBool,

  PlatformInfo(..), ContextProperties(..), ClContextProperties(..), ContextInfo(..),
  DeviceType(..), DeviceInfo(..), DeviceFPConfig(..), DeviceMemCacheType(..),
  DeviceLocalMemType(..), DeviceExecCapabilities(..),
  CommandQueueProperties(..), CommandQueueInfo(..), CommandExecStatus(..),
  ProgramInfo(..), ProgramBuildInfo(..), KernelInfo(..), KernelWorkGroupInfo(..),
  EventInfo(..), CommandType(..),
  MemFlags(..), MemInfo(..), MemObjectType(..),
)
where

{# import Foreign.OpenCL.Bindings.Internal.Types #}