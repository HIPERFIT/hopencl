-- |
-- Module      : Foreign.OpenCL.Bindings
-- Copyright   : (c) 2011, Martin Dybdal
-- License     : BSD3
-- 
-- Maintainer  : Martin Dybdal <dybber@dybber.dk>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- 
-- OpenCL bindings. The only purpose of this module is to re-export
-- all other public modules of the package.
module Foreign.OpenCL.Bindings (
  module Foreign.OpenCL.Bindings.Types,

  module Foreign.OpenCL.Bindings.Platform,
  module Foreign.OpenCL.Bindings.Device,
  module Foreign.OpenCL.Bindings.Context,
  module Foreign.OpenCL.Bindings.CommandQueue,
  module Foreign.OpenCL.Bindings.Event,
  module Foreign.OpenCL.Bindings.Program,
  module Foreign.OpenCL.Bindings.Kernel,
  module Foreign.OpenCL.Bindings.MemoryObject
) where

import Foreign.OpenCL.Bindings.Types

import Foreign.OpenCL.Bindings.Platform
import Foreign.OpenCL.Bindings.Device
import Foreign.OpenCL.Bindings.Context
import Foreign.OpenCL.Bindings.CommandQueue
import Foreign.OpenCL.Bindings.Event
import Foreign.OpenCL.Bindings.Program
import Foreign.OpenCL.Bindings.Kernel
import Foreign.OpenCL.Bindings.MemoryObject