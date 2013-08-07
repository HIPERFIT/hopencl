{-# LANGUAGE ForeignFunctionInterface, NoMonomorphismRestriction #-}
-- |
-- Module      : Foreign.OpenCL.Bindings.Platform
-- Copyright   : (c) 2011, Martin Dybdal
-- License     : BSD3
-- 
-- Maintainer  : Martin Dybdal <dybber@dybber.dk>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- OpenCL bindings for querying a list of available platforms and
-- information about those platforms. See section 4.1 in the OpenCL
-- specification

module Foreign.OpenCL.Bindings.Platform (
  getPlatformIDs,

  platformProfile, platformVersion, platformName,
  platformVendor, platformExtensions
  )
where

#include <CL/cl.h>

import Foreign.C.Types
import Foreign.Ptr

{# import Foreign.OpenCL.Bindings.Internal.Types #}
import Foreign.OpenCL.Bindings.Internal.Error
import Foreign.OpenCL.Bindings.Internal.Util
import qualified Foreign.OpenCL.Bindings.Internal.Logging as Log

-- | Obtain a list of available OpenCL platforms.
getPlatformIDs :: IO [PlatformID]
getPlatformIDs = do
  Log.debug "Invoking clGetPlatformIDs"
  getList clGetPlatformIDs_

-- | OpenCL profile string. See CL_PLATFORM_PROFILE in the OpenCL
-- specification for full documentation.
platformProfile :: PlatformID -> IO String
platformProfile pid = getPlatformInfo PlatformProfile pid

-- | OpenCL version string. See CL_PLATFORM_VERSION in the
-- OpenCL specification for full documentation.
platformVersion :: PlatformID -> IO String
platformVersion pid = getPlatformInfo PlatformVersion pid

-- | OpenCL name string
platformName :: PlatformID -> IO String
platformName pid = getPlatformInfo PlatformName pid

-- | OpenCL vendor string
platformVendor :: PlatformID -> IO String
platformVendor pid = getPlatformInfo PlatformVendor pid

-- | OpenCL extensions. Extensions defined here are supported by all
-- devices associated with this platform.
platformExtensions :: PlatformID -> IO [String]
platformExtensions pid = words `fmap` getPlatformInfo PlatformExtensions pid


-- Interfacing functions that performs error checking
getPlatformInfo info platform =
  getInfo (clGetPlatformInfo_ platform) info

clGetPlatformIDs_ num_entries platforms num_platforms = do
  checkClError "clGetPlatformIDs" =<< 
    {#call unsafe clGetPlatformIDs #} num_entries platforms num_platforms

clGetPlatformInfo_ platform name size value size_ret = do
  checkClError "clGetPlatformInfo" =<<
    {#call unsafe clGetPlatformInfo #} platform name size value size_ret

