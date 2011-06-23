{-# LANGUAGE ForeignFunctionInterface, NoMonomorphismRestriction #-}
#include <CL/cl.h>

module Foreign.OpenCL.Bindings.Platform (
  getPlatforms,

  platformProfile, platformVersion, platformName,
  platformVendor, platformExtensions
  )
where

import Foreign.ForeignPtr

{# import Foreign.OpenCL.Bindings.Types #}
{# import Foreign.OpenCL.Bindings.Import #}
import Foreign.OpenCL.Bindings.Util

-- ^Obtain a list of available OpenCL platforms.
getPlatforms :: IO [Platform]
getPlatforms = mapM attachPlatformFinalizer =<< getList clGetPlatformIDs_

platformProfile :: Platform -> IO String
platformProfile = getPlatformInfo PlatformProfile

platformVersion :: Platform -> IO String
platformVersion = getPlatformInfo PlatformVersion

platformName :: Platform -> IO String
platformName = getPlatformInfo PlatformName

platformVendor :: Platform -> IO String
platformVendor = getPlatformInfo PlatformVendor

platformExtensions :: Platform -> IO String
platformExtensions = getPlatformInfo PlatformExtensions

getPlatformInfo info platform =
   withForeignPtr platform $ \ptr ->
   getInfo (clGetPlatformInfo_ ptr) info
