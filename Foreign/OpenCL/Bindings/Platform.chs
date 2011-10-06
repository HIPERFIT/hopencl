{-# LANGUAGE ForeignFunctionInterface, NoMonomorphismRestriction #-}
#include <CL/cl.h>

module Foreign.OpenCL.Bindings.Platform (
  getPlatformIDs,

  platformProfile, platformVersion, platformName,
  platformVendor, platformExtensions
  )
where

import Foreign.C.Types
import Foreign.Ptr

{# import Foreign.OpenCL.Bindings.Types #}
{# import Foreign.OpenCL.Bindings.Error #}
import Foreign.OpenCL.Bindings.Util

-- ^Obtain a list of available OpenCL platforms.
getPlatformIDs :: IO [PlatformID]
getPlatformIDs = getList clGetPlatformIDs_

platformProfile :: PlatformID -> IO String
platformProfile = getPlatformInfo PlatformProfile

platformVersion :: PlatformID -> IO String
platformVersion = getPlatformInfo PlatformVersion

platformName :: PlatformID -> IO String
platformName = getPlatformInfo PlatformName

platformVendor :: PlatformID -> IO String
platformVendor = getPlatformInfo PlatformVendor

platformExtensions :: PlatformID -> IO String
platformExtensions = getPlatformInfo PlatformExtensions


-- Interfacing functions that performs error checking
getPlatformInfo info platform =
  getInfo (clGetPlatformInfo_ platform) info

clGetPlatformIDs_ num_entries platforms num_platforms = do
  checkClError "clGetPlatformIDs" =<< 
    {#call unsafe clGetPlatformIDs #} num_entries platforms num_platforms

clGetPlatformInfo_ platform name size value size_ret = do
  checkClError "clGetPlatformInfo" =<<
    {#call unsafe clGetPlatformInfo #} platform name size value size_ret

