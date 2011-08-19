{-# LANGUAGE ForeignFunctionInterface #-}
#include <CL/cl.h>

module Foreign.OpenCL.Bindings.Program (
   createProgram, buildProgram,
   programContext, programSource
  ) where

import Control.Applicative

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr

{# import Foreign.OpenCL.Bindings.Types #}
{# import Foreign.OpenCL.Bindings.Error #}
{# import Foreign.OpenCL.Bindings.Finalizers #}

import Foreign.OpenCL.Bindings.Util

-- | Create a program from a string containing the source code
--
createProgram :: Context -- ^The context to associate the program with
              -> String -- ^The program source
              -> IO Program -- ^The newly created program
createProgram ctx str =
   withForeignPtr ctx $ \ctx_ptr ->
   withCStringLen str $ \(cstr, len) ->
   with cstr $ \cstr_ptr -> -- clCreateProgramWithSource expects a list of strings
   with (fromIntegral len) $ \len_ptr -> -- and a list of their lengths
   alloca $ \ep -> do
      prog <- clCreateProgramWithSource_ ctx_ptr 1 cstr_ptr len_ptr ep
      checkErrorA "clCreateProgramWithSource" =<< peek ep
      attachProgramFinalizer prog

buildProgram :: Program -> [DeviceID] -> String -> IO ()
buildProgram p devs opts =
  withForeignPtr p $ \prog ->
  withArrayLen devs $ \n dev_ptr ->
  withCString opts $ \opt_ptr -> do
   err <- clBuildProgram_ prog (fromIntegral n)
                          dev_ptr opt_ptr nullFunPtr nullPtr
   if (toEnum (fromIntegral err) /= ClSuccess)
     then do
       log <- sequence $ getBuildInfo p <$> devs <*> [ProgramBuildLog]
       params <- sequence $ getBuildInfo p <$> devs <*> [ProgramBuildOptions]
       putStrLn "*************************** BUILD ERROR ***************************"
       putStrLn "Build failed when compiled with the following build options:"
       mapM_ putStrLn (params :: [String])
       putStrLn "\n***************************  BUILD LOG  ***************************"
       mapM_ putStrLn (log :: [String])
       putStrLn "*******************************************************************"
     else return ()
   checkErrorA "clBuildProgram" err

getProgramInfo program info =
    withForeignPtr program $ \program_ptr ->
    getInfo (clGetProgramInfo_ program_ptr) info

getBuildInfo program dev info =
    withForeignPtr program $ \program_ptr ->
    getInfo (clGetProgramBuildInfo_ program_ptr dev) info

programContext :: Program -> IO Context
programContext prog = attachContextFinalizer =<< getProgramInfo prog ProgramContext


programDevice :: Program -> IO [DeviceID]
programDevice prog = getProgramInfo prog ProgramDevices

programSource :: Program -> IO String
programSource prog = getProgramInfo prog ProgramSource

-- C interfacing functions
clCreateProgramWithSource_ = {#call unsafe clCreateProgramWithSource #}

clBuildProgram_ = {#call unsafe clBuildProgram #}

clGetProgramBuildInfo_ = {#call unsafe clGetProgramBuildInfo #}

clGetProgramInfo_ program name size value size_ret =
  do errcode <- {#call unsafe clGetProgramInfo #} program name size value size_ret
     checkErrorA "clGetProgramInfo" errcode
     return errcode
