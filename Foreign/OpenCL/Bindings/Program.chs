{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- Module      : Foreign.OpenCL.Bindings.Program
-- Copyright   : (c) 2011, Martin Dybdal
-- License     : BSD3
-- 
-- Maintainer  : Martin Dybdal <dybber@dybber.dk>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- 
-- Loading and compilation of OpenCL programs.

module Foreign.OpenCL.Bindings.Program (
   createProgram, createProgramWithBinary,
   buildProgram, unloadCompiler,
   programContext, programDevices, programSource, programBinaries
  ) where

#include <CL/cl.h>

import Control.Applicative
import Control.Monad

import Foreign hiding (withMany)
import Foreign.C.Types
import Foreign.C.String

import qualified Data.ByteString as B

{# import Foreign.OpenCL.Bindings.Error #}
{# import Foreign.OpenCL.Bindings.Internal.Types #}
{# import Foreign.OpenCL.Bindings.Internal.Finalizers #}
import Foreign.OpenCL.Bindings.Internal.Util

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
      prog <- {#call unsafe clCreateProgramWithSource #} ctx_ptr 1 cstr_ptr len_ptr ep
      checkClError_ "clCreateProgramWithSource" =<< peek ep
      attachFinalizer prog

-- | Create a program from the ByteStrings obtained from
-- programBinaries
--
createProgramWithBinary :: Context -- ^The context to associate the program with
                        -> [(DeviceID, B.ByteString)] -- ^Binary program specific to different devices
                        -> IO Program -- ^The newly created program
createProgramWithBinary ctx devs_and_bins =
   let (devices, binaries) = unzip devs_and_bins
       lengths = map (fromIntegral . B.length) binaries
       words' = map (map fromIntegral . B.unpack) binaries
   in withForeignPtr ctx $ \ctx_ptr ->
      withArrayLen devices $ \n dev_arr ->
      allocaArray n $ \binary_status ->
      alloca $ \ep ->
      withArrays words' $ \bin_arr_list ->
      withArray bin_arr_list $ \bin_arr ->
      withArray lengths $ \length_arr -> do
        prog <- {#call unsafe clCreateProgramWithBinary #} 
                    ctx_ptr (fromIntegral n) dev_arr length_arr
                    bin_arr binary_status ep
        checkClError_ "createProgramWithBinary" =<< peek ep
        mapM_ (checkClError "createProgramWithBinary -") =<< peekArray n binary_status
        attachFinalizer prog

-- | Compile a program for a given set of devices.
buildProgram :: Program -- ^ The program to compile
             -> [DeviceID] -- ^ List of devices to compile for
             -> String
               -- ^ Compiler arguments, see OpenCL specification for
               -- possible values.
             -> IO ()
buildProgram p devs opts =
  withForeignPtr p $ \prog ->
  withArrayLen devs $ \n dev_ptr ->
  withCString opts $ \opt_ptr -> do
   err <- {#call unsafe clBuildProgram #}
              prog (fromIntegral n)
              dev_ptr opt_ptr nullFunPtr nullPtr
   if (toEnum (fromIntegral err) /= Success)
     then do
       buildlog <- sequence $ getBuildInfo p <$> devs <*> [ProgramBuildLog]
       params <- sequence $ getBuildInfo p <$> devs <*> [ProgramBuildOptions]
       putStrLn "*************************** BUILD ERROR ***************************"
       putStrLn "Build failed when compiled with the following build options:"
       mapM_ putStrLn (params :: [String])
       putStrLn "\n***************************  BUILD LOG  ***************************"
       mapM_ putStrLn (buildlog :: [String])
       putStrLn "*******************************************************************"
     else return ()
   checkClError_ "clBuildProgram" err

-- | Hints to the OpenCL implementation that the resources allocated
-- by the OpenCL compiler can be released. It does not guarantee that
-- the compiler will not be used in the future or that the compiler
-- will actually be unloaded by the implementation. Calls to
-- 'buildProgram' after 'unloadCompiler' will reload the compiler, if
-- necessary, to build the appropriate program executable.
unloadCompiler :: IO ()
unloadCompiler = checkClError_ "clUnloadCompiler" =<< {#call unsafe clUnloadCompiler #}

-- | The context to which the 'Program' is associated
programContext :: Program -> IO Context
programContext prog = 
  getProgramInfo prog ProgramContext >>= attachRetainFinalizer

-- | The devices to which the 'Program' is associated
programDevices :: Program -> IO [DeviceID]
programDevices prog = getProgramInfo prog ProgramDevices

-- | The source code of the 'Program' as specified in the call to
-- createProgram. If program is created using createProgramWithBinary,
-- an empty string or the appropriate program source code is returned
-- depending on whether or not the program source code is stored in
-- the binary.
programSource :: Program -> IO String
programSource prog = getProgramInfo prog ProgramSource

-- | Collects binaries for an unspecified subset of the devices
-- associated with the 'Program' (depending on which it is compiled
-- to)
programBinaries :: Program -> IO [(DeviceID, B.ByteString)]
programBinaries prog =
  withForeignPtr prog $ \program_ptr -> do
    devices  <- programDevices prog
    sizes    <- (getProgramInfo prog ProgramBinarySizes :: IO [ClSize])
    allocaArrays (map fromIntegral sizes) $ \ptrs ->
      withArrayLen ptrs $ \n ptrs_array -> do
        let info_code = fromIntegral $ fromEnum ProgramBinaries
            bytes = fromIntegral $ n * sizeOf (head ptrs)
        checkClError_ "programBinaries" =<< 
          clGetProgramInfo_ program_ptr info_code bytes
                            (castPtr ptrs_array) nullPtr
        bin_ptrs <- peekArray n ptrs_array
        binaries <- foldM readBinary [] $ zip sizes bin_ptrs
        return $ zip devices binaries
  where
    readBinary :: [B.ByteString] -> (ClSize, Ptr Word8) -> IO [B.ByteString]
    readBinary bs (0, _) = return bs
    readBinary bs (size, ptr) = do
      str <- B.pack <$> peekArray (fromIntegral size) ptr
      return $ str : bs

-- C interfacing functions
getProgramInfo program info =
    withForeignPtr program $ \program_ptr ->
    getInfo (clGetProgramInfo_ program_ptr) info
      
clGetProgramInfo_ program name size value size_ret =
  checkClError "clGetProgramInfo" =<< 
    {#call unsafe clGetProgramInfo #} program name size value size_ret

getBuildInfo program dev info =
    withForeignPtr program $ \program_ptr ->
    getInfo ({#call unsafe clGetProgramBuildInfo #} program_ptr dev) info

