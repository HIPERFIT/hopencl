{-# LANGUAGE ForeignFunctionInterface, GADTs #-}
#include <CL/cl.h>

module Foreign.OpenCL.Bindings.Kernel (
   createKernel,

   kernelContext, kernelFunctionName, kernelNumArgs,
   kernelWorkGroupSize, kernelLocalMemSize,
   kernelPreferredWorkGroupSizeMultiple, kernelPrivateMemSize,

   enqueueNDRangeKernel, enqueueTask,

   KernelArg(..), setKernelArg, setKernelArgs
  ) where

import Control.Applicative
import Control.Monad

import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

{# import Foreign.OpenCL.Bindings.Types #}
{# import Foreign.OpenCL.Bindings.Error #}
{# import Foreign.OpenCL.Bindings.Finalizers #}
{# import Foreign.OpenCL.Bindings.MemoryObject #}

import Foreign.OpenCL.Bindings.Util

-- | Create a program from a string containing the source code
--
createKernel :: Program -- ^The program that contains the kernel code
              -> String -- ^The name of the kernel (as written in the program source)
              -> IO Kernel -- ^The newly created kernel
createKernel prog name =
   withForeignPtr prog $ \prog_ptr ->
   withCString name $ \cstr ->
   alloca $ \ep -> do
      kernel <- clCreateKernel_ prog_ptr cstr ep
      checkErrorA "clCreateKernel" =<< peek ep
      attachKernelFinalizer kernel

enqueueNDRangeKernel :: CommandQueue -> Kernel -> [ClSize] -> [ClSize] -> [ClSize] -> [Event] -> IO Event
enqueueNDRangeKernel cq k globalWorkOffsets globalWorkSizes localWorkSizes waitEvs =
    withForeignPtr cq $ \queue ->
    withForeignPtr k $ \kernel ->
    withArrayNull globalWorkOffsets $ \globalWorkOffsetPtr ->
    withArrayNull globalWorkSizes $ \globalWorkSizePtr ->
    withArrayNull localWorkSizes $ \localWorkSizePtr ->
    withForeignPtrs waitEvs $ \event_ptrs ->
    withArrayNullLen event_ptrs $ \n event_array ->
    alloca $ \eventPtr ->
    do err <- clEnqueueNDRangeKernel_ queue kernel workDim
                                      globalWorkOffsetPtr
                                      globalWorkSizePtr
                                      localWorkSizePtr
                                      (fromIntegral n)
                                      event_array
                                      eventPtr
       checkErrorA "clEnqueueNDRangeKernel" err
       attachEventFinalizer =<< peek eventPtr
  where workDim = fromIntegral . maximum $ map length [globalWorkOffsets, globalWorkSizes, localWorkSizes]

data KernelArg where
  MObjArg :: MemObject a -> KernelArg
  VArg :: Storable a => a -> KernelArg

setKernelArg :: Kernel -> Int -> KernelArg -> IO ()
setKernelArg kernel n param =
  withForeignPtr kernel $ \k ->
  withPtr param $ \param_ptr -> do
    err <- clSetKernelArg_ k (fromIntegral n) (size param) param_ptr
    checkErrorA "clSetKernelArg" err
      where size (MObjArg mobj) = fromIntegral $ sizeOf (memobjPtr mobj)
            size (VArg v) = fromIntegral $ sizeOf v

            withPtr :: KernelArg -> (Ptr () -> IO c) -> IO c
            withPtr (MObjArg mobj) f = with (memobjPtr mobj) $ f . castPtr
            withPtr (VArg v) f = with v $ f . castPtr


setKernelArgs :: Kernel -> [KernelArg] -> IO ()
setKernelArgs kernel args = zipWithM_ (setKernelArg kernel) [0..] args


enqueueTask :: CommandQueue -> Kernel -> [Event] -> IO Event
enqueueTask cq k waitEvs =
    withForeignPtr cq $ \queue ->
    withForeignPtr k $ \kernel ->
    withForeignPtrs waitEvs $ \event_ptrs ->
    withArrayNullLen event_ptrs $ \n event_array ->
    alloca $ \eventPtr ->
    do err <- clEnqueueTask_ queue kernel
                             (fromIntegral n)
                             event_array
                             eventPtr
       checkErrorA "clEnqueueTask" err
       attachEventFinalizer =<< peek eventPtr

getKernelInfo kernel info =
    withForeignPtr kernel $ \kernel_ptr ->
    getInfo (clGetKernelInfo_ kernel_ptr) info

getKernelWorkGroupInfo kernel device info =
    withForeignPtr kernel $ \kernel_ptr ->
    getInfo (clGetKernelWorkGroupInfo_ kernel_ptr device) info

kernelContext :: Kernel -> IO Context
kernelContext kernel = attachContextFinalizer =<< getKernelInfo kernel KernelContext

kernelFunctionName :: Kernel -> IO String
kernelFunctionName kernel = getKernelInfo kernel KernelFunctionName

kernelNumArgs :: Kernel -> IO Int
kernelNumArgs kernel = getKernelInfo kernel KernelNumArgs

kernelWorkGroupSize :: Kernel -> DeviceID -> IO CSize
kernelWorkGroupSize kernel device =
  getKernelWorkGroupInfo kernel device KernelWorkGroupSize

kernelLocalMemSize :: Kernel -> DeviceID -> IO Word64
kernelLocalMemSize kernel device =
  getKernelWorkGroupInfo kernel device KernelLocalMemSize

kernelPreferredWorkGroupSizeMultiple :: Kernel -> DeviceID -> IO CSize
kernelPreferredWorkGroupSizeMultiple kernel device =
  getKernelWorkGroupInfo kernel device KernelPreferredWorkGroupSizeMultiple

kernelPrivateMemSize :: Kernel -> DeviceID -> IO Word64
kernelPrivateMemSize kernel device =
  getKernelWorkGroupInfo kernel device KernelPrivateMemSize

-- C interfacing functions
clCreateKernel_ = {#call unsafe clCreateKernel #}
clSetKernelArg_ = {#call unsafe clSetKernelArg #}

clEnqueueNDRangeKernel_ = {#call unsafe clEnqueueNDRangeKernel #}
clEnqueueTask_ = {#call unsafe clEnqueueTask #}

clGetKernelInfo_ kernel name size value size_ret =
  do errcode <- {#call unsafe clGetKernelInfo #} kernel name size value size_ret
     checkErrorA "clGetKernelInfo" errcode
     return errcode

clGetKernelWorkGroupInfo_ kernel device name size value size_ret =
  do errcode <- {#call unsafe clGetKernelWorkGroupInfo #} kernel device name size value size_ret
     checkErrorA "clGetKernelWorkGroupInfo" errcode
     return errcode
