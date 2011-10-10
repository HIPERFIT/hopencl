{-# LANGUAGE ForeignFunctionInterface, GADTs #-}
-- |
-- Module      : Foreign.OpenCL.Bindings.Kernel
-- Copyright   : (c) 2011, Martin Dybdal
-- License     : BSD3
-- 
-- Maintainer  : Martin Dybdal <dybber@dybber.dk>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- 
-- OpenCL kernel creation, invocation and scheduling.

module Foreign.OpenCL.Bindings.Kernel (
   createKernel,

   kernelContext, kernelFunctionName, kernelNumArgs,
   kernelWorkGroupSize, kernelLocalMemSize,
   kernelPreferredWorkGroupSizeMultiple, kernelPrivateMemSize,

   enqueueNDRangeKernel, enqueueTask,

   KernelArg(..), setKernelArg, setKernelArgs
  ) where

#include <CL/cl.h>

import Control.Monad

import Foreign
import Foreign.C.String
import Foreign.C.Types

{# import Foreign.OpenCL.Bindings.Error #}
{# import Foreign.OpenCL.Bindings.Internal.Types #}
{# import Foreign.OpenCL.Bindings.Internal.Finalizers #}
import Foreign.OpenCL.Bindings.Internal.Util

-- | Create a program from a string containing the source code
--
createKernel :: Program -- ^The program that contains the kernel code
              -> String -- ^The name of the kernel (as written in the program source)
              -> IO Kernel -- ^The newly created kernel
createKernel prog name =
   withForeignPtr prog $ \prog_ptr ->
   withCString name $ \cstr ->
   alloca $ \ep -> do
      kernel <- {# call unsafe clCreateKernel #} prog_ptr cstr ep
      checkClError_ "clCreateKernel" =<< peek ep
      attachKernelFinalizer kernel

enqueueNDRangeKernel :: CommandQueue 
                     -> Kernel
                     -> [ClSize] -- ^ Global work offsets
                     -> [ClSize] -- ^ Global work sizes
                     -> [ClSize] -- ^ Local work sizes
                     -> [Event] 
                     -> IO Event
enqueueNDRangeKernel cq k globalWorkOffsets globalWorkSizes localWorkSizes waitEvs =
    withForeignPtr cq $ \queue ->
    withForeignPtr k $ \kernel ->
    withArrayNull globalWorkOffsets $ \globalWorkOffsetPtr ->
    withArrayNull globalWorkSizes $ \globalWorkSizePtr ->
    withArrayNull localWorkSizes $ \localWorkSizePtr ->
    withForeignPtrs waitEvs $ \event_ptrs ->
    withArrayNullLen event_ptrs $ \n event_array ->
    alloca $ \eventPtr ->
    do checkClError_ "clEnqueueNDRangeKernel" =<< 
         {# call unsafe clEnqueueNDRangeKernel #} 
                   queue kernel workDim
                   globalWorkOffsetPtr
                   globalWorkSizePtr
                   localWorkSizePtr
                   (fromIntegral n)
                   event_array
                   eventPtr
       attachEventFinalizer =<< peek eventPtr
  where workDim = fromIntegral . maximum $ map length [globalWorkOffsets, globalWorkSizes, localWorkSizes]

data KernelArg where
  MObjArg :: MemObject a -> KernelArg
  LocalArrayArg :: Storable a => a -> Int -> KernelArg
  VArg :: Storable a => a -> KernelArg
  StructArg :: Storable a => [a] -> KernelArg

setKernelArg :: Kernel -> Int -> KernelArg -> IO ()
setKernelArg kernel n param =
  withForeignPtr kernel $ \k ->
  withPtr param $ \param_ptr -> do
    err <- {# call unsafe clSetKernelArg #} k (fromIntegral n) (size param) param_ptr
    case toEnum $ fromIntegral err of
      InvalidArgSize  -> error $ "ClInvalidArgSize occurred in call to: clSetKernelArg. Argument #"
                                 ++ show n ++ " was set to size " ++ show (size param)
      InvalidArgIndex -> error $ "ClInvalidArgIndex occurred in call to: clSetKernelArg, when setting argument #"
                                 ++ show n
      _ -> checkClError_ "clSetKernelArg" err
      where size (MObjArg mobj) = fromIntegral $ sizeOf (memobjPtr mobj)
            size (VArg v) = fromIntegral $ sizeOf v
            size (StructArg xs) = fromIntegral . sum $ map sizeOf xs
            size (LocalArrayArg x n) = fromIntegral $ n * sizeOf x        

            withPtr :: KernelArg -> (Ptr () -> IO c) -> IO c
            withPtr (MObjArg mobj) f = with (memobjPtr mobj) $ f . castPtr
            withPtr (VArg v) f = with v $ f . castPtr
            withPtr (LocalArrayArg _ _) f = f nullPtr
            withPtr a@(StructArg xs) f = do
              allocaBytes (fromIntegral $ size a) $ \ptr -> do
                pokeElems ptr xs
                f (castPtr ptr)

            pokeElems :: Storable a => Ptr a -> [a] -> IO ()
            pokeElems ptr (x:xs) = poke ptr x >> pokeElems (plusPtr ptr (sizeOf x)) xs
            pokeElems _ [] = return ()

setKernelArgs :: Kernel -> [KernelArg] -> IO ()
setKernelArgs kernel args = zipWithM_ (setKernelArg kernel) [0..] args


enqueueTask :: CommandQueue -> Kernel -> [Event] -> IO Event
enqueueTask cq k waitEvs =
    withForeignPtr cq $ \queue ->
    withForeignPtr k $ \kernel ->
    withForeignPtrs waitEvs $ \event_ptrs ->
    withArrayNullLen event_ptrs $ \n event_array ->
    alloca $ \eventPtr ->
    do checkClError_ "clEnqueueTask" =<<
         {# call unsafe clEnqueueTask #}
                   queue kernel
                   (fromIntegral n)
                   event_array
                   eventPtr
       attachEventFinalizer =<< peek eventPtr

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
getKernelInfo kernel info =
    withForeignPtr kernel $ \kernel_ptr ->
    getInfo (clGetKernelInfo_ kernel_ptr) info
  where
    clGetKernelInfo_ kernel name size value size_ret =
      checkClError "clGetKernelInfo" =<<
        {#call unsafe clGetKernelInfo #} kernel name size value size_ret

getKernelWorkGroupInfo kernel device info =
    withForeignPtr kernel $ \kernel_ptr ->
    getInfo (clGetKernelWorkGroupInfo_ kernel_ptr device) info
  where
    clGetKernelWorkGroupInfo_ kernel device name size value size_ret =
      checkClError "clGetKernelWorkGroupInfo" =<< 
        {#call unsafe clGetKernelWorkGroupInfo #} kernel device name size value size_ret

