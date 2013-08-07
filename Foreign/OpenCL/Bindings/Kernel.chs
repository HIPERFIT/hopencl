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
   kernelWorkGroupSize, kernelCompileWorkGroupSize, kernelLocalMemSize,
   kernelPreferredWorkGroupSizeMultiple, kernelPrivateMemSize,

   enqueueNDRangeKernel, enqueueTask,

   KernelArg(..), setKernelArg, setKernelArgs
  ) where

#include <CL/cl.h>

import Control.Monad

import Foreign
import Foreign.C.String
import Foreign.C.Types

{# import Foreign.OpenCL.Bindings.Internal.Types #}
import Foreign.OpenCL.Bindings.Internal.Finalizers
import Foreign.OpenCL.Bindings.Internal.Error
import Foreign.OpenCL.Bindings.Internal.Util
import Foreign.OpenCL.Bindings.Internal.Logging as Log

-- | Create a program from a string containing the source code
--
createKernel :: Program -- ^The program that contains the kernel code
              -> String -- ^The name of the kernel (as written in the program source)
              -> IO Kernel -- ^The newly created kernel
createKernel prog name =
   withForeignPtr prog $ \prog_ptr ->
   withCString name $ \cstr ->
   alloca $ \ep -> do
      Log.debug "Invoking clCreateKernel"
      kernel <- {# call unsafe clCreateKernel #} prog_ptr cstr ep
      checkClError_ "clCreateKernel" =<< peek ep
      attachFinalizer kernel

-- | Enqueues a command to execute a given kernel on a device. See section 5.8 in the OpenCL 1.1 specification
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
    do Log.debug "Invoking clEnqueueNDRangeKernel"
       checkClError_ "clEnqueueNDRangeKernel" =<< 
         {# call unsafe clEnqueueNDRangeKernel #} 
                   queue kernel workDim
                   globalWorkOffsetPtr
                   globalWorkSizePtr
                   localWorkSizePtr
                   (fromIntegral n)
                   event_array
                   eventPtr
       attachFinalizer =<< peek eventPtr
  where workDim = fromIntegral . maximum $ map length [globalWorkOffsets, globalWorkSizes, localWorkSizes]

data KernelArg where
  MObjArg :: MemObject a -> KernelArg
  LocalArrayArg :: Storable a => a -> Int -> KernelArg
  VArg :: Storable a => a -> KernelArg
  StructArg :: Storable a => [a] -> KernelArg

-- | Invoking @setKernelArg krn n arg@ sets argument @n@ of the kernel @krn@
setKernelArg :: Kernel -> Int -> KernelArg -> IO ()
setKernelArg kernel n param =
  withForeignPtr kernel $ \k ->
  withPtr param $ \param_ptr -> do
    Log.debug "Invoking clSetKernelArgs"
    err <- {# call unsafe clSetKernelArg #} k (fromIntegral n) (size param) param_ptr
    case toEnum $ fromIntegral err of
      InvalidArgSize  -> error $ "ClInvalidArgSize occurred in call to: clSetKernelArg. Argument #"
                                 ++ show n ++ " was set to size " ++ show (size param)
      InvalidArgIndex -> error $ "ClInvalidArgIndex occurred in call to: clSetKernelArg, when setting argument #"
                                 ++ show n
      _ -> checkClError_ "clSetKernelArg" err
      where size :: KernelArg -> ClSize
            size (MObjArg mobj) = fromIntegral $ sizeOf (memobjPtr mobj)
            size (VArg v) = fromIntegral $ sizeOf v
            size (StructArg xs) = fromIntegral . sum $ map sizeOf xs
            size (LocalArrayArg x m) = fromIntegral $ m * sizeOf x        

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

-- | Sets all arguments of a kernel to the parameters in the list
setKernelArgs :: Kernel -> [KernelArg] -> IO ()
setKernelArgs kernel args = zipWithM_ (setKernelArg kernel) [0..] args

-- | Enqueue a command to execute a kernel using a single work-item.
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
       attachFinalizer =<< peek eventPtr

-- | The 'Context' associated with a 'Kernel'
kernelContext :: Kernel -> IO Context
kernelContext kernel = 
  getKernelInfo kernel KernelContext >>= attachRetainFinalizer

-- | The function name (in the OpenCL C source code) of a 'Kernel'
kernelFunctionName :: Kernel -> IO String
kernelFunctionName kernel = getKernelInfo kernel KernelFunctionName

-- | The number of arguments that needs to be set before invoking a 'Kernel'
kernelNumArgs :: Kernel -> IO Int
kernelNumArgs kernel = fromIntegral `fmap` (getKernelInfo kernel KernelNumArgs :: IO ClUInt)

-- | The maximum work-group size that can be used to execute a kernel
-- on a specific device given by device. The OpenCL implementation
-- uses the resource requirements of the kernel (register usage etc.)
-- to determine what this work group size should be.
kernelWorkGroupSize :: Kernel -> DeviceID -> IO CSize
kernelWorkGroupSize kernel device =
  getKernelWorkGroupInfo kernel device KernelWorkGroupSize

-- | Returns the work-group size specified by the
-- @__attribute__((reqd_work_group_size(X, Y, Z)))@ qualifier.
-- Refer to section 6.8.2 of the OpenCL 1.1 specification
-- If undefined, this function returns (0,0,0)
kernelCompileWorkGroupSize :: Kernel -> DeviceID -> IO CSize
kernelCompileWorkGroupSize kernel device =
  getKernelWorkGroupInfo kernel device KernelCompileWorkGroupSize

-- | Returns the amount of local memory in bytes being used by a
-- kernel. This includes local memory that may be needed by an
-- implementation to execute the kernel, variables declared inside the
-- kernel with the __local address qualifier and local memory to be
-- allocated for arguments to the kernel declared as pointers with the
-- __local address qualifier and whose size is specified with
-- 'setKernelArg'.
--
-- If the local memory size, for any pointer argument to the kernel
-- declared with the __local address qualifier, is not specified, its
-- size is assumed to be 0.
kernelLocalMemSize :: Kernel -> DeviceID -> IO Word64
kernelLocalMemSize kernel device =
  getKernelWorkGroupInfo kernel device KernelLocalMemSize

-- | Returns the preferred multiple of work-group size for
-- launch. This is a performance hint. Specifying a work-group size
-- that is not a multiple of the value returned by this query as the
-- value of the local work size argument to 'enqueueNDRangeKernel'
-- will not fail to enqueue the kernel for execution unless the
-- work-group size specified is larger than the device maximum.
kernelPreferredWorkGroupSizeMultiple :: Kernel -> DeviceID -> IO CSize
kernelPreferredWorkGroupSizeMultiple kernel device =
  getKernelWorkGroupInfo kernel device KernelPreferredWorkGroupSizeMultiple

-- | Returns the minimum amount of private memory, in bytes, used by
-- each work-item in the kernel. This value may include any private
-- memory needed by an implementation to execute the kernel, including
-- that used by the language built-ins and variable declared inside
-- the kernel with the __private qualifier.
kernelPrivateMemSize :: Kernel -> DeviceID -> IO Word64
kernelPrivateMemSize kernel device =
  getKernelWorkGroupInfo kernel device KernelPrivateMemSize

-- C interfacing functions
getKernelInfo kernel info =
    withForeignPtr kernel $ \kernel_ptr ->
    getInfo (clGetKernelInfo_ kernel_ptr) info
  where
    clGetKernelInfo_ =
      checkClError5 "clGetKernelInfo"
                    {#call unsafe clGetKernelInfo #}

getKernelWorkGroupInfo kernel device info =
    withForeignPtr kernel $ \kernel_ptr ->
    getInfo (clGetKernelWorkGroupInfo_ kernel_ptr device) info
  where
    clGetKernelWorkGroupInfo_ =
      checkClError6 "clGetKernelWorkGroupInfo" 
                    {#call unsafe clGetKernelWorkGroupInfo #}

