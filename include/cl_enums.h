#include <CL/cl.h>

/* Error Codes */
enum ClError {
  ClSuccess = CL_SUCCESS,
  ClDeviceNotFound = CL_DEVICE_NOT_FOUND,
  ClDeviceNotAvailable = CL_DEVICE_NOT_AVAILABLE,
  ClCompilerNotAvailable = CL_COMPILER_NOT_AVAILABLE,
  ClMemObjectAllocationFailure = CL_MEM_OBJECT_ALLOCATION_FAILURE,
  ClOutOfResources = CL_OUT_OF_RESOURCES,
  ClOutOfHostMemory = CL_OUT_OF_HOST_MEMORY,
  ClProfilingInfoNotAvailable = CL_PROFILING_INFO_NOT_AVAILABLE,
  ClMemCopyOverlap = CL_MEM_COPY_OVERLAP,
  ClImageFormatMismatch = CL_IMAGE_FORMAT_MISMATCH,
  ClImageFormatNotSupported = CL_IMAGE_FORMAT_NOT_SUPPORTED,
  ClBuildProgramFailure = CL_BUILD_PROGRAM_FAILURE,
  ClMapFailure = CL_MAP_FAILURE,
  ClMisalignedSubBufferOffset = CL_MISALIGNED_SUB_BUFFER_OFFSET,
  ClExecStatusErrorForEventsInWaitList = CL_EXEC_STATUS_ERROR_FOR_EVENTS_IN_WAIT_LIST,

  ClInvalidValue = CL_INVALID_VALUE,
  ClInvalidDeviceType = CL_INVALID_DEVICE_TYPE,
  ClInvalidPlatform = CL_INVALID_PLATFORM,
  ClInvalidDevice = CL_INVALID_DEVICE,
  ClInvalidContext = CL_INVALID_CONTEXT,
  ClInvalidQueueProperties = CL_INVALID_QUEUE_PROPERTIES,
  ClInvalidCommandQueue = CL_INVALID_COMMAND_QUEUE,
  ClInvalidHostPtr = CL_INVALID_HOST_PTR,
  ClInvalidMemObject = CL_INVALID_MEM_OBJECT,
  ClInvalidImageFormatDescriptor = CL_INVALID_IMAGE_FORMAT_DESCRIPTOR,
  ClInvalidImageSize = CL_INVALID_IMAGE_SIZE,
  ClInvalidSampler = CL_INVALID_SAMPLER,
  ClInvalidBinary = CL_INVALID_BINARY,
  ClInvalidBuildOptions = CL_INVALID_BUILD_OPTIONS,
  ClInvalidProgram = CL_INVALID_PROGRAM,
  ClInvalidProgramExecutable = CL_INVALID_PROGRAM_EXECUTABLE,
  ClInvalidKernelName = CL_INVALID_KERNEL_NAME,
  ClInvalidKernelDefinition = CL_INVALID_KERNEL_DEFINITION,
  ClInvalidKernel = CL_INVALID_KERNEL,
  ClInvalidArgIndex = CL_INVALID_ARG_INDEX,
  ClInvalidArgValue = CL_INVALID_ARG_VALUE,
  ClInvalidArgSize = CL_INVALID_ARG_SIZE,
  ClInvalidKernelArgs = CL_INVALID_KERNEL_ARGS,
  ClInvalidWorkDimension = CL_INVALID_WORK_DIMENSION,
  ClInvalidWorkGroupSize = CL_INVALID_WORK_GROUP_SIZE,
  ClInvalidWorkItemSize = CL_INVALID_WORK_ITEM_SIZE,
  ClInvalidGlobalOffset = CL_INVALID_GLOBAL_OFFSET,
  ClInvalidEventWaitList = CL_INVALID_EVENT_WAIT_LIST,
  ClInvalidEvent = CL_INVALID_EVENT,
  ClInvalidOperation = CL_INVALID_OPERATION,
  ClInvalidGlObject = CL_INVALID_GL_OBJECT,
  ClInvalidBufferSize = CL_INVALID_BUFFER_SIZE,
  ClInvalidMipLevel = CL_INVALID_MIP_LEVEL,
  ClInvalidGlobalWorkSize = CL_INVALID_GLOBAL_WORK_SIZE,
  ClInvalidProperty = CL_INVALID_PROPERTY
};

/* OpenCL Version */
enum ClVersion {
  ClVersion1_0 = CL_VERSION_1_0,
  ClVersion1_1 = CL_VERSION_1_1
};

/* cl_bool */
enum ClBool {
  ClFalse = CL_FALSE,
  ClTrue = CL_TRUE
};

/* cl_platform_info */
enum PlatformInfo {
  PlatformProfile = CL_PLATFORM_PROFILE,
  PlatformVersion = CL_PLATFORM_VERSION,
  PlatformName = CL_PLATFORM_NAME,
  PlatformVendor = CL_PLATFORM_VENDOR,
  PlatformExtensions = CL_PLATFORM_EXTENSIONS
};

/* cl_device_type */
enum DeviceType {
  DeviceTypeDefault = CL_DEVICE_TYPE_DEFAULT,
  DeviceTypeCpu = CL_DEVICE_TYPE_CPU,
  DeviceTypeGpu = CL_DEVICE_TYPE_GPU,
  DeviceTypeAccelerator = CL_DEVICE_TYPE_ACCELERATOR,
  DeviceTypeAll = CL_DEVICE_TYPE_ALL
};

/* cl_device_info */
enum DeviceInfo {
  DeviceType = CL_DEVICE_TYPE,
  DeviceVendorID = CL_DEVICE_VENDOR_ID,
  DeviceMaxComputeUnits = CL_DEVICE_MAX_COMPUTE_UNITS,
  DeviceMaxWorkItemDimensions = CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS,
  DeviceMaxWorkGroupSize = CL_DEVICE_MAX_WORK_GROUP_SIZE,
  DeviceMaxWorkItemSizes = CL_DEVICE_MAX_WORK_ITEM_SIZES,
  DevicePreferredVectorWidthChar = CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR,
  DevicePreferredVectorWidthShort = CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT,
  DevicePreferredVectorWidthInt = CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT,
  DevicePreferredVectorWidthLong = CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG,
  DevicePreferredVectorWidthFloat = CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT,
  DevicePreferredVectorWidthDouble = CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE,
  DeviceMaxClockFrequency = CL_DEVICE_MAX_CLOCK_FREQUENCY,
  DeviceAddressBits = CL_DEVICE_ADDRESS_BITS,
  DeviceMaxReadImageArgs = CL_DEVICE_MAX_READ_IMAGE_ARGS,
  DeviceMaxWriteImageArgs = CL_DEVICE_MAX_WRITE_IMAGE_ARGS,
  DeviceMaxMemAllocSize = CL_DEVICE_MAX_MEM_ALLOC_SIZE,
  DeviceImage2DMaxWidth = CL_DEVICE_IMAGE2D_MAX_WIDTH,
  DeviceImage2DMaxHeight = CL_DEVICE_IMAGE2D_MAX_HEIGHT,
  DeviceImage3DMaxWidth = CL_DEVICE_IMAGE3D_MAX_WIDTH,
  DeviceImage3DMaxHeight = CL_DEVICE_IMAGE3D_MAX_HEIGHT,
  DeviceImage3DMaxDepth = CL_DEVICE_IMAGE3D_MAX_DEPTH,
  DeviceImageSupport = CL_DEVICE_IMAGE_SUPPORT,
  DeviceMaxParameterSize = CL_DEVICE_MAX_PARAMETER_SIZE,
  DeviceMaxSamplers = CL_DEVICE_MAX_SAMPLERS,
  DeviceMemBaseAddrAlign = CL_DEVICE_MEM_BASE_ADDR_ALIGN,
  DeviceMinDataTypeAlignSize = CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE,
  DeviceSingleFPConfig = CL_DEVICE_SINGLE_FP_CONFIG,
  DeviceGlobalMemCacheType = CL_DEVICE_GLOBAL_MEM_CACHE_TYPE,
  DeviceGlobalMemCachelineSize = CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE,
  DeviceGlobalMemCacheSize = CL_DEVICE_GLOBAL_MEM_CACHE_SIZE,
  DeviceGlobalMemSize = CL_DEVICE_GLOBAL_MEM_SIZE,
  DeviceMaxConstantBufferSize = CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE,
  DeviceMaxConstantArgs = CL_DEVICE_MAX_CONSTANT_ARGS,
  DeviceLocalMemType = CL_DEVICE_LOCAL_MEM_TYPE,
  DeviceLocalMemSize = CL_DEVICE_LOCAL_MEM_SIZE,
  DeviceErrorCorrectionSupport = CL_DEVICE_ERROR_CORRECTION_SUPPORT,
  DeviceProfilingTimerResolution = CL_DEVICE_PROFILING_TIMER_RESOLUTION,
  DeviceEndianLittle = CL_DEVICE_ENDIAN_LITTLE,
  DeviceAvailable = CL_DEVICE_AVAILABLE,
  DeviceCompilerAvailable = CL_DEVICE_COMPILER_AVAILABLE,
  DeviceExecutionCapabilities = CL_DEVICE_EXECUTION_CAPABILITIES,
  DeviceQueueProperties = CL_DEVICE_QUEUE_PROPERTIES,
  DeviceName = CL_DEVICE_NAME,
  DeviceVendor = CL_DEVICE_VENDOR,
  DriverVersion = CL_DRIVER_VERSION,
  DeviceProfile = CL_DEVICE_PROFILE,
  DeviceVersion = CL_DEVICE_VERSION,
  DeviceExtensions = CL_DEVICE_EXTENSIONS,
  DevicePlatform = CL_DEVICE_PLATFORM,
  DevicePreferredVectorWidthHalf = CL_DEVICE_PREFERRED_VECTOR_WIDTH_HALF,
  DeviceHostUnifiedMemory = CL_DEVICE_HOST_UNIFIED_MEMORY,
  DeviceNativeVectorWidthChar = CL_DEVICE_NATIVE_VECTOR_WIDTH_CHAR,
  DeviceNativeVectorWidthShort = CL_DEVICE_NATIVE_VECTOR_WIDTH_SHORT,
  DeviceNativeVectorWidthInt = CL_DEVICE_NATIVE_VECTOR_WIDTH_INT,
  DeviceNativeVectorWidthLong = CL_DEVICE_NATIVE_VECTOR_WIDTH_LONG,
  DeviceNativeVectorWidthFloat = CL_DEVICE_NATIVE_VECTOR_WIDTH_FLOAT,
  DeviceNativeVectorWidthDouble = CL_DEVICE_NATIVE_VECTOR_WIDTH_DOUBLE,
  DeviceNativeVectorWidthHalf = CL_DEVICE_NATIVE_VECTOR_WIDTH_HALF,
  DeviceOpenclCVersion = CL_DEVICE_OPENCL_C_VERSION
};

/* cl_device_fp_config */
enum DeviceFPConfig {
  FpDenorm = CL_FP_DENORM,
  FpInfNan = CL_FP_INF_NAN,
  FpRoundToNearest = CL_FP_ROUND_TO_NEAREST,
  FpRoundToZero = CL_FP_ROUND_TO_ZERO,
  FpRoundToInf = CL_FP_ROUND_TO_INF,
  FpFma = CL_FP_FMA,
  FpSoftFloat = CL_FP_SOFT_FLOAT
};

/* cl_device_mem_cache_type */
enum DeviceMemCacheType {
  None = CL_NONE,
  ReadOnlyCache = CL_READ_ONLY_CACHE,
  ReadWriteCache = CL_READ_WRITE_CACHE
};

/* cl_device_local_mem_type */
enum DeviceLocalMemType {
  Local = CL_LOCAL,
  Global = CL_GLOBAL
};

/* cl_device_exec_capabilities */
enum DeviceExecCapabilities {
  ExecKernel = CL_EXEC_KERNEL,
  ExecNativeKernel = CL_EXEC_NATIVE_KERNEL
};

/* cl_command_queue_properties */
enum CommandQueueProperties {
  QueueOutOfOrderExecModeEnable = CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE,
  QueueProfilingEnable = CL_QUEUE_PROFILING_ENABLE
};

/* cl_context_info */
enum ContextInfo {
  ContextReferenceCount = CL_CONTEXT_REFERENCE_COUNT,
  ContextDevices = CL_CONTEXT_DEVICES,
  ContextProperties = CL_CONTEXT_PROPERTIES,
  ContextNumDevices = CL_CONTEXT_NUM_DEVICES
};

/* cl_context_properties */
enum ClContextProperties {
  ClContextPlatform = CL_CONTEXT_PLATFORM
};

/* cl_command_queue_info */
enum CommandQueueInfo {
  QueueContext = CL_QUEUE_CONTEXT,
  QueueDevice = CL_QUEUE_DEVICE,
  QueueReferenceCount = CL_QUEUE_REFERENCE_COUNT,
  QueueProperties = CL_QUEUE_PROPERTIES
};

/* cl_mem_flags */
enum MemFlags {
  MemReadWrite = CL_MEM_READ_WRITE,
  MemWriteOnly = CL_MEM_WRITE_ONLY,
  MemReadOnly = CL_MEM_READ_ONLY,
  MemUseHostPtr = CL_MEM_USE_HOST_PTR,
  MemAllocHostPtr = CL_MEM_ALLOC_HOST_PTR,
  MemCopyHostPtr = CL_MEM_COPY_HOST_PTR
};

/* cl_channel_order */
enum ClChannelOrder {
  ClR = CL_R,
  ClA = CL_A,
  ClRg = CL_RG,
  ClRa = CL_RA,
  ClRgb = CL_RGB,
  ClRgba = CL_RGBA,
  ClBgra = CL_BGRA,
  ClArgb = CL_ARGB,
  ClIntensity = CL_INTENSITY,
  ClLuminance = CL_LUMINANCE,
  ClRx = CL_Rx,
  ClRgx = CL_RGx,
  ClRgbx = CL_RGBx
};

/* cl_channel_type */
enum ClChannelType {
  ClSnormInt8 = CL_SNORM_INT8,
  ClSnormInt16 = CL_SNORM_INT16,
  ClUnormInt8 = CL_UNORM_INT8,
  ClUnormInt16 = CL_UNORM_INT16,
  ClUnormShort565 = CL_UNORM_SHORT_565,
  ClUnormShort555 = CL_UNORM_SHORT_555,
  ClUnormInt101010 = CL_UNORM_INT_101010,
  ClSignedInt8 = CL_SIGNED_INT8,
  ClSignedInt16 = CL_SIGNED_INT16,
  ClSignedInt32 = CL_SIGNED_INT32,
  ClUnsignedInt8 = CL_UNSIGNED_INT8,
  ClUnsignedInt16 = CL_UNSIGNED_INT16,
  ClUnsignedInt32 = CL_UNSIGNED_INT32,
  ClHalfFloat = CL_HALF_FLOAT,
  ClFloat = CL_FLOAT
};

/* cl_mem_object_type */
enum MemObjectType {
  MemObjectBuffer = CL_MEM_OBJECT_BUFFER,
  MemObjectImage2D = CL_MEM_OBJECT_IMAGE2D,
  MemObjectImage3D = CL_MEM_OBJECT_IMAGE3D,
};

/* cl_mem_info */
enum MemInfo {
  MemType = CL_MEM_TYPE,
  MemFlags = CL_MEM_FLAGS,
  MemSize = CL_MEM_SIZE,
  MemHostPtr = CL_MEM_HOST_PTR,
  MemMapCount = CL_MEM_MAP_COUNT,
  MemReferenceCount = CL_MEM_REFERENCE_COUNT,
  MemContext = CL_MEM_CONTEXT,
  MemAssociatedMemobject = CL_MEM_ASSOCIATED_MEMOBJECT,
  MemOffset = CL_MEM_OFFSET
};

/* cl_image_info */
enum ClImageInfo {
  ClImageFormat = CL_IMAGE_FORMAT,
  ClImageElementSize = CL_IMAGE_ELEMENT_SIZE,
  ClImageRowPitch = CL_IMAGE_ROW_PITCH,
  ClImageSlicePitch = CL_IMAGE_SLICE_PITCH,
  ClImageWidth = CL_IMAGE_WIDTH,
  ClImageHeight = CL_IMAGE_HEIGHT,
  ClImageDepth = CL_IMAGE_DEPTH
};

/* cl_addressing_mode */
enum ClAddressingMode {
  ClAddressNone = CL_ADDRESS_NONE,
  ClAddressClampToEdge = CL_ADDRESS_CLAMP_TO_EDGE,
  ClAddressClamp = CL_ADDRESS_CLAMP,
  ClAddressRepeat = CL_ADDRESS_REPEAT,
  ClAddressMirroredRepeat = CL_ADDRESS_MIRRORED_REPEAT
};

/* cl_filter_mode */
enum ClFilterMode {
  ClFilterNearest = CL_FILTER_NEAREST,
  ClFilterLinear = CL_FILTER_LINEAR
};

/* cl_sampler_info */
enum ClSamplerInfo {
  ClSamplerReferenceCount = CL_SAMPLER_REFERENCE_COUNT,
  ClSamplerContext = CL_SAMPLER_CONTEXT,
  ClSamplerNormalizedCoords = CL_SAMPLER_NORMALIZED_COORDS,
  ClSamplerAddressingMode = CL_SAMPLER_ADDRESSING_MODE,
  ClSamplerFilterMode = CL_SAMPLER_FILTER_MODE
};

/* cl_map_flags */
enum ClMapFlags {
  ClMapRead = CL_MAP_READ,
  ClMapWrite = CL_MAP_WRITE
};

/* cl_program_info */
enum ProgramInfo {
  ProgramReferenceCount = CL_PROGRAM_REFERENCE_COUNT,
  ProgramContext = CL_PROGRAM_CONTEXT,
  ProgramNumDevices = CL_PROGRAM_NUM_DEVICES,
  ProgramDevices = CL_PROGRAM_DEVICES,
  ProgramSource = CL_PROGRAM_SOURCE,
  ProgramBinarySizes = CL_PROGRAM_BINARY_SIZES,
  ProgramBinaries = CL_PROGRAM_BINARIES
};

/* cl_program_build_info */
enum ProgramBuildInfo {
  ProgramBuildStatus = CL_PROGRAM_BUILD_STATUS,
  ProgramBuildOptions = CL_PROGRAM_BUILD_OPTIONS,
  ProgramBuildLog = CL_PROGRAM_BUILD_LOG
};

/* cl_build_status */
enum ClBuildStatus {
  ClBuildSuccess = CL_BUILD_SUCCESS,
  ClBuildNone = CL_BUILD_NONE,
  ClBuildError = CL_BUILD_ERROR,
  ClBuildInProgress = CL_BUILD_IN_PROGRESS
};

/* cl_kernel_info */
enum KernelInfo {
  KernelFunctionName = CL_KERNEL_FUNCTION_NAME,
  KernelNumArgs = CL_KERNEL_NUM_ARGS,
  KernelReferenceCount = CL_KERNEL_REFERENCE_COUNT,
  KernelContext = CL_KERNEL_CONTEXT,
  KernelProgram = CL_KERNEL_PROGRAM
};

/* cl_kernel_work_group_info */
enum KernelWorkGroupInfo {
  KernelWorkGroupSize = CL_KERNEL_WORK_GROUP_SIZE,
  KernelCompileWorkGroupSize = CL_KERNEL_COMPILE_WORK_GROUP_SIZE,
  KernelLocalMemSize = CL_KERNEL_LOCAL_MEM_SIZE,
  KernelPreferredWorkGroupSizeMultiple = CL_KERNEL_PREFERRED_WORK_GROUP_SIZE_MULTIPLE,
  KernelPrivateMemSize = CL_KERNEL_PRIVATE_MEM_SIZE
};

/* cl_event_info */
enum EventInfo {
  EventCommandQueue = CL_EVENT_COMMAND_QUEUE,
  EventCommandType = CL_EVENT_COMMAND_TYPE,
  EventReferenceCount = CL_EVENT_REFERENCE_COUNT,
  EventCommandExecutionStatus = CL_EVENT_COMMAND_EXECUTION_STATUS,
  EventContext = CL_EVENT_CONTEXT
};

/* cl_command_type */
enum CommandType {
  CommandNdrangeKernel = CL_COMMAND_NDRANGE_KERNEL,
  CommandTask = CL_COMMAND_TASK,
  CommandNativeKernel = CL_COMMAND_NATIVE_KERNEL,
  CommandReadBuffer = CL_COMMAND_READ_BUFFER,
  CommandWriteBuffer = CL_COMMAND_WRITE_BUFFER,
  CommandCopyBuffer = CL_COMMAND_COPY_BUFFER,
  CommandReadImage = CL_COMMAND_READ_IMAGE,
  CommandWriteImage = CL_COMMAND_WRITE_IMAGE,
  CommandCopyImage = CL_COMMAND_COPY_IMAGE,
  CommandCopyImageToBuffer = CL_COMMAND_COPY_IMAGE_TO_BUFFER,
  CommandCopyBufferToImage = CL_COMMAND_COPY_BUFFER_TO_IMAGE,
  CommandMapBuffer = CL_COMMAND_MAP_BUFFER,
  CommandMapImage = CL_COMMAND_MAP_IMAGE,
  CommandUnmapMemObject = CL_COMMAND_UNMAP_MEM_OBJECT,
  CommandMarker = CL_COMMAND_MARKER,
  CommandAcquireGlObjects = CL_COMMAND_ACQUIRE_GL_OBJECTS,
  CommandReleaseGlObjects = CL_COMMAND_RELEASE_GL_OBJECTS,
  CommandReadBufferRect = CL_COMMAND_READ_BUFFER_RECT,
  CommandWriteBufferRect = CL_COMMAND_WRITE_BUFFER_RECT,
  CommandCopyBufferRect = CL_COMMAND_COPY_BUFFER_RECT,
  CommandUser = CL_COMMAND_USER
};

/* command execution status */
enum ClCommandExecStatus {
  ClComplete = CL_COMPLETE,
  ClRunning = CL_RUNNING,
  ClSubmitted = CL_SUBMITTED,
  ClQueued = CL_QUEUED
};

/* cl_buffer_create_type */
enum ClBufferCreateType {
  ClBufferCreateTypeRegion = CL_BUFFER_CREATE_TYPE_REGION
};

/* cl_profiling_info */
enum ClProfilingInfo {
  ClProfilingCommandQueued = CL_PROFILING_COMMAND_QUEUED,
  ClProfilingCommandSubmit = CL_PROFILING_COMMAND_SUBMIT,
  ClProfilingCommandStart = CL_PROFILING_COMMAND_START,
  ClProfilingCommandEnd = CL_PROFILING_COMMAND_END
};
