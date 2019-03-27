{-# language DataKinds, TypeApplications #-}
module Graphics.Vulkan.PhysicalDevice
  ( vkEnumeratePhysicalDevices
  , vkGetPhysicalDeviceProperties
  , vkGetPhysicalDeviceFeatures
  , vkGetPhysicalDeviceQueueFamilyProperties
  , Vk.VkPhysicalDevice
  , VkPhysicalDeviceType(..)
  , vkPhysicalDeviceType, unVkPhysicalDeviceType
  , VkPhysicalDeviceProperties(..)
  , VkPhysicalDeviceLimits(..)
  , VkPhysicalDeviceSparseProperties(..)
  , Vk.VkDeviceSize(..)
  , VkPhysicalDeviceFeatures(..)
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Int (Int32)
import Data.Word (Word8, Word32)

import qualified Graphics.Vulkan.Constants as Vk
import qualified Graphics.Vulkan.Core_1_0 as Vk
import qualified Foreign.C.Types as Foreign
import qualified Foreign.Marshal.Alloc as Foreign
import qualified Foreign.Marshal.Array as Foreign
import qualified Foreign.Ptr as Foreign
import qualified Foreign.Storable as Foreign

import Graphics.Vulkan.Queue
  (VkQueueFamilyProperties, vkQueueFamilyProperties)
import Graphics.Vulkan.Result (vkResult)
import Graphics.Vulkan.SampleCount (VkSampleCount, vkSampleCountBits)

vkEnumeratePhysicalDevices :: MonadIO m => Vk.VkInstance -> m [Vk.VkPhysicalDevice]
vkEnumeratePhysicalDevices inst =
  liftIO $
  Foreign.alloca $ \countPtr -> do
    vkResult =<< Vk.vkEnumeratePhysicalDevices inst countPtr Foreign.nullPtr
    count <- fromIntegral <$> Foreign.peek countPtr
    Foreign.allocaArray count $ \arrayPtr -> do
      vkResult =<< Vk.vkEnumeratePhysicalDevices inst countPtr arrayPtr
      Foreign.peekArray count arrayPtr

data VkPhysicalDeviceType
  = Other
  | IntegratedGpu
  | DiscreteGpu
  | VirtualGpu
  | Cpu
  deriving (Eq, Ord, Show)

vkPhysicalDeviceType :: Vk.VkPhysicalDeviceType -> VkPhysicalDeviceType
vkPhysicalDeviceType a =
  case a of
    Vk.VK_PHYSICAL_DEVICE_TYPE_OTHER -> Other
    Vk.VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU -> IntegratedGpu
    Vk.VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU -> DiscreteGpu
    Vk.VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU -> VirtualGpu

unVkPhysicalDeviceType :: VkPhysicalDeviceType -> Vk.VkPhysicalDeviceType
unVkPhysicalDeviceType a =
  case a of
    Other -> Vk.VK_PHYSICAL_DEVICE_TYPE_OTHER
    IntegratedGpu -> Vk.VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU
    DiscreteGpu -> Vk.VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU
    VirtualGpu -> Vk.VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU

data VkPhysicalDeviceProperties
  = VkPhysicalDeviceProperties
  { apiVersion :: Word32
  , driverVersion :: Word32
  , vendorID :: Word32
  , deviceID :: Word32
  , deviceType :: VkPhysicalDeviceType
  , deviceName :: String
  , pipelineCacheUUID :: [Word8]
  , limits :: VkPhysicalDeviceLimits
  , sparseProperties :: VkPhysicalDeviceSparseProperties
  } deriving (Eq, Ord, Show)

data VkPhysicalDeviceSparseProperties
  = VkPhysicalDeviceSparseProperties
  { residencyStandard2DBlockShape :: Bool
  , residencyStandard2DMultisampleBlockShape :: Bool
  , residencyStandard3DBlockShape :: Bool
  , residencyAlignedMipSize :: Bool
  , residencyNonResidentStrict :: Bool
  } deriving (Eq, Ord, Show)

data VkPhysicalDeviceLimits
  = VkPhysicalDeviceLimits
  { maxImageDimension1D :: Word32
  , maxImageDimension2D :: Word32
  , maxImageDimension3D :: Word32
  , maxImageDimensionCube :: Word32
  , maxImageArrayLayers :: Word32
  , maxTexelBufferElements :: Word32
  , maxUniformBufferRange :: Word32
  , maxStorageBufferRange :: Word32
  , maxPushConstantsSize :: Word32
  , maxMemoryAllocationCount :: Word32
  , maxSamplerAllocationCount :: Word32
  , bufferImageGranularity :: Vk.VkDeviceSize
  , sparseAddressSpaceSize :: Vk.VkDeviceSize
  , maxBoundDescriptorSets :: Word32
  , maxPerStageDescriptorSamplers :: Word32
  , maxPerStageDescriptorUniformBuffers :: Word32
  , maxPerStageDescriptorStorageBuffers :: Word32
  , maxPerStageDescriptorSampledImages :: Word32
  , maxPerStageDescriptorStorageImages :: Word32
  , maxPerStageDescriptorInputAttachments :: Word32
  , maxPerStageResources :: Word32
  , maxDescriptorSetSamplers :: Word32
  , maxDescriptorSetUniformBuffers :: Word32
  , maxDescriptorSetUniformBuffersDynamic :: Word32
  , maxDescriptorSetStorageBuffers :: Word32
  , maxDescriptorSetStorageBuffersDynamic :: Word32
  , maxDescriptorSetSampledImages :: Word32
  , maxDescriptorSetStorageImages :: Word32
  , maxDescriptorSetInputAttachments :: Word32
  , maxVertexInputAttributes :: Word32
  , maxVertexInputBindings :: Word32
  , maxVertexInputAttributeOffset :: Word32
  , maxVertexInputBindingStride :: Word32
  , maxVertexOutputComponents :: Word32
  , maxTessellationGenerationLevel :: Word32
  , maxTessellationPatchSize :: Word32
  , maxTessellationControlPerVertexInputComponents :: Word32
  , maxTessellationControlPerVertexOutputComponents :: Word32
  , maxTessellationControlPerPatchOutputComponents :: Word32
  , maxTessellationControlTotalOutputComponents :: Word32
  , maxTessellationEvaluationInputComponents :: Word32
  , maxTessellationEvaluationOutputComponents :: Word32
  , maxGeometryShaderInvocations :: Word32
  , maxGeometryInputComponents :: Word32
  , maxGeometryOutputComponents :: Word32
  , maxGeometryOutputVertices :: Word32
  , maxGeometryTotalOutputComponents :: Word32
  , maxFragmentInputComponents :: Word32
  , maxFragmentOutputAttachments :: Word32
  , maxFragmentDualSrcAttachments :: Word32
  , maxFragmentCombinedOutputResources :: Word32
  , maxComputeSharedMemorySize :: Word32
  , maxComputeWorkGroupCount :: (Word32, Word32, Word32)
  , maxComputeWorkGroupInvocations :: Word32
  , maxComputeWorkGroupSize :: (Word32, Word32, Word32)
  , subPixelPrecisionBits :: Word32
  , subTexelPrecisionBits :: Word32
  , mipmapPrecisionBits :: Word32
  , maxDrawIndexedIndexValue :: Word32
  , maxDrawIndirectCount :: Word32
  , maxSamplerLodBias :: Float
  , maxSamplerAnisotropy :: Float
  , maxViewports :: Word32
  , maxViewportDimensions :: (Word32, Word32)
  , viewportBoundsRange :: (Float, Float)
  , viewportSubPixelBits :: Word32
  , minMemoryMapAlignment :: Foreign.CSize
  , minTexelBufferOffsetAlignment :: Vk.VkDeviceSize
  , minUniformBufferOffsetAlignment :: Vk.VkDeviceSize
  , minStorageBufferOffsetAlignment :: Vk.VkDeviceSize
  , minTexelOffset :: Int32
  , maxTexelOffset :: Word32
  , minTexelGatherOffset :: Int32
  , maxTexelGatherOffset :: Word32
  , minInterpolationOffset :: Float
  , maxInterpolationOffset :: Float
  , subPixelInterpolationOffsetBits :: Word32
  , maxFramebufferWidth :: Word32
  , maxFramebufferHeight :: Word32
  , maxFramebufferLayers :: Word32
  , framebufferColorSampleCounts :: [VkSampleCount]
  , framebufferDepthSampleCounts :: [VkSampleCount]
  , framebufferStencilSampleCounts :: [VkSampleCount]
  , framebufferNoAttachmentsSampleCounts :: [VkSampleCount]
  , maxColorAttachments :: Word32
  , sampledImageColorSampleCounts :: [VkSampleCount]
  , sampledImageIntegerSampleCounts :: [VkSampleCount]
  , sampledImageDepthSampleCounts :: [VkSampleCount]
  , sampledImageStencilSampleCounts :: [VkSampleCount]
  , storageImageSampleCounts :: [VkSampleCount]
  , maxSampleMaskWords :: Word32
  , timestampComputeAndGraphics :: Bool
  , timestampPeriod :: Float
  , maxClipDistances :: Word32
  , maxCullDistances :: Word32
  , maxCombinedClipAndCullDistances :: Word32
  , discreteQueuePriorities :: Word32
  , pointSizeRange :: (Float, Float)
  , lineWidthRange :: (Float, Float)
  , pointSizeGranularity :: Float
  , lineWidthGranularity :: Float
  , strictLines :: Bool
  , standardSampleLocations :: Bool
  , optimalBufferCopyOffsetAlignment :: Vk.VkDeviceSize
  , optimalBufferCopyRowPitchAlignment :: Vk.VkDeviceSize
  , nonCoherentAtomSize :: Vk.VkDeviceSize
  } deriving (Eq, Show, Ord)

vkPhysicalDeviceProperties ::
  MonadIO m =>
  Vk.VkPhysicalDeviceProperties ->
  m VkPhysicalDeviceProperties
vkPhysicalDeviceProperties p =
  liftIO $
  (\uuid ->
     VkPhysicalDeviceProperties
     { apiVersion = Vk.getField @"apiVersion" p
     , driverVersion = Vk.getField @"driverVersion" p
     , vendorID = Vk.getField @"vendorID" p
     , deviceID = Vk.getField @"deviceID" p
     , deviceType = vkPhysicalDeviceType $ Vk.getField @"deviceType" p
     , deviceName = Vk.getStringField @"deviceName" p
     , pipelineCacheUUID = uuid
     , limits = vkPhysicalDeviceLimits $ Vk.getField @"limits" p
     , sparseProperties = vkPhysicalDeviceSparseProperties $ Vk.getField @"sparseProperties" p
     }) <$>
  Foreign.peekArray Vk.VK_UUID_SIZE
    (Foreign.plusPtr
       (Vk.unsafePtr p)
       (Vk.fieldOffset @"pipelineCacheUUID" @Vk.VkPhysicalDeviceProperties))

vkPhysicalDeviceSparseProperties ::
  Vk.VkPhysicalDeviceSparseProperties ->
  VkPhysicalDeviceSparseProperties
vkPhysicalDeviceSparseProperties p =
  VkPhysicalDeviceSparseProperties
  { residencyStandard2DBlockShape =
      Vk.getField @"residencyStandard2DBlockShape" p == Vk.VK_TRUE
  , residencyStandard2DMultisampleBlockShape =
      Vk.getField @"residencyStandard2DMultisampleBlockShape" p == Vk.VK_TRUE
  , residencyStandard3DBlockShape =
      Vk.getField @"residencyStandard3DBlockShape" p == Vk.VK_TRUE
  , residencyAlignedMipSize =
      Vk.getField @"residencyAlignedMipSize" p == Vk.VK_TRUE
  , residencyNonResidentStrict =
      Vk.getField @"residencyNonResidentStrict" p == Vk.VK_TRUE
  }

vkPhysicalDeviceLimits ::
  Vk.VkPhysicalDeviceLimits ->
  VkPhysicalDeviceLimits
vkPhysicalDeviceLimits p =
  VkPhysicalDeviceLimits
  { maxImageDimension1D = Vk.getField @"maxImageDimension1D" p
  , maxImageDimension2D = Vk.getField @"maxImageDimension2D" p
  , maxImageDimension3D = Vk.getField @"maxImageDimension3D" p
  , maxImageDimensionCube = Vk.getField @"maxImageDimensionCube" p
  , maxImageArrayLayers = Vk.getField @"maxImageArrayLayers" p
  , maxTexelBufferElements = Vk.getField @"maxTexelBufferElements" p
  , maxUniformBufferRange = Vk.getField @"maxUniformBufferRange" p
  , maxStorageBufferRange = Vk.getField @"maxStorageBufferRange" p
  , maxPushConstantsSize = Vk.getField @"maxPushConstantsSize" p
  , maxMemoryAllocationCount = Vk.getField @"maxMemoryAllocationCount" p
  , maxSamplerAllocationCount = Vk.getField @"maxSamplerAllocationCount" p
  , bufferImageGranularity = Vk.getField @"bufferImageGranularity" p
  , sparseAddressSpaceSize = Vk.getField @"sparseAddressSpaceSize" p
  , maxBoundDescriptorSets = Vk.getField @"maxBoundDescriptorSets" p
  , maxPerStageDescriptorSamplers = Vk.getField @"maxPerStageDescriptorSamplers" p
  , maxPerStageDescriptorUniformBuffers = Vk.getField @"maxPerStageDescriptorUniformBuffers" p
  , maxPerStageDescriptorStorageBuffers = Vk.getField @"maxPerStageDescriptorStorageBuffers" p
  , maxPerStageDescriptorSampledImages = Vk.getField @"maxPerStageDescriptorSampledImages" p
  , maxPerStageDescriptorStorageImages = Vk.getField @"maxPerStageDescriptorStorageImages" p
  , maxPerStageDescriptorInputAttachments = Vk.getField @"maxPerStageDescriptorInputAttachments" p
  , maxPerStageResources = Vk.getField @"maxPerStageResources" p
  , maxDescriptorSetSamplers = Vk.getField @"maxDescriptorSetSamplers" p
  , maxDescriptorSetUniformBuffers = Vk.getField @"maxDescriptorSetUniformBuffers" p
  , maxDescriptorSetUniformBuffersDynamic = Vk.getField @"maxDescriptorSetUniformBuffersDynamic" p
  , maxDescriptorSetStorageBuffers = Vk.getField @"maxDescriptorSetStorageBuffers" p
  , maxDescriptorSetStorageBuffersDynamic = Vk.getField @"maxDescriptorSetStorageBuffersDynamic" p
  , maxDescriptorSetSampledImages = Vk.getField @"maxDescriptorSetSampledImages" p
  , maxDescriptorSetStorageImages = Vk.getField @"maxDescriptorSetStorageImages" p
  , maxDescriptorSetInputAttachments = Vk.getField @"maxDescriptorSetInputAttachments" p
  , maxVertexInputAttributes = Vk.getField @"maxVertexInputAttributes" p
  , maxVertexInputBindings = Vk.getField @"maxVertexInputBindings" p
  , maxVertexInputAttributeOffset = Vk.getField @"maxVertexInputAttributeOffset" p
  , maxVertexInputBindingStride = Vk.getField @"maxVertexInputBindingStride" p
  , maxVertexOutputComponents = Vk.getField @"maxVertexOutputComponents" p
  , maxTessellationGenerationLevel = Vk.getField @"maxTessellationGenerationLevel" p
  , maxTessellationPatchSize = Vk.getField @"maxTessellationPatchSize" p
  , maxTessellationControlPerVertexInputComponents = Vk.getField @"maxTessellationControlPerVertexInputComponents" p
  , maxTessellationControlPerVertexOutputComponents = Vk.getField @"maxTessellationControlPerVertexOutputComponents" p
  , maxTessellationControlPerPatchOutputComponents = Vk.getField @"maxTessellationControlPerPatchOutputComponents" p
  , maxTessellationControlTotalOutputComponents = Vk.getField @"maxTessellationControlTotalOutputComponents" p
  , maxTessellationEvaluationInputComponents = Vk.getField @"maxTessellationEvaluationInputComponents" p
  , maxTessellationEvaluationOutputComponents = Vk.getField @"maxTessellationEvaluationOutputComponents" p
  , maxGeometryShaderInvocations = Vk.getField @"maxGeometryShaderInvocations" p
  , maxGeometryInputComponents = Vk.getField @"maxGeometryInputComponents" p
  , maxGeometryOutputComponents = Vk.getField @"maxGeometryOutputComponents" p
  , maxGeometryOutputVertices = Vk.getField @"maxGeometryOutputVertices" p
  , maxGeometryTotalOutputComponents = Vk.getField @"maxGeometryTotalOutputComponents" p
  , maxFragmentInputComponents = Vk.getField @"maxFragmentInputComponents" p
  , maxFragmentOutputAttachments = Vk.getField @"maxFragmentOutputAttachments" p
  , maxFragmentDualSrcAttachments = Vk.getField @"maxFragmentDualSrcAttachments" p
  , maxFragmentCombinedOutputResources = Vk.getField @"maxFragmentCombinedOutputResources" p
  , maxComputeSharedMemorySize = Vk.getField @"maxComputeSharedMemorySize" p
  , maxComputeWorkGroupCount =
    ( Vk.getFieldArray @"maxComputeWorkGroupCount" @0 p
    , Vk.getFieldArray @"maxComputeWorkGroupCount" @1 p
    , Vk.getFieldArray @"maxComputeWorkGroupCount" @2 p
    )
  , maxComputeWorkGroupInvocations = Vk.getField @"maxComputeWorkGroupInvocations" p
  , maxComputeWorkGroupSize =
    ( Vk.getFieldArray @"maxComputeWorkGroupSize" @0 p
    , Vk.getFieldArray @"maxComputeWorkGroupSize" @1 p
    , Vk.getFieldArray @"maxComputeWorkGroupSize" @2 p
    )
  , subPixelPrecisionBits = Vk.getField @"subPixelPrecisionBits" p
  , subTexelPrecisionBits = Vk.getField @"subTexelPrecisionBits" p
  , mipmapPrecisionBits = Vk.getField @"mipmapPrecisionBits" p
  , maxDrawIndexedIndexValue = Vk.getField @"maxDrawIndexedIndexValue" p
  , maxDrawIndirectCount = Vk.getField @"maxDrawIndirectCount" p
  , maxSamplerLodBias = Vk.getField @"maxSamplerLodBias" p
  , maxSamplerAnisotropy = Vk.getField @"maxSamplerAnisotropy" p
  , maxViewports = Vk.getField @"maxViewports" p
  , maxViewportDimensions =
    ( Vk.getFieldArray @"maxViewportDimensions" @0 p
    , Vk.getFieldArray @"maxViewportDimensions" @1 p
    )
  , viewportBoundsRange =
    ( Vk.getFieldArray @"viewportBoundsRange" @0 p
    , Vk.getFieldArray @"viewportBoundsRange" @1 p
    )
  , viewportSubPixelBits = Vk.getField @"viewportSubPixelBits" p
  , minMemoryMapAlignment = Vk.getField @"minMemoryMapAlignment" p
  , minTexelBufferOffsetAlignment = Vk.getField @"minTexelBufferOffsetAlignment" p
  , minUniformBufferOffsetAlignment = Vk.getField @"minUniformBufferOffsetAlignment" p
  , minStorageBufferOffsetAlignment = Vk.getField @"minStorageBufferOffsetAlignment" p
  , minTexelOffset = Vk.getField @"minTexelOffset" p
  , maxTexelOffset = Vk.getField @"maxTexelOffset" p
  , minTexelGatherOffset = Vk.getField @"minTexelGatherOffset" p
  , maxTexelGatherOffset = Vk.getField @"maxTexelGatherOffset" p
  , minInterpolationOffset = Vk.getField @"minInterpolationOffset" p
  , maxInterpolationOffset = Vk.getField @"maxInterpolationOffset" p
  , subPixelInterpolationOffsetBits = Vk.getField @"subPixelInterpolationOffsetBits" p
  , maxFramebufferWidth = Vk.getField @"maxFramebufferWidth" p
  , maxFramebufferHeight = Vk.getField @"maxFramebufferHeight" p
  , maxFramebufferLayers = Vk.getField @"maxFramebufferLayers" p
  , framebufferColorSampleCounts =
      vkSampleCountBits $ Vk.getField @"framebufferColorSampleCounts" p
  , framebufferDepthSampleCounts =
      vkSampleCountBits $ Vk.getField @"framebufferDepthSampleCounts" p
  , framebufferStencilSampleCounts =
      vkSampleCountBits $ Vk.getField @"framebufferStencilSampleCounts" p
  , framebufferNoAttachmentsSampleCounts =
      vkSampleCountBits $ Vk.getField @"framebufferNoAttachmentsSampleCounts" p
  , maxColorAttachments = Vk.getField @"maxColorAttachments" p
  , sampledImageColorSampleCounts =
      vkSampleCountBits $ Vk.getField @"sampledImageColorSampleCounts" p
  , sampledImageIntegerSampleCounts =
      vkSampleCountBits $ Vk.getField @"sampledImageIntegerSampleCounts" p
  , sampledImageDepthSampleCounts =
      vkSampleCountBits $ Vk.getField @"sampledImageDepthSampleCounts" p
  , sampledImageStencilSampleCounts =
      vkSampleCountBits $ Vk.getField @"sampledImageStencilSampleCounts" p
  , storageImageSampleCounts = vkSampleCountBits $ Vk.getField @"storageImageSampleCounts" p
  , maxSampleMaskWords = Vk.getField @"maxSampleMaskWords" p
  , timestampComputeAndGraphics = Vk.getField @"timestampComputeAndGraphics" p == Vk.VK_TRUE
  , timestampPeriod = Vk.getField @"timestampPeriod" p
  , maxClipDistances = Vk.getField @"maxClipDistances" p
  , maxCullDistances = Vk.getField @"maxCullDistances" p
  , maxCombinedClipAndCullDistances = Vk.getField @"maxCombinedClipAndCullDistances" p
  , discreteQueuePriorities = Vk.getField @"discreteQueuePriorities" p
  , pointSizeRange =
    ( Vk.getFieldArray @"pointSizeRange" @0 p
    , Vk.getFieldArray @"pointSizeRange" @1 p
    )
  , lineWidthRange =
    ( Vk.getFieldArray @"lineWidthRange" @0 p
    , Vk.getFieldArray @"lineWidthRange" @1 p
    )
  , pointSizeGranularity = Vk.getField @"pointSizeGranularity" p
  , lineWidthGranularity = Vk.getField @"lineWidthGranularity" p
  , strictLines = Vk.getField @"strictLines" p == Vk.VK_TRUE
  , standardSampleLocations = Vk.getField @"standardSampleLocations" p == Vk.VK_TRUE
  , optimalBufferCopyOffsetAlignment = Vk.getField @"optimalBufferCopyOffsetAlignment" p
  , optimalBufferCopyRowPitchAlignment = Vk.getField @"optimalBufferCopyRowPitchAlignment" p
  , nonCoherentAtomSize = Vk.getField @"nonCoherentAtomSize" p
  }

vkGetPhysicalDeviceProperties ::
  MonadIO m =>
  Vk.VkPhysicalDevice ->
  m VkPhysicalDeviceProperties
vkGetPhysicalDeviceProperties d =
  liftIO $
  Foreign.alloca $ \propsPtr -> do
    Vk.vkGetPhysicalDeviceProperties d propsPtr
    vkPhysicalDeviceProperties =<< Foreign.peek propsPtr

data VkPhysicalDeviceFeatures
  = VkPhysicalDeviceFeatures
  { robustBufferAccess :: Bool
  , fullDrawIndexUint32 :: Bool
  , imageCubeArray :: Bool
  , independentBlend :: Bool
  , geometryShader :: Bool
  , tessellationShader :: Bool
  , sampleRateShading :: Bool
  , dualSrcBlend :: Bool
  , logicOp :: Bool
  , multiDrawIndirect :: Bool
  , drawIndirectFirstInstance :: Bool
  , depthClamp :: Bool
  , depthBiasClamp :: Bool
  , fillModeNonSolid :: Bool
  , depthBounds :: Bool
  , wideLines :: Bool
  , largePoints :: Bool
  , alphaToOne :: Bool
  , multiViewport :: Bool
  , samplerAnisotropy :: Bool
  , textureCompressionETC2 :: Bool
  , textureCompressionASTC_LDR :: Bool
  , textureCompressionBC :: Bool
  , occlusionQueryPrecise :: Bool
  , pipelineStatisticsQuery :: Bool
  , vertexPipelineStoresAndAtomics :: Bool
  , fragmentStoresAndAtomics :: Bool
  , shaderTessellationAndGeometryPointSize :: Bool
  , shaderImageGatherExtended :: Bool
  , shaderStorageImageExtendedFormats :: Bool
  , shaderStorageImageMultisample :: Bool
  , shaderStorageImageReadWithoutFormat :: Bool
  , shaderStorageImageWriteWithoutFormat :: Bool
  , shaderUniformBufferArrayDynamicIndexing :: Bool
  , shaderSampledImageArrayDynamicIndexing :: Bool
  , shaderStorageBufferArrayDynamicIndexing :: Bool
  , shaderStorageImageArrayDynamicIndexing :: Bool
  , shaderClipDistance :: Bool
  , shaderCullDistance :: Bool
  , shaderFloat64 :: Bool
  , shaderInt64 :: Bool
  , shaderInt16 :: Bool
  , shaderResourceResidency :: Bool
  , shaderResourceMinLod :: Bool
  , sparseBinding :: Bool
  , sparseResidencyBuffer :: Bool
  , sparseResidencyImage2D :: Bool
  , sparseResidencyImage3D :: Bool
  , sparseResidency2Samples :: Bool
  , sparseResidency4Samples :: Bool
  , sparseResidency8Samples :: Bool
  , sparseResidency16Samples :: Bool
  , sparseResidencyAliased :: Bool
  , variableMultisampleRate :: Bool
  , inheritedQueries :: Bool
  } deriving (Eq, Ord, Show)

vkPhysicalDeviceFeatures ::
  Vk.VkPhysicalDeviceFeatures ->
  VkPhysicalDeviceFeatures
vkPhysicalDeviceFeatures p =
  VkPhysicalDeviceFeatures
  { robustBufferAccess = Vk.getField @"robustBufferAccess" p == Vk.VK_TRUE
  , fullDrawIndexUint32 = Vk.getField @"fullDrawIndexUint32" p == Vk.VK_TRUE
  , imageCubeArray = Vk.getField @"imageCubeArray" p == Vk.VK_TRUE
  , independentBlend = Vk.getField @"independentBlend" p == Vk.VK_TRUE
  , geometryShader = Vk.getField @"geometryShader" p == Vk.VK_TRUE
  , tessellationShader = Vk.getField @"tessellationShader" p == Vk.VK_TRUE
  , sampleRateShading = Vk.getField @"sampleRateShading" p == Vk.VK_TRUE
  , dualSrcBlend = Vk.getField @"dualSrcBlend" p == Vk.VK_TRUE
  , logicOp = Vk.getField @"logicOp" p == Vk.VK_TRUE
  , multiDrawIndirect = Vk.getField @"multiDrawIndirect" p == Vk.VK_TRUE
  , drawIndirectFirstInstance = Vk.getField @"drawIndirectFirstInstance" p == Vk.VK_TRUE
  , depthClamp = Vk.getField @"depthClamp" p == Vk.VK_TRUE
  , depthBiasClamp = Vk.getField @"depthBiasClamp" p == Vk.VK_TRUE
  , fillModeNonSolid = Vk.getField @"fillModeNonSolid" p == Vk.VK_TRUE
  , depthBounds = Vk.getField @"depthBounds" p == Vk.VK_TRUE
  , wideLines = Vk.getField @"wideLines" p == Vk.VK_TRUE
  , largePoints = Vk.getField @"largePoints" p == Vk.VK_TRUE
  , alphaToOne = Vk.getField @"alphaToOne" p == Vk.VK_TRUE
  , multiViewport = Vk.getField @"multiViewport" p == Vk.VK_TRUE
  , samplerAnisotropy = Vk.getField @"samplerAnisotropy" p == Vk.VK_TRUE
  , textureCompressionETC2 = Vk.getField @"textureCompressionETC2" p == Vk.VK_TRUE
  , textureCompressionASTC_LDR = Vk.getField @"textureCompressionASTC_LDR" p == Vk.VK_TRUE
  , textureCompressionBC = Vk.getField @"textureCompressionBC" p == Vk.VK_TRUE
  , occlusionQueryPrecise = Vk.getField @"occlusionQueryPrecise" p == Vk.VK_TRUE
  , pipelineStatisticsQuery = Vk.getField @"pipelineStatisticsQuery" p == Vk.VK_TRUE
  , vertexPipelineStoresAndAtomics = Vk.getField @"vertexPipelineStoresAndAtomics" p == Vk.VK_TRUE
  , fragmentStoresAndAtomics = Vk.getField @"fragmentStoresAndAtomics" p == Vk.VK_TRUE
  , shaderTessellationAndGeometryPointSize =
      Vk.getField @"shaderTessellationAndGeometryPointSize" p == Vk.VK_TRUE
  , shaderImageGatherExtended = Vk.getField @"shaderImageGatherExtended" p == Vk.VK_TRUE
  , shaderStorageImageExtendedFormats =
      Vk.getField @"shaderStorageImageExtendedFormats" p == Vk.VK_TRUE
  , shaderStorageImageMultisample = Vk.getField @"shaderStorageImageMultisample" p == Vk.VK_TRUE
  , shaderStorageImageReadWithoutFormat =
      Vk.getField @"shaderStorageImageReadWithoutFormat" p == Vk.VK_TRUE
  , shaderStorageImageWriteWithoutFormat =
      Vk.getField @"shaderStorageImageWriteWithoutFormat" p == Vk.VK_TRUE
  , shaderUniformBufferArrayDynamicIndexing =
      Vk.getField @"shaderUniformBufferArrayDynamicIndexing" p == Vk.VK_TRUE
  , shaderSampledImageArrayDynamicIndexing =
      Vk.getField @"shaderSampledImageArrayDynamicIndexing" p == Vk.VK_TRUE
  , shaderStorageBufferArrayDynamicIndexing =
      Vk.getField @"shaderStorageBufferArrayDynamicIndexing" p == Vk.VK_TRUE
  , shaderStorageImageArrayDynamicIndexing =
      Vk.getField @"shaderStorageImageArrayDynamicIndexing" p == Vk.VK_TRUE
  , shaderClipDistance = Vk.getField @"shaderClipDistance" p == Vk.VK_TRUE
  , shaderCullDistance = Vk.getField @"shaderCullDistance" p == Vk.VK_TRUE
  , shaderFloat64 = Vk.getField @"shaderFloat64" p == Vk.VK_TRUE
  , shaderInt64 = Vk.getField @"shaderInt64" p == Vk.VK_TRUE
  , shaderInt16 = Vk.getField @"shaderInt16" p == Vk.VK_TRUE
  , shaderResourceResidency = Vk.getField @"shaderResourceResidency" p == Vk.VK_TRUE
  , shaderResourceMinLod = Vk.getField @"shaderResourceMinLod" p == Vk.VK_TRUE
  , sparseBinding = Vk.getField @"sparseBinding" p == Vk.VK_TRUE
  , sparseResidencyBuffer = Vk.getField @"sparseResidencyBuffer" p == Vk.VK_TRUE
  , sparseResidencyImage2D = Vk.getField @"sparseResidencyImage2D" p == Vk.VK_TRUE
  , sparseResidencyImage3D = Vk.getField @"sparseResidencyImage3D" p == Vk.VK_TRUE
  , sparseResidency2Samples = Vk.getField @"sparseResidency2Samples" p == Vk.VK_TRUE
  , sparseResidency4Samples = Vk.getField @"sparseResidency4Samples" p == Vk.VK_TRUE
  , sparseResidency8Samples = Vk.getField @"sparseResidency8Samples" p == Vk.VK_TRUE
  , sparseResidency16Samples = Vk.getField @"sparseResidency16Samples" p == Vk.VK_TRUE
  , sparseResidencyAliased = Vk.getField @"sparseResidencyAliased" p == Vk.VK_TRUE
  , variableMultisampleRate = Vk.getField @"variableMultisampleRate" p == Vk.VK_TRUE
  , inheritedQueries = Vk.getField @"inheritedQueries" p == Vk.VK_TRUE
  }

vkGetPhysicalDeviceFeatures ::
  MonadIO m =>
  Vk.VkPhysicalDevice ->
  m VkPhysicalDeviceFeatures
vkGetPhysicalDeviceFeatures d =
  liftIO $
  Foreign.alloca $ \fPtr -> do
    Vk.vkGetPhysicalDeviceFeatures d fPtr
    vkPhysicalDeviceFeatures <$> Foreign.peek fPtr

vkGetPhysicalDeviceQueueFamilyProperties ::
  MonadIO m =>
  Vk.VkPhysicalDevice ->
  m [VkQueueFamilyProperties]
vkGetPhysicalDeviceQueueFamilyProperties p =
  liftIO $
  Foreign.alloca $ \countPtr -> do
    Vk.vkGetPhysicalDeviceQueueFamilyProperties p countPtr Foreign.nullPtr
    count <- fromIntegral <$> Foreign.peek countPtr
    Foreign.allocaArray count $ \arrayPtr -> do
      Vk.vkGetPhysicalDeviceQueueFamilyProperties p countPtr arrayPtr
      fmap vkQueueFamilyProperties <$> Foreign.peekArray count arrayPtr
