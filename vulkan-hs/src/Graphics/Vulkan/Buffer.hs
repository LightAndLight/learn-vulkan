{-# language DataKinds, TypeApplications #-}
module Graphics.Vulkan.Buffer
  ( Vk.VkBuffer
  , vkCreateBuffer
  , vkGetBufferMemoryRequirements
  , vkBindBufferMemory
  , VkBufferCreateFlag(..)
  , unVkBufferCreateBit, unVkBufferCreateBits
  , VkBufferUsageFlag(..)
  , unVkBufferUsageBit, unVkBufferUsageBits
  , VkBufferCreateInfo(..), unVkBufferCreateInfo
  )
where

import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Managed.Safe (MonadManaged, using, managed)
import Data.Bits ((.|.))
import Data.Word (Word32)
import Unsafe.Coerce (unsafeCoerce)

import qualified Foreign
import qualified Graphics.Vulkan.Core_1_0 as Vk
import qualified Graphics.Vulkan.Core_1_1 as Vk
-- import qualified Graphics.Vulkan.Ext.VK_EXT_buffer_device_address as Vk
-- import qualified Graphics.Vulkan.Ext.VK_EXT_conditional_rendering as Vk
-- import qualified Graphics.Vulkan.Ext.VK_EXT_transform_feedback as Vk
-- import qualified Graphics.Vulkan.Ext.VK_NV_ray_tracing as Vk
import qualified Graphics.Vulkan.Marshal as Vk

import Graphics.Vulkan.MemoryRequirements (VkMemoryRequirements, vkMemoryRequirements)
import Graphics.Vulkan.Result (vkResult)
import Graphics.Vulkan.SharingMode (VkSharingMode, unVkSharingMode)

data VkBufferCreateFlag
  = SparseBinding
  | SparseResidency
  | SparseAliased
  | Protected
  | DeviceAddressCaptureReplayEXT
  deriving (Eq, Ord, Show)

unVkBufferCreateBit ::
  VkBufferCreateFlag ->
  Vk.VkBufferCreateBitmask a
unVkBufferCreateBit a =
  case a of
    SparseBinding ->
      Vk.VK_BUFFER_CREATE_SPARSE_BINDING_BIT
    SparseResidency ->
      Vk.VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT
    SparseAliased ->
      Vk.VK_BUFFER_CREATE_SPARSE_ALIASED_BIT
    Protected ->
      unsafeCoerce Vk.VK_BUFFER_CREATE_PROTECTED_BIT
    -- DeviceAddressCaptureReplayEXT ->
      -- Vk.VK_BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT

unVkBufferCreateBits ::
  [VkBufferCreateFlag] ->
  Vk.VkBufferCreateFlags
unVkBufferCreateBits = foldr (\a b -> unVkBufferCreateBit a .|. b) 0

data VkBufferUsageFlag
  = TransferSrc
  | TransferDst
  | UniformTexelBuffer
  | StorageTexelBuffer
  | UniformBuffer
  | StorageBuffer
  | IndexBuffer
  | VertexBuffer
  | IndirectBuffer
  | TransformFeedbackBufferEXT
  | TransformFeedbackCounterBufferEXT
  | ConditionalRenderingEXT
  | RayTracingNV
  | ShaderDeviceAddressEXT
  deriving (Eq, Ord, Show)

unVkBufferUsageBit ::
  VkBufferUsageFlag ->
  Vk.VkBufferUsageBitmask a
unVkBufferUsageBit a =
  case a of
    TransferSrc ->
      Vk.VK_BUFFER_USAGE_TRANSFER_SRC_BIT
    TransferDst ->
      Vk.VK_BUFFER_USAGE_TRANSFER_DST_BIT
    UniformTexelBuffer ->
      Vk.VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT
    StorageTexelBuffer ->
      Vk.VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT
    UniformBuffer ->
      Vk.VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT
    StorageBuffer ->
      Vk.VK_BUFFER_USAGE_STORAGE_BUFFER_BIT
    IndexBuffer ->
      Vk.VK_BUFFER_USAGE_INDEX_BUFFER_BIT
    VertexBuffer ->
      Vk.VK_BUFFER_USAGE_VERTEX_BUFFER_BIT
    IndirectBuffer ->
      Vk.VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT
    -- TransformFeedbackBufferEXT ->
      -- Vk.VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT
    -- TransformFeedbackCounterBufferEXT ->
      -- Vk.VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT
    -- ConditionalRenderingEXT ->
      -- Vk.VK_BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT
    -- RayTracingNV ->
      -- Vk.VK_BUFFER_USAGE_RAY_TRACING_BIT_NV
    -- ShaderDeviceAddressEXT ->
      -- Vk.VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_EXT

unVkBufferUsageBits ::
  [VkBufferUsageFlag] ->
  Vk.VkBufferUsageFlags
unVkBufferUsageBits = foldr (\a b -> unVkBufferUsageBit a .|. b) 0

data VkBufferCreateInfo
  = VkBufferCreateInfo
  { flags :: [VkBufferCreateFlag]
  , size :: Vk.VkDeviceSize
  , usage :: [VkBufferUsageFlag]
  , sharingMode :: VkSharingMode
  , pQueueFamilyIndices :: [Word32]
  } deriving (Eq, Ord, Show)

unVkBufferCreateInfo :: MonadIO m => VkBufferCreateInfo -> m Vk.VkBufferCreateInfo
unVkBufferCreateInfo info =
  liftIO . Foreign.withArray (pQueueFamilyIndices info) $ \ixPtr ->
  Vk.newVkData $ \ptr -> do
    Vk.writeField @"sType" ptr Vk.VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO
    Vk.writeField @"pNext" ptr Foreign.nullPtr
    Vk.writeField @"flags" ptr $ unVkBufferCreateBits (flags info)
    Vk.writeField @"size" ptr $ size info
    Vk.writeField @"usage" ptr $ unVkBufferUsageBits (usage info)
    Vk.writeField @"sharingMode" ptr $ unVkSharingMode (sharingMode info)
    Vk.writeField @"queueFamilyIndexCount" ptr (fromIntegral . length $ pQueueFamilyIndices info)
    Vk.writeField @"pQueueFamilyIndices" ptr ixPtr

vkCreateBuffer ::
  MonadManaged m =>
  Vk.VkDevice ->
  VkBufferCreateInfo ->
  Foreign.Ptr Vk.VkAllocationCallbacks ->
  m Vk.VkBuffer
vkCreateBuffer dev info cbs = do
  info' <- unVkBufferCreateInfo info
  bufPtr <- using $ managed Foreign.alloca
  liftIO $ vkResult =<< Vk.vkCreateBuffer dev (Vk.unsafePtr info') cbs bufPtr
  using $ managed (bracket (Foreign.peek bufPtr) (\b -> Vk.vkDestroyBuffer dev b cbs))

vkGetBufferMemoryRequirements ::
  MonadIO m =>
  Vk.VkDevice ->
  Vk.VkBuffer ->
  m VkMemoryRequirements
vkGetBufferMemoryRequirements dev buf =
  liftIO . Foreign.alloca $ \mPtr -> do
    Vk.vkGetBufferMemoryRequirements dev buf mPtr
    vkMemoryRequirements <$> Foreign.peek mPtr

vkBindBufferMemory ::
  MonadIO m =>
  Vk.VkDevice ->
  Vk.VkBuffer ->
  Vk.VkDeviceMemory ->
  Vk.VkDeviceSize ->
  m ()
vkBindBufferMemory dev buf mem off =
  liftIO $ vkResult =<< Vk.vkBindBufferMemory dev buf mem off
