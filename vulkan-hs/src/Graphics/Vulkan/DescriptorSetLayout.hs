{-# language DataKinds, TypeApplications #-}
module Graphics.Vulkan.DescriptorSetLayout
  ( Vk.VkDescriptorSetLayout
  , vkCreateDescriptorSetLayout
  , VkDescriptorSetLayoutCreateInfo(..), unVkDescriptorSetLayoutCreateInfo
  , VkDescriptorSetLayoutCreateFlag(..)
  , unVkDescriptorSetLayoutCreateBit, unVkDescriptorSetLayoutCreateBits
  , VkDescriptorSetLayoutBinding(..), unVkDescriptorSetLayoutBinding
  )
where

import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Managed (MonadManaged, using, managed)
import Data.Bits ((.|.))
import Data.Word (Word32)
import Unsafe.Coerce (unsafeCoerce)

import qualified Foreign
import qualified Graphics.Vulkan.Core_1_0 as Vk
import qualified Graphics.Vulkan.Ext.VK_EXT_descriptor_indexing as Vk
import qualified Graphics.Vulkan.Ext.VK_KHR_push_descriptor as Vk
import qualified Graphics.Vulkan.Marshal as Vk

import Graphics.Vulkan.DescriptorType (VkDescriptorType, unVkDescriptorType)
import Graphics.Vulkan.ShaderStage (VkShaderStageFlag, unVkShaderStageBits)
import Graphics.Vulkan.Result (vkResult)

data VkDescriptorSetLayoutCreateFlag
  = PushDescriptorKHR
  | UpdateAfterBindPoolEXT
  deriving (Eq, Ord, Show)

unVkDescriptorSetLayoutCreateBit ::
  VkDescriptorSetLayoutCreateFlag ->
  Vk.VkDescriptorSetLayoutCreateBitmask a
unVkDescriptorSetLayoutCreateBit a =
  case a of
    PushDescriptorKHR -> unsafeCoerce Vk.VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR
    UpdateAfterBindPoolEXT ->
      unsafeCoerce Vk.VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT

unVkDescriptorSetLayoutCreateBits ::
  [VkDescriptorSetLayoutCreateFlag] ->
  Vk.VkDescriptorSetLayoutCreateFlags
unVkDescriptorSetLayoutCreateBits =
  foldr (\a b -> unVkDescriptorSetLayoutCreateBit a .|. b) 0

data VkDescriptorSetLayoutBinding
  = VkDescriptorSetLayoutBinding
  { binding :: Word32
  , descriptorType :: VkDescriptorType
  , descriptorCount :: Word32
  , stageFlags :: [VkShaderStageFlag]
  , pImmutableSamplers :: Maybe [Vk.VkSampler]
  } deriving (Eq, Ord, Show)

unVkDescriptorSetLayoutBinding ::
  MonadIO m =>
  VkDescriptorSetLayoutBinding ->
  m Vk.VkDescriptorSetLayoutBinding
unVkDescriptorSetLayoutBinding a =
  liftIO $
  case pImmutableSamplers a of
    Nothing -> go Foreign.nullPtr
    Just s -> Foreign.withArray s go
  where
    go :: Foreign.Ptr Vk.VkSampler -> IO Vk.VkDescriptorSetLayoutBinding
    go sPtr =
      Vk.newVkData $ \ptr -> do
        Vk.writeField @"binding" ptr (binding a)
        Vk.writeField @"descriptorType" ptr (unVkDescriptorType $ descriptorType a)
        Vk.writeField @"descriptorCount" ptr (descriptorCount a)
        Vk.writeField @"stageFlags" ptr (unVkShaderStageBits $ stageFlags a)
        Vk.writeField @"pImmutableSamplers" ptr sPtr

data VkDescriptorSetLayoutCreateInfo
  = VkDescriptorSetLayoutCreateInfo
  { flags :: [VkDescriptorSetLayoutCreateFlag]
  , pBindings :: [VkDescriptorSetLayoutBinding]
  } deriving (Eq, Ord, Show)

unVkDescriptorSetLayoutCreateInfo ::
  MonadIO m =>
  VkDescriptorSetLayoutCreateInfo ->
  m Vk.VkDescriptorSetLayoutCreateInfo
unVkDescriptorSetLayoutCreateInfo a = do
  bs <- traverse unVkDescriptorSetLayoutBinding $ pBindings a
  liftIO . Foreign.withArray bs $ \bPtr ->
    Vk.newVkData $ \ptr -> do
      Vk.writeField @"sType" ptr Vk.VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO
      Vk.writeField @"pNext" ptr Foreign.nullPtr
      Vk.writeField @"flags" ptr (unVkDescriptorSetLayoutCreateBits $ flags a)
      Vk.writeField @"bindingCount" ptr (fromIntegral . length $ pBindings a)
      Vk.writeField @"pBindings" ptr bPtr

vkCreateDescriptorSetLayout ::
  MonadManaged m =>
  Vk.VkDevice ->
  VkDescriptorSetLayoutCreateInfo ->
  Foreign.Ptr Vk.VkAllocationCallbacks ->
  m Vk.VkDescriptorSetLayout
vkCreateDescriptorSetLayout dev info cbs = do
  info' <- unVkDescriptorSetLayoutCreateInfo info
  lPtr <- using $ managed Foreign.alloca
  liftIO $ vkResult =<< Vk.vkCreateDescriptorSetLayout dev (Vk.unsafePtr info') cbs lPtr
  using $ managed (bracket (Foreign.peek lPtr) (\l -> Vk.vkDestroyDescriptorSetLayout dev l cbs))
