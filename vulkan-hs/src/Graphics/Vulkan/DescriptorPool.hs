{-# language DataKinds, TypeApplications #-}
module Graphics.Vulkan.DescriptorPool
  ( Vk.VkDescriptorPool
  , vkCreateDescriptorPool
  , VkDescriptorPoolSize(..), unVkDescriptorPoolSize
  , VkDescriptorPoolCreateFlag(..), unVkDescriptorPoolCreateBit, unVkDescriptorPoolCreateBits
  , VkDescriptorPoolCreateInfo(..), unVkDescriptorPoolCreateInfo
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
import qualified Graphics.Vulkan.Marshal as Vk

import Graphics.Vulkan.DescriptorType (VkDescriptorType, unVkDescriptorType)
import Graphics.Vulkan.Result (vkResult)

data VkDescriptorPoolSize
  = VkDescriptorPoolSize
  { descriptorType :: VkDescriptorType
  , descriptorCount :: Word32
  } deriving (Eq, Ord, Show)

unVkDescriptorPoolSize ::
  MonadIO m =>
  VkDescriptorPoolSize ->
  m Vk.VkDescriptorPoolSize
unVkDescriptorPoolSize a =
  liftIO . Vk.newVkData $ \ptr -> do
    Vk.writeField @"type" ptr (unVkDescriptorType $ descriptorType a)
    Vk.writeField @"descriptorCount" ptr (descriptorCount a)

data VkDescriptorPoolCreateFlag
  = FreeDescriptorSet
  | UpdateAfterBindEXT
  deriving (Eq, Ord, Show)

unVkDescriptorPoolCreateBit ::
  VkDescriptorPoolCreateFlag ->
  Vk.VkDescriptorPoolCreateBitmask a
unVkDescriptorPoolCreateBit a =
  case a of
    FreeDescriptorSet ->
      Vk.VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT
    UpdateAfterBindEXT ->
      unsafeCoerce Vk.VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT

unVkDescriptorPoolCreateBits ::
  [VkDescriptorPoolCreateFlag] ->
  Vk.VkDescriptorPoolCreateFlags
unVkDescriptorPoolCreateBits = foldr (\a b -> unVkDescriptorPoolCreateBit a .|. b) 0

data VkDescriptorPoolCreateInfo
  = VkDescriptorPoolCreateInfo
  { flags :: [VkDescriptorPoolCreateFlag]
  , maxSets :: Word32
  , pPoolSizes :: [VkDescriptorPoolSize]
  } deriving (Eq, Ord, Show)

unVkDescriptorPoolCreateInfo ::
  MonadIO m =>
  VkDescriptorPoolCreateInfo ->
  m Vk.VkDescriptorPoolCreateInfo
unVkDescriptorPoolCreateInfo a = do
  ss <- traverse unVkDescriptorPoolSize $ pPoolSizes a
  liftIO . Foreign.withArray ss $ \sPtr ->
    Vk.newVkData $ \ptr -> do
      Vk.writeField @"sType" ptr Vk.VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO
      Vk.writeField @"pNext" ptr Foreign.nullPtr
      Vk.writeField @"flags" ptr (unVkDescriptorPoolCreateBits $ flags a)
      Vk.writeField @"maxSets" ptr (maxSets a)
      Vk.writeField @"poolSizeCount" ptr (fromIntegral $ length ss)
      Vk.writeField @"pPoolSizes" ptr sPtr

vkCreateDescriptorPool ::
  MonadManaged m =>
  Vk.VkDevice ->
  VkDescriptorPoolCreateInfo ->
  Foreign.Ptr Vk.VkAllocationCallbacks ->
  m Vk.VkDescriptorPool
vkCreateDescriptorPool dev info cbs = do
  info' <- unVkDescriptorPoolCreateInfo info
  pPtr <- using $ managed Foreign.alloca
  liftIO $ vkResult =<< Vk.vkCreateDescriptorPool dev (Vk.unsafePtr info') cbs pPtr
  using $ managed (bracket (Foreign.peek pPtr) (\p -> Vk.vkDestroyDescriptorPool dev p cbs))
