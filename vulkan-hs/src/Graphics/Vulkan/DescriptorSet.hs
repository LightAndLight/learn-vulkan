{-# language DataKinds, TypeApplications #-}
module Graphics.Vulkan.DescriptorSet
  ( Vk.VkDescriptorSet
  , vkAllocateDescriptorSets
  , VkDescriptorSetAllocateInfo(..), unVkDescriptorSetAllocateInfo
  )
where

import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Managed.Safe (MonadManaged, using, managed)

import qualified Foreign
import qualified Graphics.Vulkan.Core_1_0 as Vk
import qualified Graphics.Vulkan.Marshal as Vk

import Graphics.Vulkan.Result (vkResult)

data VkDescriptorSetAllocateInfo
  = VkDescriptorSetAllocateInfo
  { descriptorPool :: Vk.VkDescriptorPool
  , pSetLayouts :: [Vk.VkDescriptorSetLayout]
  } deriving (Eq, Ord, Show)

unVkDescriptorSetAllocateInfo ::
  MonadIO m =>
  VkDescriptorSetAllocateInfo ->
  m Vk.VkDescriptorSetAllocateInfo
unVkDescriptorSetAllocateInfo a =
  liftIO . Foreign.withArray (pSetLayouts a) $ \lPtr ->
  Vk.newVkData $ \ptr -> do
    Vk.writeField @"sType" ptr Vk.VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO
    Vk.writeField @"pNext" ptr Foreign.nullPtr
    Vk.writeField @"descriptorSetCount" ptr (fromIntegral . length $ pSetLayouts a)
    Vk.writeField @"pSetLayouts" ptr lPtr

vkAllocateDescriptorSets ::
  MonadManaged m =>
  Vk.VkDevice ->
  VkDescriptorSetAllocateInfo ->
  m [Vk.VkDescriptorSet]
vkAllocateDescriptorSets dev info = do
  let size = length $ pSetLayouts info
  sPtr <- using $ managed (Foreign.allocaArray size)
  info' <- unVkDescriptorSetAllocateInfo info
  liftIO $ vkResult =<< Vk.vkAllocateDescriptorSets dev (Vk.unsafePtr info') sPtr
  using $ managed (bracket
    (Foreign.peekArray size sPtr)
    (\_ -> Vk.vkFreeDescriptorSets dev (descriptorPool info) (fromIntegral size) sPtr))
