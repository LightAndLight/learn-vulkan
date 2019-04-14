{-# language DataKinds, TypeApplications #-}
module Graphics.Vulkan.DescriptorSetLayout where

import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Managed (MonadManaged, using, managed)

import qualified Foreign
import qualified Graphics.Vulkan.Core_1_0 as Vk
import qualified Graphics.Vulkan.Marshal as Vk

vkCreateDescriptorSetLayout ::
  Vk.VkDevice ->
  VkDescriptorSetLayoutCreateInfo ->
  Foreign.Ptr Vk.VkAllocatorCallbacks ->
  m Vk.VkDescriptorSetLayout
vkCreateDescriptorSetLayout = _
