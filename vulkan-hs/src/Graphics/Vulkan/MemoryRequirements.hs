{-# language DataKinds, TypeApplications #-}
module Graphics.Vulkan.MemoryRequirements where

import Data.Word (Word32)

import qualified Graphics.Vulkan.Core_1_0 as Vk
import qualified Graphics.Vulkan.Marshal as Vk

data VkMemoryRequirements
  = VkMemoryRequirements
  { size :: Vk.VkDeviceSize
  , alignment :: Vk.VkDeviceSize
  , memoryTypeBits :: Word32
  } deriving (Eq, Ord, Show)

vkMemoryRequirements :: Vk.VkMemoryRequirements -> VkMemoryRequirements
vkMemoryRequirements a =
  VkMemoryRequirements
  { size = Vk.getField @"size" a
  , alignment = Vk.getField @"alignment" a
  , memoryTypeBits = Vk.getField @"memoryTypeBits" a
  }
