{-# language DataKinds, TypeApplications #-}
{-# language DuplicateRecordFields #-}
module Graphics.Vulkan.Extent where

import Data.Word (Word32)

import qualified Graphics.Vulkan.Core_1_0 as Vk
import qualified Graphics.Vulkan.Marshal as Vk

data VkExtent3D
  = VkExtent3D
  { width :: Word32
  , height :: Word32
  , depth :: Word32
  } deriving (Eq, Ord, Show)

vkExtent3D :: Vk.VkExtent3D -> VkExtent3D
vkExtent3D p =
  VkExtent3D
  { width = Vk.getField @"width" p
  , height = Vk.getField @"height" p
  , depth = Vk.getField @"depth" p
  }

data VkExtent2D
  = VkExtent2D
  { width :: Word32
  , height :: Word32
  } deriving (Eq, Ord, Show)

vkExtent2D :: Vk.VkExtent2D -> VkExtent2D
vkExtent2D p =
  VkExtent2D
  { width = Vk.getField @"width" p
  , height = Vk.getField @"height" p
  }
