{-# language DataKinds, TypeApplications #-}
{-# language DuplicateRecordFields #-}
module Graphics.Vulkan.Extent where

import Control.Monad.IO.Class (MonadIO, liftIO)
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

unVkExtent3D :: MonadIO m => VkExtent3D -> m Vk.VkExtent3D
unVkExtent3D p =
  liftIO . Vk.newVkData $ \extPtr -> do
    Vk.writeField @"width" extPtr (width (p :: VkExtent3D))
    Vk.writeField @"height" extPtr (height (p :: VkExtent3D))
    Vk.writeField @"depth" extPtr (depth p)

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

unVkExtent2D :: MonadIO m => VkExtent2D -> m Vk.VkExtent2D
unVkExtent2D p =
  liftIO . Vk.newVkData $ \extPtr -> do
    Vk.writeField @"width" extPtr (width (p :: VkExtent2D))
    Vk.writeField @"height" extPtr (height (p :: VkExtent2D))
