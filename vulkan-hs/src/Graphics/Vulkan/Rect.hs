{-# language DataKinds, TypeApplications #-}
module Graphics.Vulkan.Rect where

import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Graphics.Vulkan.Core_1_0 as Vk
import qualified Graphics.Vulkan.Marshal as Vk

import Graphics.Vulkan.Offset
import Graphics.Vulkan.Extent

data VkRect2D
  = VkRect2D
  { offset :: VkOffset2D
  , extent :: VkExtent2D
  } deriving (Eq, Ord, Show)

vkRect2D :: Vk.VkRect2D -> VkRect2D
vkRect2D a =
  VkRect2D
  { offset = vkOffset2D $ Vk.getField @"offset" a
  , extent = vkExtent2D $ Vk.getField @"extent" a
  }

unVkRect2D :: MonadIO m => VkRect2D -> m Vk.VkRect2D
unVkRect2D a =
  liftIO . Vk.newVkData $ \ptr -> do
    Vk.writeField @"offset" ptr =<< unVkOffset2D (offset a)
    Vk.writeField @"extent" ptr =<< unVkExtent2D (extent a)
