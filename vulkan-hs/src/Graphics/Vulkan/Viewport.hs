{-# language DataKinds, TypeApplications #-}
module Graphics.Vulkan.Viewport where

import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Graphics.Vulkan.Core_1_0 as Vk
import qualified Graphics.Vulkan.Marshal as Vk

data VkViewport
  = VkViewport
  { x :: Float
  , y :: Float
  , width :: Float
  , height :: Float
  , minDepth :: Float
  , maxDepth :: Float
  } deriving (Eq, Ord, Show)

vkViewport :: Vk.VkViewport -> VkViewport
vkViewport a =
  VkViewport
  { x = Vk.getField @"x" a
  , y = Vk.getField @"y" a
  , width = Vk.getField @"width" a
  , height = Vk.getField @"height" a
  , minDepth = Vk.getField @"minDepth" a
  , maxDepth = Vk.getField @"maxDepth" a
  }

unVkViewport :: MonadIO m => VkViewport -> m Vk.VkViewport
unVkViewport a =
  liftIO . Vk.newVkData $ \ptr -> do
    Vk.writeField @"x" ptr (x a)
    Vk.writeField @"y" ptr (y a)
    Vk.writeField @"width" ptr (width a)
    Vk.writeField @"height" ptr (height a)
    Vk.writeField @"minDepth" ptr (minDepth a)
    Vk.writeField @"maxDepth" ptr (maxDepth a)
