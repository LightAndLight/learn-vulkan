{-# language DataKinds, TypeApplications #-}
{-# language DuplicateRecordFields #-}
module Graphics.Vulkan.Offset where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Int (Int32)

import qualified Graphics.Vulkan.Core_1_0 as Vk
import qualified Graphics.Vulkan.Marshal as Vk

data VkOffset2D
  = VkOffset2D
  { x :: Int32
  , y :: Int32
  } deriving (Eq, Ord, Show)

vkOffset2D :: Vk.VkOffset2D -> VkOffset2D
vkOffset2D a =
  VkOffset2D
  { x = Vk.getField @"x" a
  , y = Vk.getField @"y" a
  }

unVkOffset2D :: MonadIO m => VkOffset2D -> m Vk.VkOffset2D
unVkOffset2D a =
  liftIO . Vk.newVkData $ \ptr -> do
    Vk.writeField @"x" ptr (x (a :: VkOffset2D))
    Vk.writeField @"y" ptr (y (a :: VkOffset2D))

data VkOffset3D
  = VkOffset3D
  { x :: Int32
  , y :: Int32
  , z :: Int32
  } deriving (Eq, Ord, Show)

vkOffset3D :: Vk.VkOffset3D -> VkOffset3D
vkOffset3D a =
  VkOffset3D
  { x = Vk.getField @"x" a
  , y = Vk.getField @"y" a
  , z = Vk.getField @"z" a
  }

unVkOffset3D :: MonadIO m => VkOffset3D -> m Vk.VkOffset3D
unVkOffset3D a =
  liftIO . Vk.newVkData $ \ptr -> do
    Vk.writeField @"x" ptr (x (a :: VkOffset3D))
    Vk.writeField @"y" ptr (y (a :: VkOffset3D))
    Vk.writeField @"z" ptr (z (a :: VkOffset3D))
