{-# language EmptyCase, EmptyDataDeriving #-}
{-# language DataKinds, TypeApplications #-}
module Graphics.Vulkan.FramebufferCreateInfo where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Word (Word32)

import qualified Foreign
import qualified Graphics.Vulkan.Core_1_0 as Vk
import qualified Graphics.Vulkan.Marshal as Vk

data VkFramebufferCreateFlag
  deriving (Eq, Ord, Show)

unVkFramebufferCreateBits ::
  [VkFramebufferCreateFlag] ->
  Vk.VkFramebufferCreateFlags
unVkFramebufferCreateBits [] = 0
unVkFramebufferCreateBits (x:_) = case x of

data VkFramebufferCreateInfo
  = VkFramebufferCreateInfo
  { flags :: [VkFramebufferCreateFlag]
  , renderPass :: Vk.VkRenderPass
  , pAttachments :: [Vk.VkImageView]
  , width :: Word32
  , height :: Word32
  , layers :: Word32
  } deriving (Eq, Ord, Show)

unVkFramebufferCreateInfo ::
  MonadIO m =>
  VkFramebufferCreateInfo ->
  m Vk.VkFramebufferCreateInfo
unVkFramebufferCreateInfo a =
  liftIO . Foreign.withArray (pAttachments a) $ \aPtr ->
    Vk.newVkData $ \ptr -> do
      Vk.writeField @"sType" ptr Vk.VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO
      Vk.writeField @"flags" ptr (unVkFramebufferCreateBits $ flags a)
      Vk.writeField @"renderPass" ptr (renderPass a)
      Vk.writeField @"attachmentCount" ptr (fromIntegral . length $ pAttachments a)
      Vk.writeField @"pAttachments" ptr aPtr
      Vk.writeField @"width" ptr (width a)
      Vk.writeField @"height" ptr (height a)
      Vk.writeField @"layers" ptr (layers a)
