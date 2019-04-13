module Graphics.Vulkan.ImageView
  (Vk.VkImageView, vkCreateImageView)
where

import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Managed.Safe (MonadManaged, using, managed)

import qualified Foreign.Marshal.Alloc as Foreign
import qualified Foreign.Ptr as Foreign
import qualified Foreign.Storable as Foreign
import qualified Graphics.Vulkan.Core_1_0 as Vk

import Graphics.Vulkan.ImageViewCreateInfo (VkImageViewCreateInfo, unVkImageViewCreateInfo)
import Graphics.Vulkan.Result (vkResult)

vkCreateImageView ::
  (MonadManaged m, MonadIO m) =>
  Vk.VkDevice ->
  VkImageViewCreateInfo ->
  Foreign.Ptr Vk.VkAllocationCallbacks ->
  m Vk.VkImageView
vkCreateImageView d info cbs = do
  ivPtr <- using $ managed Foreign.alloca
  info' <- unVkImageViewCreateInfo info
  liftIO $ vkResult =<< Vk.vkCreateImageView d (Vk.unsafePtr info') cbs ivPtr
  using $ managed (bracket (Foreign.peek ivPtr) (\iv -> Vk.vkDestroyImageView d iv cbs))
