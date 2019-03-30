module Graphics.Vulkan.Framebuffer
  (Vk.VkFramebuffer, vkCreateFramebuffer)
where

import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Managed.Safe (MonadManaged, using, managed)

import qualified Foreign
import qualified Graphics.Vulkan.Core_1_0 as Vk

import Graphics.Vulkan.FramebufferCreateInfo (VkFramebufferCreateInfo, unVkFramebufferCreateInfo)
import Graphics.Vulkan.Result (vkResult)

vkCreateFramebuffer ::
  (MonadManaged m, MonadIO m) =>
  Vk.VkDevice ->
  VkFramebufferCreateInfo ->
  Foreign.Ptr Vk.VkAllocationCallbacks ->
  m Vk.VkFramebuffer
vkCreateFramebuffer d info cbs = do
  info' <- unVkFramebufferCreateInfo info
  fbPtr <- using $ managed Foreign.alloca
  liftIO $ vkResult =<< Vk.vkCreateFramebuffer d (Vk.unsafePtr info') cbs fbPtr
  using $ managed (bracket (Foreign.peek fbPtr) (\fb -> Vk.vkDestroyFramebuffer d fb cbs))
