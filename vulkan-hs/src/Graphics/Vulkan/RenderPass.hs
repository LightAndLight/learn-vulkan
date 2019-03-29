module Graphics.Vulkan.RenderPass
  (Vk.VkRenderPass, vkCreateRenderPass)
where

import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Managed.Safe (MonadManaged, using, managed)

import qualified Foreign
import qualified Graphics.Vulkan.Core_1_0 as Vk

import Graphics.Vulkan.RenderPassCreateInfo
  (VkRenderPassCreateInfo, unVkRenderPassCreateInfo)

vkCreateRenderPass ::
  (MonadManaged m, MonadIO m) =>
  Vk.VkDevice ->
  VkRenderPassCreateInfo ->
  Foreign.Ptr Vk.VkAllocationCallbacks ->
  m Vk.VkRenderPass
vkCreateRenderPass d info cbs = do
  rpPtr <- using $ managed Foreign.alloca
  info' <- unVkRenderPassCreateInfo info
  liftIO $ Vk.vkCreateRenderPass d (Vk.unsafePtr info') cbs rpPtr
  using $ managed (bracket
    (Foreign.peek rpPtr)
    (\rp -> Vk.vkDestroyRenderPass d rp cbs))
