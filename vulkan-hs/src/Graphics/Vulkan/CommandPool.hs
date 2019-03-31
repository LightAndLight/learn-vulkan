module Graphics.Vulkan.CommandPool (Vk.VkCommandPool, vkCreateCommandPool) where

import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Managed.Safe (MonadManaged, using, managed)

import qualified Foreign
import qualified Graphics.Vulkan.Core_1_0 as Vk

import Graphics.Vulkan.CommandPoolCreateInfo (VkCommandPoolCreateInfo, unVkCommandPoolCreateInfo)
import Graphics.Vulkan.Result (vkResult)

vkCreateCommandPool ::
  (MonadManaged m, MonadIO m) =>
  Vk.VkDevice ->
  VkCommandPoolCreateInfo ->
  Foreign.Ptr Vk.VkAllocationCallbacks ->
  m Vk.VkCommandPool
vkCreateCommandPool d info cbs = do
  info' <- unVkCommandPoolCreateInfo info
  pPtr <- using $ managed Foreign.alloca
  liftIO $ vkResult =<< Vk.vkCreateCommandPool d (Vk.unsafePtr info') cbs pPtr
  using $ managed (bracket (Foreign.peek pPtr) (\p -> Vk.vkDestroyCommandPool d p cbs))
