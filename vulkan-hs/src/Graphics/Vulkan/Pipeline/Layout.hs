module Graphics.Vulkan.Pipeline.Layout
  ( Vk.VkPipelineLayout
  , vkCreatePipelineLayout
  )
where

import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Managed.Safe (MonadManaged, using, managed)

import qualified Foreign
import qualified Graphics.Vulkan.Core_1_0 as Vk
import qualified Graphics.Vulkan.Marshal as Vk

import Graphics.Vulkan.Pipeline.LayoutCreateInfo
  ( VkPipelineLayoutCreateInfo
  , unVkPipelineLayoutCreateInfo
  )
import Graphics.Vulkan.Result (vkResult)

vkCreatePipelineLayout ::
  (MonadManaged m, MonadIO m) =>
  Vk.VkDevice ->
  VkPipelineLayoutCreateInfo ->
  Foreign.Ptr Vk.VkAllocationCallbacks ->
  m Vk.VkPipelineLayout
vkCreatePipelineLayout d info cbs = do
  info' <- unVkPipelineLayoutCreateInfo info
  layoutPtr <- using $ managed Foreign.alloca
  liftIO $ vkResult =<< Vk.vkCreatePipelineLayout d (Vk.unsafePtr info') cbs layoutPtr
  using $ managed (bracket
    (Foreign.peek layoutPtr)
    (\l -> Vk.vkDestroyPipelineLayout d l cbs))
