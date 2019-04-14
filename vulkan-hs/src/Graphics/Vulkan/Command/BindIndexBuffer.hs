module Graphics.Vulkan.Command.BindIndexBuffer where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Word (Word32)

import qualified Foreign
import qualified Graphics.Vulkan.Core_1_0 as Vk

import Graphics.Vulkan.IndexType (VkIndexType, unVkIndexType)

vkCmdBindIndexBuffer ::
  MonadIO m =>
  Vk.VkCommandBuffer ->
  Vk.VkBuffer ->
  Vk.VkDeviceSize ->
  VkIndexType ->
  m ()
vkCmdBindIndexBuffer cmdBuf buf off ixType =
  liftIO $ Vk.vkCmdBindIndexBuffer cmdBuf buf off (unVkIndexType ixType)
