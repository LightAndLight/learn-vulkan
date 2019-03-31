{-# language DataKinds, TypeApplications #-}
module Graphics.Vulkan.Command.RenderPass where

import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Foreign
import qualified Graphics.Vulkan.Core_1_0 as Vk
import qualified Graphics.Vulkan.Marshal as Vk

import Graphics.Vulkan.ClearValue (VkClearValue, unVkClearValue)
import Graphics.Vulkan.Rect (VkRect2D, unVkRect2D)
import Graphics.Vulkan.SubpassContents (VkSubpassContents, unVkSubpassContents)

data VkRenderPassBeginInfo
  = VkRenderPassBeginInfo
  { renderPass :: Vk.VkRenderPass
  , framebuffer :: Vk.VkFramebuffer
  , renderArea :: VkRect2D
  , pClearValues :: [VkClearValue]
  } deriving (Eq, Ord, Show)

unVkRenderPassBeginInfo ::
  MonadIO m =>
  VkRenderPassBeginInfo ->
  m Vk.VkRenderPassBeginInfo
unVkRenderPassBeginInfo a = do
  cvs <- traverse unVkClearValue (pClearValues a)
  liftIO . Foreign.withArray cvs $ \cvPtr ->
    Vk.newVkData $ \ptr -> do
      Vk.writeField @"sType" ptr Vk.VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO
      Vk.writeField @"pNext" ptr Vk.VK_NULL
      Vk.writeField @"renderPass" ptr (renderPass a)
      Vk.writeField @"framebuffer" ptr (framebuffer a)
      Vk.writeField @"renderArea" ptr =<< unVkRect2D (renderArea a)
      Vk.writeField @"clearValueCount" ptr (fromIntegral . length $ pClearValues a)
      Vk.writeField @"pClearValues" ptr cvPtr

vkCmdBeginRenderPass ::
  MonadIO m =>
  Vk.VkCommandBuffer ->
  VkRenderPassBeginInfo ->
  VkSubpassContents ->
  m ()
vkCmdBeginRenderPass buf info sp = do
  info' <- unVkRenderPassBeginInfo info
  liftIO $ Vk.vkCmdBeginRenderPass buf (Vk.unsafePtr info') (unVkSubpassContents sp)

vkCmdEndRenderPass :: MonadIO m => Vk.VkCommandBuffer -> m ()
vkCmdEndRenderPass = do liftIO . Vk.vkCmdEndRenderPass

withCmdRenderPass ::
  MonadIO m =>
  Vk.VkCommandBuffer ->
  VkRenderPassBeginInfo ->
  VkSubpassContents ->
  m a ->
  m a
withCmdRenderPass buf info sp m = vkCmdBeginRenderPass buf info sp *> m <* vkCmdEndRenderPass buf
