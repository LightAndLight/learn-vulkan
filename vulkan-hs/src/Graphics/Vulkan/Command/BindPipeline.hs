module Graphics.Vulkan.Command.BindPipeline where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Word (Word32)

import qualified Graphics.Vulkan.Core_1_0 as Vk

import Graphics.Vulkan.Pipeline.BindPoint (VkPipelineBindPoint, unVkPipelineBindPoint)

vkCmdBindPipeline :: MonadIO m => Vk.VkCommandBuffer -> VkPipelineBindPoint -> Vk.VkPipeline -> m ()
vkCmdBindPipeline buf bp p = liftIO $ Vk.vkCmdBindPipeline buf (unVkPipelineBindPoint bp) p
