module Graphics.Vulkan.Command.BindDescriptorSets where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Word (Word32)

import qualified Foreign
import qualified Graphics.Vulkan.Core_1_0 as Vk

import Graphics.Vulkan.Pipeline.BindPoint (VkPipelineBindPoint, unVkPipelineBindPoint)

vkCmdBindDescriptorSets ::
  MonadIO m =>
  Vk.VkCommandBuffer ->
  VkPipelineBindPoint ->
  Vk.VkPipelineLayout ->
  Word32 ->
  [Vk.VkDescriptorSet] ->
  [Word32] ->
  m ()
vkCmdBindDescriptorSets cmdBuf bp layout first ds offs = do
  liftIO . Foreign.withArray ds $ \dPtr ->
    Foreign.withArray offs $ \oPtr ->
    Vk.vkCmdBindDescriptorSets
      cmdBuf
      (unVkPipelineBindPoint bp)
      layout
      first
      (fromIntegral $ length ds)
      dPtr
      (fromIntegral $ length offs)
      oPtr
