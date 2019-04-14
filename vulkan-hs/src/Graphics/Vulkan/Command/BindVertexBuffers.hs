module Graphics.Vulkan.Command.BindVertexBuffers where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Word (Word32)

import qualified Foreign
import qualified Graphics.Vulkan.Core_1_0 as Vk

vkCmdBindVertexBuffers ::
  MonadIO m =>
  Vk.VkCommandBuffer ->
  Word32 ->
  [(Vk.VkBuffer, Vk.VkDeviceSize)] ->
  m ()
vkCmdBindVertexBuffers cmdBuf first xs =
  liftIO . Foreign.withArray bufs $ \pBufs ->
  Foreign.withArray offs $ \pOffs ->
  Vk.vkCmdBindVertexBuffers cmdBuf first (fromIntegral $ length xs) pBufs pOffs
  where
    (bufs, offs) = unzip xs
