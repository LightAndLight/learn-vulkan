module Graphics.Vulkan.Command.Draw where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Int (Int32)
import Data.Word (Word32)

import qualified Graphics.Vulkan.Core_1_0 as Vk

vkCmdDraw ::
  MonadIO m =>
  Vk.VkCommandBuffer ->
  Word32 -> -- ^ Vertex count
  Word32 -> -- ^ Instance count
  Word32 -> -- ^ First vertex
  Word32 -> -- ^ First instance
  m ()
vkCmdDraw buf vc ic fv fi = liftIO $ Vk.vkCmdDraw buf vc ic fv fi

vkCmdDrawIndexed ::
  MonadIO m =>
  Vk.VkCommandBuffer ->
  Word32 -> -- ^ Index count
  Word32 -> -- ^ Instance count
  Word32 -> -- ^ First index
  Int32 -> -- ^ Vertex offset
  Word32 -> -- ^ First instance
  m ()
vkCmdDrawIndexed buf ixc ic fix voff fi =
  liftIO $ Vk.vkCmdDrawIndexed buf ixc ic fix voff fi
