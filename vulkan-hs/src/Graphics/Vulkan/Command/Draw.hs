module Graphics.Vulkan.Command.Draw where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Word (Word32)

import qualified Graphics.Vulkan.Core_1_0 as Vk

vkCmdDraw :: MonadIO m => Vk.VkCommandBuffer -> Word32 -> Word32 -> Word32 -> Word32 -> m ()
vkCmdDraw buf vc ic fv fi = liftIO $ Vk.vkCmdDraw buf vc ic fv fi
