{-# language EmptyCase, EmptyDataDeriving #-}
{-# language DataKinds, TypeApplications #-}
module Graphics.Vulkan.Pipeline.TessellationStateCreateInfo where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Word (Word32)

import qualified Graphics.Vulkan.Core_1_0 as Vk
import qualified Graphics.Vulkan.Marshal as Vk

data VkPipelineTessellationStateCreateFlag
  deriving (Eq, Ord, Show)

unVkPipelineTessellationStateCreateBits ::
  [VkPipelineTessellationStateCreateFlag] ->
  Vk.VkPipelineTessellationStateCreateFlags
unVkPipelineTessellationStateCreateBits [] = 0
unVkPipelineTessellationStateCreateBits (x:_) = case x of

data VkPipelineTessellationStateCreateInfo
  = VkPipelineTessellationStateCreateInfo
  { flags :: [VkPipelineTessellationStateCreateFlag]
  , patchControlPoints :: Word32
  } deriving (Eq, Ord, Show)

unVkPipelineTessellationStateCreateInfo ::
  MonadIO m =>
  VkPipelineTessellationStateCreateInfo ->
  m Vk.VkPipelineTessellationStateCreateInfo
unVkPipelineTessellationStateCreateInfo a =
  liftIO . Vk.newVkData $ \ptr -> do
    Vk.writeField @"sType" ptr Vk.VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO
    Vk.writeField @"pNext" ptr Vk.VK_NULL
    Vk.writeField @"flags" ptr (unVkPipelineTessellationStateCreateBits $ flags a)
    Vk.writeField @"patchControlPoints" ptr (patchControlPoints a)
