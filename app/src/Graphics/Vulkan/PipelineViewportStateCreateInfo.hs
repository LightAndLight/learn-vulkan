{-# language DataKinds, TypeApplications #-}
{-# language EmptyCase, EmptyDataDeriving #-}
module Graphics.Vulkan.PipelineViewportStateCreateInfo where

import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Foreign
import qualified Graphics.Vulkan.Core_1_0 as Vk
import qualified Graphics.Vulkan.Marshal as Vk

import Graphics.Vulkan.Rect (VkRect2D, vkRect2D, unVkRect2D)
import Graphics.Vulkan.Viewport (VkViewport, vkViewport, unVkViewport)

data VkPipelineViewportStateCreateFlag
  deriving (Eq, Ord, Show)

vkPipelineViewportStateCreateBits ::
  Vk.VkPipelineViewportStateCreateFlags ->
  [VkPipelineViewportStateCreateFlag]
vkPipelineViewportStateCreateBits _ = []

unVkPipelineViewportStateCreateBits ::
  [VkPipelineViewportStateCreateFlag] ->
  Vk.VkPipelineViewportStateCreateFlags
unVkPipelineViewportStateCreateBits [] = 0
unVkPipelineViewportStateCreateBits (x:_) = case x of

data VkPipelineViewportStateCreateInfo
  = VkPipelineViewportStateCreateInfo
  { flags :: [VkPipelineViewportStateCreateFlag]
  , pViewports :: [VkViewport]
  , pScissors :: [VkRect2D]
  } deriving (Eq, Show, Ord)

vkPipelineViewportStateCreateInfo ::
  MonadIO m =>
  Vk.VkPipelineViewportStateCreateInfo ->
  m VkPipelineViewportStateCreateInfo
vkPipelineViewportStateCreateInfo a =
  liftIO $
  (\vs ss ->
   VkPipelineViewportStateCreateInfo
   { flags = vkPipelineViewportStateCreateBits $ Vk.getField @"flags" a
   , pViewports = vkViewport <$> vs
   , pScissors = vkRect2D <$> ss
   }) <$>
  Foreign.peekArray
    (fromIntegral $ Vk.getField @"viewportCount" a) (Vk.getField @"pViewports" a) <*>
  Foreign.peekArray
    (fromIntegral $ Vk.getField @"scissorCount" a) (Vk.getField @"pScissors" a)

unVkPipelineViewportStateCreateInfo ::
  MonadIO m =>
  VkPipelineViewportStateCreateInfo ->
  m Vk.VkPipelineViewportStateCreateInfo
unVkPipelineViewportStateCreateInfo a =
  liftIO $ do
    vs <- traverse unVkViewport (pViewports a)
    ss <- traverse unVkRect2D (pScissors a)
    Foreign.withArray vs $ \vPtr ->
      Foreign.withArray ss $ \sPtr ->
      Vk.newVkData $ \ptr -> do
        Vk.writeField @"sType" ptr Vk.VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO
        Vk.writeField @"flags" ptr (unVkPipelineViewportStateCreateBits $ flags a)
        Vk.writeField @"viewportCount" ptr (fromIntegral $ length $ pViewports a)
        Vk.writeField @"pViewports" ptr vPtr
        Vk.writeField @"scissorCount" ptr (fromIntegral $ length $ pScissors a)
        Vk.writeField @"pScissors" ptr sPtr
