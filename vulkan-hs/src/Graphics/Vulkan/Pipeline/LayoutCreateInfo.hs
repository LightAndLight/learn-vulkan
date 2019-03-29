{-# language DataKinds, TypeApplications #-}
{-# language EmptyCase, EmptyDataDeriving #-}
module Graphics.Vulkan.Pipeline.LayoutCreateInfo where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Word (Word32)

import qualified Foreign
import qualified Graphics.Vulkan.Core_1_0 as Vk
import qualified Graphics.Vulkan.Marshal as Vk

import Graphics.Vulkan.ShaderStage (VkShaderStageFlag, unVkShaderStageBits)

data VkPipelineLayoutCreateFlag
  deriving (Eq, Ord, Show)

unVkPipelineLayoutCreateBits ::
  [VkPipelineLayoutCreateFlag] ->
  Vk.VkPipelineLayoutCreateFlags
unVkPipelineLayoutCreateBits [] = 0
unVkPipelineLayoutCreateBits (x:_) = case x of

data VkPushConstantRange
  = VkPushConstantRange
  { stageFlags :: [VkShaderStageFlag]
  , offset :: Word32
  , size :: Word32
  } deriving (Eq, Ord, Show)

unVkPushConstantRange ::
  MonadIO m =>
  VkPushConstantRange ->
  m Vk.VkPushConstantRange
unVkPushConstantRange a =
  liftIO . Vk.newVkData $ \ptr -> do
    Vk.writeField @"stageFlags" ptr (unVkShaderStageBits $ stageFlags a)
    Vk.writeField @"offset" ptr (offset a)
    Vk.writeField @"size" ptr (size a)

data VkPipelineLayoutCreateInfo
  = VkPipelineLayoutCreateInfo
  { flags :: [VkPipelineLayoutCreateFlag]
  , pSetLayouts :: [Vk.VkDescriptorSetLayout]
  , pPushConstantRanges :: [VkPushConstantRange]
  } deriving (Eq, Ord, Show)

unVkPipelineLayoutCreateInfo ::
  MonadIO m =>
  VkPipelineLayoutCreateInfo ->
  m Vk.VkPipelineLayoutCreateInfo
unVkPipelineLayoutCreateInfo a = do
  pcs <- traverse unVkPushConstantRange (pPushConstantRanges a)
  liftIO . Foreign.withArray (pSetLayouts a) $ \slPtr ->
    Foreign.withArray pcs $ \pcPtr ->
    Vk.newVkData $ \ptr -> do
      Vk.writeField @"sType" ptr Vk.VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO
      Vk.writeField @"flags" ptr (unVkPipelineLayoutCreateBits $ flags a)
      Vk.writeField @"setLayoutCount" ptr (fromIntegral . length $ pSetLayouts a)
      Vk.writeField @"pSetLayouts" ptr slPtr
      Vk.writeField @"pushConstantRangeCount" ptr (fromIntegral . length $ pPushConstantRanges a)
      Vk.writeField @"pPushConstantRanges" ptr pcPtr
