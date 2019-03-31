{-# language DataKinds, TypeApplications #-}
{-# language EmptyCase, EmptyDataDeriving #-}
module Graphics.Vulkan.Pipeline.MultisampleStateCreateInfo
  ( VkPipelineMultisampleStateCreateFlag(..)
  , vkPipelineMultisampleStateCreateBits
  , unVkPipelineMultisampleStateCreateBits
  , Vk.VkSampleMask(..)
  , VkPipelineMultisampleStateCreateInfo(..)
  , unVkPipelineMultisampleStateCreateInfo
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Foreign
import qualified Graphics.Vulkan.Core_1_0 as Vk
import qualified Graphics.Vulkan.Marshal as Vk

import Graphics.Vulkan.SampleCount (VkSampleCount, unVkSampleCountBit)
import Graphics.Vulkan.Utils (unVkBool32)

data VkPipelineMultisampleStateCreateFlag
  deriving (Eq, Ord, Show)

vkPipelineMultisampleStateCreateBits ::
  Vk.VkPipelineMultisampleStateCreateFlags ->
  [VkPipelineMultisampleStateCreateFlag]
vkPipelineMultisampleStateCreateBits _ = []

unVkPipelineMultisampleStateCreateBits ::
  [VkPipelineMultisampleStateCreateFlag] ->
  Vk.VkPipelineMultisampleStateCreateFlags
unVkPipelineMultisampleStateCreateBits [] = 0
unVkPipelineMultisampleStateCreateBits (x:_) = case x of

data VkPipelineMultisampleStateCreateInfo
  = VkPipelineMultisampleStateCreateInfo
  { flags :: [VkPipelineMultisampleStateCreateFlag]
  , rasterizationSamples :: VkSampleCount
  , sampleShadingEnable :: Bool
  , minSampleShading :: Float
  , pSampleMask :: [Vk.VkSampleMask]
  , alphaToCoverageEnable :: Bool
  , alphaToOneEnable :: Bool
  } deriving (Eq, Ord, Show)

unVkPipelineMultisampleStateCreateInfo ::
  MonadIO m =>
  VkPipelineMultisampleStateCreateInfo ->
  m Vk.VkPipelineMultisampleStateCreateInfo
unVkPipelineMultisampleStateCreateInfo a =
  liftIO $
  Foreign.withArray (pSampleMask a) $ \sPtr ->
  Vk.newVkData $ \ptr -> do
    Vk.writeField @"sType" ptr Vk.VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO
    Vk.writeField @"pNext" ptr Vk.VK_NULL
    Vk.writeField @"flags" ptr (unVkPipelineMultisampleStateCreateBits $ flags a)
    Vk.writeField @"rasterizationSamples" ptr (unVkSampleCountBit $ rasterizationSamples a)
    Vk.writeField @"sampleShadingEnable" ptr (unVkBool32 $ sampleShadingEnable a)
    Vk.writeField @"minSampleShading" ptr (minSampleShading a)
    Vk.writeField @"pSampleMask" ptr sPtr
    Vk.writeField @"alphaToCoverageEnable" ptr (unVkBool32 $ alphaToCoverageEnable a)
    Vk.writeField @"alphaToOneEnable" ptr (unVkBool32 $ alphaToOneEnable a)
