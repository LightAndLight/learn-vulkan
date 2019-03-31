{-# language DataKinds, TypeApplications #-}
{-# language EmptyCase, EmptyDataDeriving #-}
module Graphics.Vulkan.Pipeline.DynamicStateCreateInfo where

import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Foreign
import qualified Graphics.Vulkan.Core_1_0 as Vk
import qualified Graphics.Vulkan.Ext.VK_EXT_discard_rectangles as Vk
import qualified Graphics.Vulkan.Ext.VK_EXT_sample_locations as Vk
import qualified Graphics.Vulkan.Ext.VK_NV_clip_space_w_scaling as Vk
-- import qualified Graphics.Vulkan.Ext.VK_NV_scissor_exclusive as Vk
-- import qualified Graphics.Vulkan.Ext.VK_NV_shading_rate_image as Vk
import qualified Graphics.Vulkan.Marshal as Vk

data VkPipelineDynamicStateCreateFlag
  deriving (Eq, Ord, Show)

unVkPipelineDynamicStateCreateBits ::
  [VkPipelineDynamicStateCreateFlag] ->
  Vk.VkPipelineDynamicStateCreateFlags
unVkPipelineDynamicStateCreateBits [] = 0
unVkPipelineDynamicStateCreateBits (x:_) = case x of

data VkDynamicState
  = Viewport
  | Scissor
  | LineWidth
  | DepthBias
  | BlendConstants
  | DepthBounds
  | StencilCompareMask
  | StencilWriteMask
  | StencilReference
  | ViewportWScalingNV
  | DiscardRectangleEXT
  | SampleLocationsEXT
  -- ViewportShadingRatePaletteNV
  -- ViewportCoarseSampleOrderNV
  -- ExclusiveScissorNV
  deriving (Eq, Ord, Show)

unVkDynamicState :: VkDynamicState -> Vk.VkDynamicState
unVkDynamicState a =
  case a of
    Viewport -> Vk.VK_DYNAMIC_STATE_VIEWPORT
    Scissor -> Vk.VK_DYNAMIC_STATE_SCISSOR
    LineWidth -> Vk.VK_DYNAMIC_STATE_LINE_WIDTH
    DepthBias -> Vk.VK_DYNAMIC_STATE_DEPTH_BIAS
    BlendConstants -> Vk.VK_DYNAMIC_STATE_BLEND_CONSTANTS
    DepthBounds -> Vk.VK_DYNAMIC_STATE_DEPTH_BOUNDS
    StencilCompareMask -> Vk.VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK
    StencilWriteMask -> Vk.VK_DYNAMIC_STATE_STENCIL_WRITE_MASK
    StencilReference -> Vk.VK_DYNAMIC_STATE_STENCIL_REFERENCE
    ViewportWScalingNV -> Vk.VK_DYNAMIC_STATE_VIEWPORT_W_SCALING_NV
    DiscardRectangleEXT -> Vk.VK_DYNAMIC_STATE_DISCARD_RECTANGLE_EXT
    SampleLocationsEXT -> Vk.VK_DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT
    -- ViewportShadingRatePaletteNV -> Vk.VK_DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV
    -- ViewportCoarseSampleOrderNV -> Vk.VK_DYNAMIC_STATE_VIEWPORT_COARSE_SAMPLE_ORDER_NV
    -- ExclusiveScissorNV -> Vk.VK_DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV

data VkPipelineDynamicStateCreateInfo
  = VkPipelineDynamicStateCreateInfo
  { flags :: [VkPipelineDynamicStateCreateFlag]
  , pDynamicStates :: [VkDynamicState]
  } deriving (Eq, Ord, Show)

unVkPipelineDynamicStateCreateInfo ::
  MonadIO m =>
  VkPipelineDynamicStateCreateInfo ->
  m Vk.VkPipelineDynamicStateCreateInfo
unVkPipelineDynamicStateCreateInfo a =
  liftIO . Foreign.withArray (unVkDynamicState <$> pDynamicStates a) $ \aPtr ->
  Vk.newVkData $ \ptr -> do
    Vk.writeField @"sType" ptr Vk.VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO
    Vk.writeField @"pNext" ptr Vk.VK_NULL
    Vk.writeField @"flags" ptr (unVkPipelineDynamicStateCreateBits $ flags a)
    Vk.writeField @"dynamicStateCount" ptr (fromIntegral . length $ pDynamicStates a)
    Vk.writeField @"pDynamicStates" ptr aPtr
