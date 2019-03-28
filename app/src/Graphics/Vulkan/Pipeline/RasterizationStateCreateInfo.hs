{-# language DataKinds, TypeApplications #-}
{-# language EmptyCase, EmptyDataDeriving #-}
module Graphics.Vulkan.Pipeline.RasterizationStateCreateInfo where

import Data.Bits ((.&.), (.|.))
import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Graphics.Vulkan.Core_1_0 as Vk
import qualified Graphics.Vulkan.Ext.VK_NV_fill_rectangle as Vk
import qualified Graphics.Vulkan.Marshal as Vk

import Graphics.Vulkan.Utils (vkBool32, unVkBool32)

data VkPipelineRasterizationStateCreateFlag
  deriving (Eq, Ord, Show)

vkPipelineRasterizationStateCreateBits ::
  Vk.VkPipelineRasterizationStateCreateFlags ->
  [VkPipelineRasterizationStateCreateFlag]
vkPipelineRasterizationStateCreateBits _ = []

unVkPipelineRasterizationStateCreateBits ::
  [VkPipelineRasterizationStateCreateFlag] ->
  Vk.VkPipelineRasterizationStateCreateFlags
unVkPipelineRasterizationStateCreateBits [] = 0
unVkPipelineRasterizationStateCreateBits (x:_) = case x of

data VkPolygonMode
  = Fill
  | Line
  | Point
  | FillRectangleNV
  deriving (Eq, Ord, Show)

vkPolygonMode :: Vk.VkPolygonMode -> VkPolygonMode
vkPolygonMode a =
  case a of
    Vk.VK_POLYGON_MODE_FILL -> Fill
    Vk.VK_POLYGON_MODE_LINE -> Line
    Vk.VK_POLYGON_MODE_POINT -> Point
    Vk.VK_POLYGON_MODE_FILL_RECTANGLE_NV -> FillRectangleNV

unVkPolygonMode :: VkPolygonMode -> Vk.VkPolygonMode
unVkPolygonMode a =
  case a of
    Fill -> Vk.VK_POLYGON_MODE_FILL
    Line -> Vk.VK_POLYGON_MODE_LINE
    Point -> Vk.VK_POLYGON_MODE_POINT
    FillRectangleNV -> Vk.VK_POLYGON_MODE_FILL_RECTANGLE_NV


data VkCullModeFlag
  = None
  | Front
  | Back
  | FrontAndBack
  deriving (Eq, Ord, Show)

vkCullModeBit ::
  Vk.VkCullModeBitmask a ->
  VkCullModeFlag
vkCullModeBit a =
  case a of
    Vk.VK_CULL_MODE_NONE -> None
    Vk.VK_CULL_MODE_FRONT_BIT -> Front
    Vk.VK_CULL_MODE_BACK_BIT -> Back
    Vk.VK_CULL_MODE_FRONT_AND_BACK -> FrontAndBack

unVkCullModeBit ::
  VkCullModeFlag ->
  Vk.VkCullModeBitmask a
unVkCullModeBit a =
  case a of
    None -> Vk.VK_CULL_MODE_NONE
    Front -> Vk.VK_CULL_MODE_FRONT_BIT
    Back -> Vk.VK_CULL_MODE_BACK_BIT
    FrontAndBack -> Vk.VK_CULL_MODE_FRONT_AND_BACK

vkCullModeBits ::
  Vk.VkCullModeFlags ->
  [VkCullModeFlag]
vkCullModeBits bs =
  foldr
    (\(mask, val) b -> if mask .&. bs == mask then val : b else b)
    []
    [ (Vk.VK_CULL_MODE_NONE, None)
    , (Vk.VK_CULL_MODE_FRONT_BIT, Front)
    , (Vk.VK_CULL_MODE_BACK_BIT, Back)
    , (Vk.VK_CULL_MODE_FRONT_AND_BACK, FrontAndBack)
    ]

unVkCullModeBits ::
  [VkCullModeFlag] ->
  Vk.VkCullModeFlags
unVkCullModeBits = foldr (\a b -> unVkCullModeBit a .|. b) 0

data VkFrontFace
  = CounterClockwise
  | Clockwise
  deriving (Eq, Ord, Show)

vkFrontFace :: Vk.VkFrontFace -> VkFrontFace
vkFrontFace a =
  case a of
    Vk.VK_FRONT_FACE_COUNTER_CLOCKWISE -> CounterClockwise
    Vk.VK_FRONT_FACE_CLOCKWISE -> Clockwise

unVkFrontFace :: VkFrontFace -> Vk.VkFrontFace
unVkFrontFace a =
  case a of
    CounterClockwise -> Vk.VK_FRONT_FACE_COUNTER_CLOCKWISE
    Clockwise -> Vk.VK_FRONT_FACE_CLOCKWISE

data VkPipelineRasterizationStateCreateInfo
  = VkPipelineRasterizationStateCreateInfo
  { flags :: [VkPipelineRasterizationStateCreateFlag]
  , depthClampEnable :: Bool
  , rasterizerDiscardEnable :: Bool
  , polygonMode :: VkPolygonMode
  , cullMode :: [VkCullModeFlag]
  , frontFace :: VkFrontFace
  , depthBiasEnable :: Bool
  , depthBiasConstantFactor :: Float
  , depthBiasClamp :: Float
  , depthBiasSlopeFactor :: Float
  , lineWidth :: Float
  } deriving (Eq, Ord, Show)

vkPipelineRasterizationStateCreateInfo ::
  Vk.VkPipelineRasterizationStateCreateInfo ->
  VkPipelineRasterizationStateCreateInfo
vkPipelineRasterizationStateCreateInfo a =
  VkPipelineRasterizationStateCreateInfo
  { flags = vkPipelineRasterizationStateCreateBits $ Vk.getField @"flags" a
  , depthClampEnable = vkBool32 $ Vk.getField @"depthClampEnable" a
  , rasterizerDiscardEnable = vkBool32 $ Vk.getField @"rasterizerDiscardEnable" a
  , polygonMode = vkPolygonMode $ Vk.getField @"polygonMode" a
  , cullMode = vkCullModeBits $ Vk.getField @"cullMode" a
  , frontFace = vkFrontFace $ Vk.getField @"frontFace" a
  , depthBiasEnable = vkBool32 $ Vk.getField @"depthBiasEnable" a
  , depthBiasConstantFactor = Vk.getField @"depthBiasConstantFactor" a
  , depthBiasClamp = Vk.getField @"depthBiasClamp" a
  , depthBiasSlopeFactor = Vk.getField @"depthBiasSlopeFactor" a
  , lineWidth = Vk.getField @"lineWidth" a
  }

unVkPipelineRasterizationStateCreateInfo ::
  MonadIO m =>
  VkPipelineRasterizationStateCreateInfo ->
  m Vk.VkPipelineRasterizationStateCreateInfo
unVkPipelineRasterizationStateCreateInfo a =
  liftIO . Vk.newVkData $ \ptr -> do
    Vk.writeField @"sType" ptr Vk.VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO
    Vk.writeField @"flags" ptr (unVkPipelineRasterizationStateCreateBits $ flags a)
    Vk.writeField @"depthClampEnable" ptr (unVkBool32 $ depthClampEnable a)
    Vk.writeField @"rasterizerDiscardEnable" ptr (unVkBool32 $ rasterizerDiscardEnable a)
    Vk.writeField @"polygonMode" ptr (unVkPolygonMode $ polygonMode a)
    Vk.writeField @"cullMode" ptr (unVkCullModeBits $ cullMode a)
    Vk.writeField @"frontFace" ptr (unVkFrontFace $ frontFace a)
    Vk.writeField @"depthBiasEnable" ptr (unVkBool32 $ depthBiasEnable a)
    Vk.writeField @"depthBiasConstantFactor" ptr (depthBiasConstantFactor a)
    Vk.writeField @"depthBiasClamp" ptr (depthBiasClamp a)
    Vk.writeField @"depthBiasSlopeFactor" ptr (depthBiasSlopeFactor a)
    Vk.writeField @"lineWidth" ptr (lineWidth a)
