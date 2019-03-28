{-# language DataKinds, TypeApplications #-}
module Graphics.Vulkan.Pipeline.ColorBlendAttachmentState where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits ((.|.))

import qualified Graphics.Vulkan.Core_1_0 as Vk
import qualified Graphics.Vulkan.Ext.VK_EXT_blend_operation_advanced as Vk
import qualified Graphics.Vulkan.Marshal as Vk

import Graphics.Vulkan.Utils (unVkBool32)

data VkColorComponentFlag
  = R
  | G
  | B
  | A
  deriving (Eq, Ord, Show)

unVkColorComponentFlagBit :: VkColorComponentFlag -> Vk.VkColorComponentBitmask a
unVkColorComponentFlagBit a =
  case a of
    R -> Vk.VK_COLOR_COMPONENT_R_BIT
    G -> Vk.VK_COLOR_COMPONENT_G_BIT
    B -> Vk.VK_COLOR_COMPONENT_B_BIT
    A -> Vk.VK_COLOR_COMPONENT_A_BIT

unVkColorComponentFlagBits :: [VkColorComponentFlag] -> Vk.VkColorComponentFlags
unVkColorComponentFlagBits = foldr (\a b -> unVkColorComponentFlagBit a .|. b) 0

data VkBlendFactor
  = Zero
  | One
  | SrcColor
  | OneMinusSrcColor
  | DstColor
  | OneMinusDstColor
  | SrcAlpha
  | OneMinusSrcAlpha
  | DstAlpha
  | OneMinusDstAlpha
  | ConstantColor
  | OneMinusConstantColor
  | ConstantAlpha
  | OneMinusConstantAlpha
  | SrcAlphaSaturate
  | Src1Color
  | OneMinusSrc1Color
  | Src1Alpha
  | OneMinusSrc1Alpha
  deriving (Eq, Ord, Show)

unVkBlendFactor :: VkBlendFactor -> Vk.VkBlendFactor
unVkBlendFactor a =
  case a of
    Zero -> Vk.VK_BLEND_FACTOR_ZERO
    One -> Vk.VK_BLEND_FACTOR_ONE
    SrcColor -> Vk.VK_BLEND_FACTOR_SRC_COLOR
    OneMinusSrcColor -> Vk.VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR
    DstColor -> Vk.VK_BLEND_FACTOR_DST_COLOR
    OneMinusDstColor -> Vk.VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR
    SrcAlpha -> Vk.VK_BLEND_FACTOR_SRC_ALPHA
    OneMinusSrcAlpha -> Vk.VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA
    DstAlpha -> Vk.VK_BLEND_FACTOR_DST_ALPHA
    OneMinusDstAlpha -> Vk.VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA
    ConstantColor -> Vk.VK_BLEND_FACTOR_CONSTANT_COLOR
    OneMinusConstantColor -> Vk.VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR
    ConstantAlpha -> Vk.VK_BLEND_FACTOR_CONSTANT_ALPHA
    OneMinusConstantAlpha -> Vk.VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA
    SrcAlphaSaturate -> Vk.VK_BLEND_FACTOR_SRC_ALPHA_SATURATE
    Src1Color -> Vk.VK_BLEND_FACTOR_SRC1_COLOR
    OneMinusSrc1Color -> Vk.VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR
    Src1Alpha -> Vk.VK_BLEND_FACTOR_SRC1_ALPHA
    OneMinusSrc1Alpha -> Vk.VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA

data VkBlendOp
  = Add
  | Subtract
  | ReverseSubtract
  | Min
  | Max
  | ZeroEXT
  | SrcEXT
  | DstEXT
  | SrcOverEXT
  | DstOverEXT
  | SrcInEXT
  | DstInEXT
  | SrcOutEXT
  | DstOutEXT
  | SrcAtopEXT
  | DstAtopEXT
  | XorEXT
  | MultiplyEXT
  | ScreenEXT
  | OverlayEXT
  | DarkenEXT
  | LightenEXT
  | ColordodgeEXT
  | ColorburnEXT
  | HardlightEXT
  | SoftlightEXT
  | DifferenceEXT
  | ExclusionEXT
  | InvertEXT
  | InvertRgbEXT
  | LineardodgeEXT
  | LinearburnEXT
  | VividlightEXT
  | LinearlightEXT
  | PinlightEXT
  | HardmixEXT
  | HslHueEXT
  | HslSaturationEXT
  | HslColorEXT
  | HslLuminosityEXT
  | PlusEXT
  | PlusClampedEXT
  | PlusClampedAlphaEXT
  | PlusDarkerEXT
  | MinusEXT
  | MinusClampedEXT
  | ContrastEXT
  | InvertOvgEXT
  | RedEXT
  | GreenEXT
  | BlueEXT
  deriving (Eq, Ord, Show)

unVkBlendOp :: VkBlendOp -> Vk.VkBlendOp
unVkBlendOp a =
  case a of
    Add -> Vk.VK_BLEND_OP_ADD
    Subtract -> Vk.VK_BLEND_OP_SUBTRACT
    ReverseSubtract -> Vk.VK_BLEND_OP_REVERSE_SUBTRACT
    Min -> Vk.VK_BLEND_OP_MIN
    Max -> Vk.VK_BLEND_OP_MAX
    ZeroEXT -> Vk.VK_BLEND_OP_ZERO_EXT
    SrcEXT -> Vk.VK_BLEND_OP_SRC_EXT
    DstEXT -> Vk.VK_BLEND_OP_DST_EXT
    SrcOverEXT -> Vk.VK_BLEND_OP_SRC_OVER_EXT
    DstOverEXT -> Vk.VK_BLEND_OP_DST_OVER_EXT
    SrcInEXT -> Vk.VK_BLEND_OP_SRC_IN_EXT
    DstInEXT -> Vk.VK_BLEND_OP_DST_IN_EXT
    SrcOutEXT -> Vk.VK_BLEND_OP_SRC_OUT_EXT
    DstOutEXT -> Vk.VK_BLEND_OP_DST_OUT_EXT
    SrcAtopEXT -> Vk.VK_BLEND_OP_SRC_ATOP_EXT
    DstAtopEXT -> Vk.VK_BLEND_OP_DST_ATOP_EXT
    XorEXT -> Vk.VK_BLEND_OP_XOR_EXT
    MultiplyEXT -> Vk.VK_BLEND_OP_MULTIPLY_EXT
    ScreenEXT -> Vk.VK_BLEND_OP_SCREEN_EXT
    OverlayEXT -> Vk.VK_BLEND_OP_OVERLAY_EXT
    DarkenEXT -> Vk.VK_BLEND_OP_DARKEN_EXT
    LightenEXT -> Vk.VK_BLEND_OP_LIGHTEN_EXT
    ColordodgeEXT -> Vk.VK_BLEND_OP_COLORDODGE_EXT
    ColorburnEXT -> Vk.VK_BLEND_OP_COLORBURN_EXT
    HardlightEXT -> Vk.VK_BLEND_OP_HARDLIGHT_EXT
    SoftlightEXT -> Vk.VK_BLEND_OP_SOFTLIGHT_EXT
    DifferenceEXT -> Vk.VK_BLEND_OP_DIFFERENCE_EXT
    ExclusionEXT -> Vk.VK_BLEND_OP_EXCLUSION_EXT
    InvertEXT -> Vk.VK_BLEND_OP_INVERT_EXT
    InvertRgbEXT -> Vk.VK_BLEND_OP_INVERT_RGB_EXT
    LineardodgeEXT -> Vk.VK_BLEND_OP_LINEARDODGE_EXT
    LinearburnEXT -> Vk.VK_BLEND_OP_LINEARBURN_EXT
    VividlightEXT -> Vk.VK_BLEND_OP_VIVIDLIGHT_EXT
    LinearlightEXT -> Vk.VK_BLEND_OP_LINEARLIGHT_EXT
    PinlightEXT -> Vk.VK_BLEND_OP_PINLIGHT_EXT
    HardmixEXT -> Vk.VK_BLEND_OP_HARDMIX_EXT
    HslHueEXT -> Vk.VK_BLEND_OP_HSL_HUE_EXT
    HslSaturationEXT -> Vk.VK_BLEND_OP_HSL_SATURATION_EXT
    HslColorEXT -> Vk.VK_BLEND_OP_HSL_COLOR_EXT
    HslLuminosityEXT -> Vk.VK_BLEND_OP_HSL_LUMINOSITY_EXT
    PlusEXT -> Vk.VK_BLEND_OP_PLUS_EXT
    PlusClampedEXT -> Vk.VK_BLEND_OP_PLUS_CLAMPED_EXT
    PlusClampedAlphaEXT -> Vk.VK_BLEND_OP_PLUS_CLAMPED_ALPHA_EXT
    PlusDarkerEXT -> Vk.VK_BLEND_OP_PLUS_DARKER_EXT
    MinusEXT -> Vk.VK_BLEND_OP_MINUS_EXT
    MinusClampedEXT -> Vk.VK_BLEND_OP_MINUS_CLAMPED_EXT
    ContrastEXT -> Vk.VK_BLEND_OP_CONTRAST_EXT
    InvertOvgEXT -> Vk.VK_BLEND_OP_INVERT_OVG_EXT
    RedEXT -> Vk.VK_BLEND_OP_RED_EXT
    GreenEXT -> Vk.VK_BLEND_OP_GREEN_EXT
    BlueEXT -> Vk.VK_BLEND_OP_BLUE_EXT

data VkPipelineColorBlendAttachmentState
  = VkPipelineColorBlendAttachmentState
  { blendEnable :: Bool
  , srcColorBlendFactor :: VkBlendFactor
  , dstColorBlendFactor :: VkBlendFactor
  , colorBlendOp :: VkBlendOp
  , srcAlphaBlendFactor :: VkBlendFactor
  , dstAlphaBlendFactor :: VkBlendFactor
  , alphaBlendOp :: VkBlendOp
  , colorWriteMask :: [VkColorComponentFlag]
  } deriving (Eq, Ord, Show)

unVkPipelineColorBlendAttachmentState ::
  MonadIO m =>
  VkPipelineColorBlendAttachmentState ->
  m Vk.VkPipelineColorBlendAttachmentState
unVkPipelineColorBlendAttachmentState a =
  liftIO . Vk.newVkData $ \ptr -> do
    Vk.writeField @"blendEnable" ptr (unVkBool32 $ blendEnable a)
    Vk.writeField @"srcColorBlendFactor" ptr (unVkBlendFactor $ srcColorBlendFactor a)
    Vk.writeField @"dstColorBlendFactor" ptr (unVkBlendFactor $ dstColorBlendFactor a)
    Vk.writeField @"colorBlendOp" ptr (unVkBlendOp $ colorBlendOp a)
    Vk.writeField @"srcAlphaBlendFactor" ptr (unVkBlendFactor $ srcAlphaBlendFactor a)
    Vk.writeField @"dstAlphaBlendFactor" ptr (unVkBlendFactor $ dstAlphaBlendFactor a)
    Vk.writeField @"alphaBlendOp" ptr (unVkBlendOp $ alphaBlendOp a)
    Vk.writeField @"colorWriteMask" ptr (unVkColorComponentFlagBits $ colorWriteMask a)
