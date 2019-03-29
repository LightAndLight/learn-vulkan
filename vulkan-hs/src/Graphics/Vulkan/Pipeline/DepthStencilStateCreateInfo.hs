{-# language EmptyCase, EmptyDataDeriving #-}
{-# language DataKinds, TypeApplications #-}
module Graphics.Vulkan.Pipeline.DepthStencilStateCreateInfo where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Word (Word32)

import qualified Graphics.Vulkan.Core_1_0 as Vk
import qualified Graphics.Vulkan.Marshal as Vk

import Graphics.Vulkan.CompareOp (VkCompareOp, unVkCompareOp)
import Graphics.Vulkan.Utils (unVkBool32)

data VkPipelineDepthStencilStateCreateFlag
  deriving (Eq, Show, Ord)

unVkPipelineDepthStencilStateCreateBits ::
  [VkPipelineDepthStencilStateCreateFlag] ->
  Vk.VkPipelineDepthStencilStateCreateFlags
unVkPipelineDepthStencilStateCreateBits [] = 0
unVkPipelineDepthStencilStateCreateBits (x:_) = case x of

data VkStencilOp
  = Keep
  | Zero
  | Replace
  | IncrementAndClamp
  | DecrementAndClamp
  | Invert
  | IncrementAndWrap
  | DecrementAndWrap
  deriving (Eq, Ord, Show)

unVkStencilOp :: VkStencilOp -> Vk.VkStencilOp
unVkStencilOp a =
  case a of
    Keep -> Vk.VK_STENCIL_OP_KEEP
    Zero -> Vk.VK_STENCIL_OP_ZERO
    Replace -> Vk.VK_STENCIL_OP_REPLACE
    IncrementAndClamp -> Vk.VK_STENCIL_OP_INCREMENT_AND_CLAMP
    DecrementAndClamp -> Vk.VK_STENCIL_OP_DECREMENT_AND_CLAMP
    Invert -> Vk.VK_STENCIL_OP_INVERT
    IncrementAndWrap -> Vk.VK_STENCIL_OP_INCREMENT_AND_WRAP
    DecrementAndWrap -> Vk.VK_STENCIL_OP_DECREMENT_AND_WRAP

data VkStencilOpState
  = VkStencilOpState
  { failOp :: VkStencilOp
  , passOp :: VkStencilOp
  , depthFailOp :: VkStencilOp
  , compareOp :: VkCompareOp
  , compareMask :: Word32
  , writeMask :: Word32
  , reference :: Word32
  } deriving (Eq, Ord, Show)

unVkStencilOpState :: MonadIO m => VkStencilOpState -> m Vk.VkStencilOpState
unVkStencilOpState a =
  liftIO . Vk.newVkData $ \ptr -> do
    Vk.writeField @"failOp" ptr (unVkStencilOp $ failOp a)
    Vk.writeField @"passOp" ptr (unVkStencilOp $ passOp a)
    Vk.writeField @"depthFailOp" ptr (unVkStencilOp $ depthFailOp a)
    Vk.writeField @"compareOp" ptr (unVkCompareOp $ compareOp a)
    Vk.writeField @"compareMask" ptr (compareMask a)
    Vk.writeField @"writeMask" ptr (writeMask a)
    Vk.writeField @"reference" ptr (reference a)

data VkPipelineDepthStencilStateCreateInfo
  = VkPipelineDepthStencilStateCreateInfo
  { flags :: [VkPipelineDepthStencilStateCreateFlag]
  , depthTestEnable :: Bool
  , depthWriteEnable :: Bool
  , depthCompareOp :: VkCompareOp
  , depthBoundsTestEnable :: Bool
  , stencilTestEnable :: Bool
  , front :: VkStencilOpState
  , back :: VkStencilOpState
  , minDepthBounds :: Float
  , maxDepthBounds :: Float
  } deriving (Eq, Ord, Show)

unVkPipelineDepthStencilStateCreateInfo ::
  MonadIO m =>
  VkPipelineDepthStencilStateCreateInfo ->
  m Vk.VkPipelineDepthStencilStateCreateInfo
unVkPipelineDepthStencilStateCreateInfo a =
  liftIO . Vk.newVkData $ \ptr -> do
    Vk.writeField @"sType" ptr Vk.VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO
    Vk.writeField @"flags" ptr (unVkPipelineDepthStencilStateCreateBits $ flags a)
    Vk.writeField @"depthTestEnable" ptr (unVkBool32 $ depthTestEnable a)
    Vk.writeField @"depthWriteEnable" ptr (unVkBool32 $ depthWriteEnable a)
    Vk.writeField @"depthCompareOp" ptr (unVkCompareOp $ depthCompareOp a)
    Vk.writeField @"depthBoundsTestEnable" ptr (unVkBool32 $ depthBoundsTestEnable a)
    Vk.writeField @"stencilTestEnable" ptr (unVkBool32 $ stencilTestEnable a)
    Vk.writeField @"front" ptr =<< unVkStencilOpState (front a)
    Vk.writeField @"back" ptr =<< unVkStencilOpState (back a)
    Vk.writeField @"minDepthBounds" ptr (minDepthBounds a)
    Vk.writeField @"maxDepthBounds" ptr (maxDepthBounds a)
