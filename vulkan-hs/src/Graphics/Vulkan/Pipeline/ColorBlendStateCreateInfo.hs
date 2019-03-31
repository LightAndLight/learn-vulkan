{-# language DataKinds, TypeApplications #-}
{-# language EmptyCase, EmptyDataDeriving #-}
module Graphics.Vulkan.Pipeline.ColorBlendStateCreateInfo where

import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Foreign
import qualified Graphics.Vulkan.Core_1_0 as Vk
import qualified Graphics.Vulkan.Marshal as Vk

import Graphics.Vulkan.Pipeline.ColorBlendAttachmentState
  (VkPipelineColorBlendAttachmentState, unVkPipelineColorBlendAttachmentState)
import Graphics.Vulkan.Utils (unVkBool32)

data VkPipelineColorBlendStateCreateFlag
  deriving (Eq, Ord, Show)

unVkPipelineColorBlendStateCreateBits ::
  [VkPipelineColorBlendStateCreateFlag] ->
  Vk.VkPipelineColorBlendStateCreateFlags
unVkPipelineColorBlendStateCreateBits [] = 0
unVkPipelineColorBlendStateCreateBits (x:_) = case x of

data VkLogicOp
  = Clear
  | And
  | AndReverse
  | Copy
  | AndInverted
  | NoOp
  | Xor
  | Or
  | Nor
  | Equivalent
  | Invert
  | OrReverse
  | CopyInverted
  | OrInverted
  | Nand
  | Set
  deriving (Eq, Ord, Show)

unVkLogicOp :: VkLogicOp -> Vk.VkLogicOp
unVkLogicOp a =
  case a of
    Clear -> Vk.VK_LOGIC_OP_CLEAR
    And -> Vk.VK_LOGIC_OP_AND
    AndReverse -> Vk.VK_LOGIC_OP_AND_REVERSE
    Copy -> Vk.VK_LOGIC_OP_COPY
    AndInverted -> Vk.VK_LOGIC_OP_AND_INVERTED
    NoOp -> Vk.VK_LOGIC_OP_NO_OP
    Xor -> Vk.VK_LOGIC_OP_XOR
    Or -> Vk.VK_LOGIC_OP_OR
    Nor -> Vk.VK_LOGIC_OP_NOR
    Equivalent -> Vk.VK_LOGIC_OP_EQUIVALENT
    Invert -> Vk.VK_LOGIC_OP_INVERT
    OrReverse -> Vk.VK_LOGIC_OP_OR_REVERSE
    CopyInverted -> Vk.VK_LOGIC_OP_COPY_INVERTED
    OrInverted -> Vk.VK_LOGIC_OP_OR_INVERTED
    Nand -> Vk.VK_LOGIC_OP_NAND
    Set -> Vk.VK_LOGIC_OP_SET

data VkPipelineColorBlendStateCreateInfo
  = VkPipelineColorBlendStateCreateInfo
  { flags :: [VkPipelineColorBlendStateCreateFlag]
  , logicOpEnable :: Bool
  , logicOp :: VkLogicOp
  , pAttachments :: [VkPipelineColorBlendAttachmentState]
  , blendConstants :: (Float, Float, Float, Float)
  } deriving (Eq, Ord, Show)

unVkPipelineColorBlendStateCreateInfo ::
  MonadIO m =>
  VkPipelineColorBlendStateCreateInfo ->
  m Vk.VkPipelineColorBlendStateCreateInfo
unVkPipelineColorBlendStateCreateInfo a = do
  as <- traverse unVkPipelineColorBlendAttachmentState (pAttachments a)
  liftIO . Foreign.withArray as $ \aPtr ->
    Vk.newVkData $ \ptr -> do
      Vk.writeField @"sType" ptr Vk.VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO
      Vk.writeField @"pNext" ptr Vk.VK_NULL
      Vk.writeField @"flags" ptr (unVkPipelineColorBlendStateCreateBits $ flags a)
      Vk.writeField @"logicOpEnable" ptr (unVkBool32 $ logicOpEnable a)
      Vk.writeField @"logicOp" ptr (unVkLogicOp $ logicOp a)
      Vk.writeField @"attachmentCount" ptr (fromIntegral . length $ pAttachments a)
      Vk.writeField @"pAttachments" ptr aPtr
      let (c1, c2, c3, c4) = blendConstants a
      Vk.writeFieldArray @"blendConstants" @0 ptr c1
      Vk.writeFieldArray @"blendConstants" @1 ptr c2
      Vk.writeFieldArray @"blendConstants" @2 ptr c3
      Vk.writeFieldArray @"blendConstants" @3 ptr c4
