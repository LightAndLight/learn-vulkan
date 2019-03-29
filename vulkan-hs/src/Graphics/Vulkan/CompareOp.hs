module Graphics.Vulkan.CompareOp where

import qualified Graphics.Vulkan.Core_1_0 as Vk

data VkCompareOp
  = Never
  | Less
  | Equal
  | LessOrEqual
  | Greater
  | NotEqual
  | GreaterOrEqual
  | Always
  deriving (Eq, Ord, Show)

unVkCompareOp :: VkCompareOp -> Vk.VkCompareOp
unVkCompareOp a =
  case a of
    Never -> Vk.VK_COMPARE_OP_NEVER
    Less -> Vk.VK_COMPARE_OP_LESS
    Equal -> Vk.VK_COMPARE_OP_EQUAL
    LessOrEqual -> Vk.VK_COMPARE_OP_LESS_OR_EQUAL
    Greater -> Vk.VK_COMPARE_OP_GREATER
    NotEqual -> Vk.VK_COMPARE_OP_NOT_EQUAL
    GreaterOrEqual -> Vk.VK_COMPARE_OP_GREATER_OR_EQUAL
    Always -> Vk.VK_COMPARE_OP_ALWAYS
