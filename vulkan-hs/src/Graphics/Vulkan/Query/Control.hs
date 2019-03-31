module Graphics.Vulkan.Query.Control where

import Data.Bits ((.|.))

import qualified Graphics.Vulkan.Core_1_0 as Vk

data VkQueryControlFlag
  = Precise
  deriving (Eq, Ord, Show)

unVkQueryControlBit ::
  VkQueryControlFlag ->
  Vk.VkQueryControlBitmask a
unVkQueryControlBit a =
  case a of
    Precise -> Vk.VK_QUERY_CONTROL_PRECISE_BIT

unVkQueryControlBits ::
  [VkQueryControlFlag] ->
  Vk.VkQueryControlFlags
unVkQueryControlBits = foldr (\a b -> unVkQueryControlBit a .|. b) 0
