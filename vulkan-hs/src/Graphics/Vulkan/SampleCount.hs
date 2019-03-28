module Graphics.Vulkan.SampleCount where

import Data.Bits ((.&.), (.|.))
import qualified Graphics.Vulkan.Core_1_0 as Vk

data VkSampleCount
  = SC1
  | SC2
  | SC4
  | SC8
  | SC16
  | SC32
  | SC64
  deriving (Eq, Ord, Show)

vkSampleCountBit ::
  Vk.VkSampleCountBitmask a ->
  VkSampleCount
vkSampleCountBit a =
  case a of
    Vk.VK_SAMPLE_COUNT_1_BIT -> SC1
    Vk.VK_SAMPLE_COUNT_2_BIT -> SC2
    Vk.VK_SAMPLE_COUNT_4_BIT -> SC4
    Vk.VK_SAMPLE_COUNT_8_BIT -> SC8
    Vk.VK_SAMPLE_COUNT_16_BIT -> SC16
    Vk.VK_SAMPLE_COUNT_32_BIT -> SC32
    Vk.VK_SAMPLE_COUNT_64_BIT -> SC64

unVkSampleCountBit ::
  VkSampleCount ->
  Vk.VkSampleCountBitmask a
unVkSampleCountBit a =
  case a of
    SC1 -> Vk.VK_SAMPLE_COUNT_1_BIT
    SC2 -> Vk.VK_SAMPLE_COUNT_2_BIT
    SC4 -> Vk.VK_SAMPLE_COUNT_4_BIT
    SC8 -> Vk.VK_SAMPLE_COUNT_8_BIT
    SC16 -> Vk.VK_SAMPLE_COUNT_16_BIT
    SC32 -> Vk.VK_SAMPLE_COUNT_32_BIT
    SC64 -> Vk.VK_SAMPLE_COUNT_64_BIT

vkSampleCountBits ::
  Vk.VkSampleCountFlags ->
  [VkSampleCount]
vkSampleCountBits a =
  foldr
  (\(mask, val) rest -> if mask .&. a == mask then val : rest else rest)
  []
  [ (Vk.VK_SAMPLE_COUNT_1_BIT, SC1)
  , (Vk.VK_SAMPLE_COUNT_2_BIT, SC2)
  , (Vk.VK_SAMPLE_COUNT_4_BIT, SC4)
  , (Vk.VK_SAMPLE_COUNT_8_BIT, SC8)
  , (Vk.VK_SAMPLE_COUNT_16_BIT, SC16)
  , (Vk.VK_SAMPLE_COUNT_32_BIT, SC32)
  , (Vk.VK_SAMPLE_COUNT_64_BIT, SC64)
  ]

unVkSampleCountBits ::
  [VkSampleCount] ->
  Vk.VkSampleCountFlags
unVkSampleCountBits = foldr (\a b -> unVkSampleCountBit a .|. b) 0
