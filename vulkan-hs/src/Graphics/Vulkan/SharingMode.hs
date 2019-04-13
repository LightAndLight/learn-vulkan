module Graphics.Vulkan.SharingMode where

import qualified Graphics.Vulkan.Core_1_0 as Vk

data VkSharingMode
  = Exclusive
  | Concurrent
  deriving (Eq, Ord, Show)

vkSharingMode :: Vk.VkSharingMode -> VkSharingMode
vkSharingMode a =
  case a of
    Vk.VK_SHARING_MODE_EXCLUSIVE -> Exclusive
    Vk.VK_SHARING_MODE_CONCURRENT -> Concurrent

unVkSharingMode :: VkSharingMode -> Vk.VkSharingMode
unVkSharingMode a =
  case a of
    Exclusive -> Vk.VK_SHARING_MODE_EXCLUSIVE
    Concurrent -> Vk.VK_SHARING_MODE_CONCURRENT
