module Graphics.Vulkan.Utils where

import qualified Graphics.Vulkan.Constants as Vk
import qualified Graphics.Vulkan.Core_1_0 as Vk

vkBool32 :: Vk.VkBool32 -> Bool
vkBool32 = (== Vk.VK_TRUE)

unVkBool32 :: Bool -> Vk.VkBool32
unVkBool32 True = Vk.VK_TRUE
unVkBool32 False = Vk.VK_FALSE
