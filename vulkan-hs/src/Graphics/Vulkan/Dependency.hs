module Graphics.Vulkan.Dependency where

import Data.Bits ((.|.))
import Unsafe.Coerce (unsafeCoerce)

import qualified Graphics.Vulkan.Core_1_0 as Vk
import qualified Graphics.Vulkan.Core_1_1 as Vk
import qualified Graphics.Vulkan.Ext.VK_KHR_device_group as Vk
import qualified Graphics.Vulkan.Ext.VK_KHR_multiview as Vk

data VkDependencyFlag
  = ByRegion
  | DeviceGroup
  | ViewLocal
  | ViewLocalKHR
  | DeviceGroupKHR
  deriving (Eq, Ord, Show)

unVkDependencyBit :: VkDependencyFlag -> Vk.VkDependencyBitmask a
unVkDependencyBit a =
  case a of
    ByRegion -> Vk.VK_DEPENDENCY_BY_REGION_BIT
    DeviceGroup -> unsafeCoerce Vk.VK_DEPENDENCY_DEVICE_GROUP_BIT
    ViewLocal -> unsafeCoerce Vk.VK_DEPENDENCY_VIEW_LOCAL_BIT
    ViewLocalKHR -> unsafeCoerce Vk.VK_DEPENDENCY_VIEW_LOCAL_BIT_KHR
    DeviceGroupKHR -> unsafeCoerce Vk.VK_DEPENDENCY_DEVICE_GROUP_BIT_KHR

unVkDependencyBits :: [VkDependencyFlag] -> Vk.VkDependencyFlags
unVkDependencyBits = foldr (\a b -> unVkDependencyBit a .|. b) 0
