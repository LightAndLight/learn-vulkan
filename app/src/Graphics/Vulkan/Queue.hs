{-# language DataKinds, TypeApplications #-}
{-# language GADTs #-}
{-# language ViewPatterns #-}
{-# language StandaloneDeriving #-}
module Graphics.Vulkan.Queue
  ( Vk.FlagType(..)
  , VkQueueType(..)
  , vkQueueBit, unVkQueueBit
  , vkQueueBits, unVkQueueBits
  , VkExtent3D(..)
  , vkExtent3D
  , VkQueueFamilyProperties(..)
  , vkQueueFamilyProperties
  )
where

import Data.Bits ((.&.), (.|.))
import Data.Word (Word32)
import Unsafe.Coerce (unsafeCoerce)

import qualified Graphics.Vulkan.Core_1_1 as Vk
import qualified Graphics.Vulkan.Marshal as Vk

data VkQueueType a where
  Graphics :: VkQueueType a
  Compute :: VkQueueType a
  Transfer :: VkQueueType a
  SparseBinding :: VkQueueType a
  Protected :: VkQueueType 'Vk.FlagBit
deriving instance Eq (VkQueueType a)
deriving instance Ord (VkQueueType a)
deriving instance Show (VkQueueType a)

vkQueueBit ::
  Vk.VkQueueBitmask a ->
  VkQueueType a
vkQueueBit a =
  case a of
    Vk.VK_QUEUE_GRAPHICS_BIT -> Graphics
    Vk.VK_QUEUE_COMPUTE_BIT -> Compute
    Vk.VK_QUEUE_TRANSFER_BIT -> Transfer
    Vk.VK_QUEUE_SPARSE_BINDING_BIT -> SparseBinding
    (unsafeCoerce -> Vk.VK_QUEUE_PROTECTED_BIT) -> unsafeCoerce Protected

unVkQueueBit ::
  VkQueueType a ->
  Vk.VkQueueBitmask a
unVkQueueBit a =
  case a of
    Graphics -> Vk.VK_QUEUE_GRAPHICS_BIT
    Compute -> Vk.VK_QUEUE_COMPUTE_BIT
    Transfer -> Vk.VK_QUEUE_TRANSFER_BIT
    SparseBinding -> Vk.VK_QUEUE_SPARSE_BINDING_BIT
    Protected -> Vk.VK_QUEUE_PROTECTED_BIT

vkQueueBits ::
  Vk.VkQueueFlags ->
  [VkQueueType 'Vk.FlagMask]
vkQueueBits a =
  foldr
  (\(mask, val) rest -> if mask .&. a == mask then val : rest else rest)
  []
  [ (Vk.VK_QUEUE_GRAPHICS_BIT, Graphics)
  , (Vk.VK_QUEUE_COMPUTE_BIT, Compute)
  , (Vk.VK_QUEUE_TRANSFER_BIT, Transfer)
  , (Vk.VK_QUEUE_SPARSE_BINDING_BIT, SparseBinding)
  ]

unVkQueueBits ::
  [VkQueueType 'Vk.FlagMask] ->
  Vk.VkQueueFlags
unVkQueueBits = foldr (\a b -> unVkQueueBit a .|. b) 0

data VkExtent3D
  = VkExtent3D
  { width :: Word32
  , height :: Word32
  , depth :: Word32
  } deriving (Eq, Ord, Show)

vkExtent3D :: Vk.VkExtent3D -> VkExtent3D
vkExtent3D p =
  VkExtent3D
  { width = Vk.getField @"width" p
  , height = Vk.getField @"height" p
  , depth = Vk.getField @"depth" p
  }

data VkQueueFamilyProperties
  = VkQueueFamilyProperties
  { queueFlags :: [VkQueueType 'Vk.FlagMask]
  , queueCount :: Word32
  , timestampValidBits :: Word32
  , minImageTransferGranularity :: VkExtent3D
  } deriving (Eq, Ord, Show)

vkQueueFamilyProperties ::
  Vk.VkQueueFamilyProperties ->
  VkQueueFamilyProperties
vkQueueFamilyProperties p =
  VkQueueFamilyProperties
  { queueFlags = vkQueueBits $ Vk.getField @"queueFlags" p
  , queueCount = Vk.getField @"queueCount" p
  , timestampValidBits = Vk.getField @"timestampValidBits" p
  , minImageTransferGranularity =
      vkExtent3D $ Vk.getField @"minImageTransferGranularity" p
  }
