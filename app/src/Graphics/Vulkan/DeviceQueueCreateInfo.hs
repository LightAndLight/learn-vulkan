{-# language DataKinds, TypeApplications #-}
{-# language ViewPatterns #-}
module Graphics.Vulkan.DeviceQueueCreateInfo where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Managed.Safe (MonadManaged, using, managed)
import Data.Bits ((.&.), (.|.))
import Data.Word (Word32)
import Unsafe.Coerce (unsafeCoerce)

import qualified Foreign.Marshal.Array as Foreign
import qualified Graphics.Vulkan.Core_1_1 as Vk
import qualified Graphics.Vulkan.Marshal as Vk

data VkDeviceQueueCreateFlag
  = Protected
  deriving (Eq, Show, Ord)

vkDeviceQueueCreateBit ::
  Vk.VkDeviceQueueCreateBitmask a ->
  VkDeviceQueueCreateFlag
vkDeviceQueueCreateBit a =
  case a of
    -- i think this is a bug; see https://github.com/achirkin/vulkan/issues/29
    (unsafeCoerce -> Vk.VK_DEVICE_QUEUE_CREATE_PROTECTED_BIT) -> Protected

unVkDeviceQueueCreateBit ::
  VkDeviceQueueCreateFlag ->
  Vk.VkDeviceQueueCreateBitmask a
unVkDeviceQueueCreateBit a =
  case a of
    Protected -> unsafeCoerce Vk.VK_DEVICE_QUEUE_CREATE_PROTECTED_BIT

vkDeviceQueueCreateBits ::
  Vk.VkDeviceQueueCreateFlags ->
  [VkDeviceQueueCreateFlag]
vkDeviceQueueCreateBits a =
  foldr
  (\(mask, val) rest -> if mask .&. a == mask then val : rest else rest)
  []
  -- see https://github.com/achirkin/vulkan/issues/29
  [ (unsafeCoerce Vk.VK_DEVICE_QUEUE_CREATE_PROTECTED_BIT, Protected)
  ]

unVkDeviceQueueCreateBits ::
  [VkDeviceQueueCreateFlag] ->
  Vk.VkDeviceQueueCreateFlags
unVkDeviceQueueCreateBits = foldr (\a b -> unVkDeviceQueueCreateBit a .|. b) 0

data VkDeviceQueueCreateInfo
  = VkDeviceQueueCreateInfo
  { flags :: [VkDeviceQueueCreateFlag]
  , queueFamilyIndex :: Word32
  , queueCount :: Word32
  , pQueuePriorities :: [Float]
  } deriving (Eq, Ord, Show)

unVkDeviceQueueCreateInfo ::
  (MonadManaged m, MonadIO m) =>
  VkDeviceQueueCreateInfo ->
  m Vk.VkDeviceQueueCreateInfo
unVkDeviceQueueCreateInfo p = do
  priosPtr <- using $ managed (Foreign.withArray $ pQueuePriorities p)
  liftIO $ Vk.newVkData $ \infoPtr -> do
    Vk.writeField @"sType" infoPtr Vk.VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO
    Vk.writeField @"flags" infoPtr (unVkDeviceQueueCreateBits $ flags p)
    Vk.writeField @"queueFamilyIndex" infoPtr (queueFamilyIndex p)
    Vk.writeField @"queueCount" infoPtr (queueCount p)
    Vk.writeField @"pQueuePriorities" infoPtr priosPtr
