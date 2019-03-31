{-# language DataKinds, TypeApplications #-}
{-# language EmptyCase #-}
{-# language EmptyDataDeriving #-}
module Graphics.Vulkan.DeviceCreateInfo where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Managed.Safe (MonadManaged, using, managed)
import Data.Word (Word32)

import qualified Foreign.C.String as Foreign
import qualified Foreign.Marshal.Alloc as Foreign
import qualified Foreign.Marshal.Array as Foreign
import qualified Foreign.Storable as Foreign
import qualified Graphics.Vulkan.Core_1_0 as Vk
import qualified Graphics.Vulkan.Marshal as Vk

import Graphics.Vulkan.DeviceQueueCreateInfo (VkDeviceQueueCreateInfo, unVkDeviceQueueCreateInfo)
import Graphics.Vulkan.Ext (VkDeviceExtension, unVkDeviceExtension)
import Graphics.Vulkan.Layer (VkLayer, unVkLayer)
import Graphics.Vulkan.PhysicalDevice (VkPhysicalDeviceFeatures, unVkPhysicalDeviceFeatures)

data VkDeviceCreateFlag
  deriving (Eq, Ord, Show)

unVkDeviceCreateBits :: [VkDeviceCreateFlag] -> Vk.VkDeviceCreateFlags
unVkDeviceCreateBits [] = 0
unVkDeviceCreateBits (x:_) = case x of

data VkDeviceCreateInfo
  = VkDeviceCreateInfo
  { flags :: [VkDeviceCreateFlag]
  , pQueueCreateInfos :: [VkDeviceQueueCreateInfo]
  , ppEnabledLayerNames :: [VkLayer]
  , ppEnabledExtensionNames :: [VkDeviceExtension]
  , pEnabledFeatures :: VkPhysicalDeviceFeatures
  } deriving (Eq, Ord, Show)

unVkDeviceCreateInfo ::
  (MonadManaged m, MonadIO m) =>
  VkDeviceCreateInfo ->
  m Vk.VkDeviceCreateInfo
unVkDeviceCreateInfo p = do
  let layerNames = unVkLayer <$> ppEnabledLayerNames p
  layerNamesPtr <- using $ managed (Foreign.withArray layerNames)
  let extNames = unVkDeviceExtension <$> ppEnabledExtensionNames p
  extNamesPtr <- using $ managed (Foreign.withArray extNames)
  creates <- traverse unVkDeviceQueueCreateInfo (pQueueCreateInfos p)
  createsPtr <- using $ managed (Foreign.withArray creates)
  liftIO $ do
    Vk.newVkData $ \infoPtr -> do
      Vk.writeField @"sType" infoPtr Vk.VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO
      Vk.writeField @"pNext" infoPtr Vk.VK_NULL
      Vk.writeField @"flags" infoPtr (unVkDeviceCreateBits $ flags p)
      Vk.writeField @"queueCreateInfoCount" infoPtr
        (fromIntegral . length $ pQueueCreateInfos p)
      Vk.writeField @"pQueueCreateInfos" infoPtr createsPtr
      Vk.writeField @"enabledLayerCount" infoPtr
        (fromIntegral . length $ ppEnabledLayerNames p)
      Vk.writeField @"ppEnabledLayerNames" infoPtr layerNamesPtr
      Vk.writeField @"enabledExtensionCount" infoPtr
        (fromIntegral . length $ ppEnabledExtensionNames p)
      Vk.writeField @"ppEnabledExtensionNames" infoPtr extNamesPtr
      Vk.writeField @"pEnabledFeatures" infoPtr . Vk.unsafePtr =<<
        unVkPhysicalDeviceFeatures (pEnabledFeatures p)
