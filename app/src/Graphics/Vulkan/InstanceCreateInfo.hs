{-# language DataKinds, TypeApplications #-}
module Graphics.Vulkan.InstanceCreateInfo where

import Data.Word (Word32)

import qualified Foreign.Marshal.Array as Foreign
import qualified Foreign.Ptr as Foreign
import qualified Graphics.Vulkan.Core_1_0 as Vk
import qualified Graphics.Vulkan.Marshal as Vk

import Graphics.Vulkan.ApplicationInfo
import Graphics.Vulkan.Ext
import Graphics.Vulkan.Layer

data VkInstanceCreateInfo
  = VkInstanceCreateInfo
  { pApplicationInfo :: VkApplicationInfo
  , ppEnabledLayerNames :: [VkLayer]
  , ppEnabledExtensionNames :: [VkExtension]
  } deriving (Eq, Ord, Show)

withInstanceCreateInfo ::
  VkInstanceCreateInfo ->
  (Foreign.Ptr Vk.VkInstanceCreateInfo -> IO ()) ->
  IO ()
withInstanceCreateInfo ii f =
  Foreign.withArray (unVkLayer <$> layerNames) $ \layerNamesPtr ->
  Foreign.withArray (unVkExtension <$> extNames) $ \extNamesPtr ->
  withApplicationInfo (pApplicationInfo ii) $ \appInfoPtr -> do
    info <-
      Vk.newVkData @Vk.VkInstanceCreateInfo $ \ptr -> do
        Vk.writeField @"sType" ptr Vk.VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
        Vk.writeField @"pApplicationInfo" ptr appInfoPtr
        Vk.writeField @"enabledLayerCount" ptr (fromIntegral $ length layerNames)
        Vk.writeField @"ppEnabledLayerNames" ptr layerNamesPtr
        Vk.writeField @"enabledExtensionCount" ptr (fromIntegral $ length extNames)
        Vk.writeField @"ppEnabledExtensionNames" ptr extNamesPtr
    f (Vk.unsafePtr info)
  where
    layerNames = ppEnabledLayerNames ii
    extNames = ppEnabledExtensionNames ii
