{-# language DataKinds, TypeApplications #-}
module Graphics.Vulkan.InstanceCreateInfo where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Managed.Safe (MonadManaged, using, managed)
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
  , ppEnabledExtensionNames :: [VkInstanceExtension]
  } deriving (Eq, Ord, Show)

mkInstanceCreateInfo ::
  (MonadManaged m, MonadIO m) =>
  VkInstanceCreateInfo ->
  m (Foreign.Ptr Vk.VkInstanceCreateInfo)
mkInstanceCreateInfo ii = do
  layerNamesPtr <- using $ managed (Foreign.withArray $ unVkLayer <$> layerNames)
  extNamesPtr <- using $ managed (Foreign.withArray $ unVkInstanceExtension <$> extNames)
  appInfoPtr <- mkApplicationInfo $ pApplicationInfo ii
  liftIO $
    fmap Vk.unsafePtr <$>
    Vk.newVkData @Vk.VkInstanceCreateInfo $ \ptr -> do
      Vk.writeField @"sType" ptr Vk.VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
      Vk.writeField @"pNext" ptr Vk.VK_NULL
      Vk.writeField @"pApplicationInfo" ptr appInfoPtr
      Vk.writeField @"enabledLayerCount" ptr (fromIntegral $ length layerNames)
      Vk.writeField @"ppEnabledLayerNames" ptr layerNamesPtr
      Vk.writeField @"enabledExtensionCount" ptr (fromIntegral $ length extNames)
      Vk.writeField @"ppEnabledExtensionNames" ptr extNamesPtr
  where
    layerNames = ppEnabledLayerNames ii
    extNames = ppEnabledExtensionNames ii
