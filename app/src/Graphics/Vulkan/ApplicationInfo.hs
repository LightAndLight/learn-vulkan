{-# language DataKinds, TypeApplications #-}
module Graphics.Vulkan.ApplicationInfo where

import Data.Word (Word32)

import qualified Foreign.C.String as Foreign
import qualified Foreign.Ptr as Foreign
import qualified Graphics.Vulkan.Core_1_0 as Vk
import qualified Graphics.Vulkan.Marshal as Vk

type VkVersion = Vk.Word32

data VkApplicationInfo
  = VkApplicationInfo
  { pApplicationName :: String
  , applicationVersion :: Word32
  , pEngineName :: String
  , engineVersion :: Word32
  , apiVersion :: Word32
  } deriving (Eq, Ord, Show)

withApplicationInfo ::
  VkApplicationInfo ->
  (Foreign.Ptr Vk.VkApplicationInfo -> IO ()) ->
  IO ()
withApplicationInfo ai f =
  Foreign.withCString (pApplicationName ai) $ \appNamePtr ->
  Foreign.withCString (pEngineName ai) $ \engineNamePtr -> do
    appInfo <-
      Vk.newVkData @Vk.VkApplicationInfo $ \appInfoPtr -> do
        Vk.writeField @"sType" appInfoPtr Vk.VK_STRUCTURE_TYPE_APPLICATION_INFO
        Vk.writeField @"pApplicationName" appInfoPtr appNamePtr
        Vk.writeField @"applicationVersion" appInfoPtr (applicationVersion ai)
        Vk.writeField @"pEngineName" appInfoPtr engineNamePtr
        Vk.writeField @"engineVersion" appInfoPtr (engineVersion ai)
        Vk.writeField @"apiVersion" appInfoPtr (apiVersion ai)
    f (Vk.unsafePtr appInfo)
