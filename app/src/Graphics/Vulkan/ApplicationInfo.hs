{-# language DataKinds, TypeApplications #-}
module Graphics.Vulkan.ApplicationInfo where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Managed.Safe (MonadManaged, using, managed)
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

mkApplicationInfo ::
  (MonadManaged m, MonadIO m) =>
  VkApplicationInfo ->
  m (Foreign.Ptr Vk.VkApplicationInfo)
mkApplicationInfo ai = do
  appNamePtr <- using $ managed (Foreign.withCString $ pApplicationName ai)
  engineNamePtr <- using $ managed (Foreign.withCString $ pEngineName ai)
  liftIO $
    fmap Vk.unsafePtr <$>
    Vk.newVkData @Vk.VkApplicationInfo $ \appInfoPtr -> do
      Vk.writeField @"sType" appInfoPtr Vk.VK_STRUCTURE_TYPE_APPLICATION_INFO
      Vk.writeField @"pApplicationName" appInfoPtr appNamePtr
      Vk.writeField @"applicationVersion" appInfoPtr (applicationVersion ai)
      Vk.writeField @"pEngineName" appInfoPtr engineNamePtr
      Vk.writeField @"engineVersion" appInfoPtr (engineVersion ai)
      Vk.writeField @"apiVersion" appInfoPtr (apiVersion ai)
