{-# language DeriveDataTypeable #-}
module Graphics.Vulkan.Result where

import Control.Exception (Exception, throwIO)
import Data.Typeable (Typeable)

import qualified Graphics.Vulkan.Core_1_0 as Vk

data VkError
  = OutOfHostMemory
  | OutOfDeviceMemory
  | InitializationFailed
  | LayerNotPresent
  | ExtensionNotPresent
  | IncompatibleDriver
  deriving (Eq, Ord, Show, Typeable)

instance Exception VkError

vkResult :: Vk.VkResult -> IO ()
vkResult res =
  case res of
    Vk.VK_SUCCESS -> pure ()
    Vk.VK_ERROR_OUT_OF_HOST_MEMORY -> throwIO OutOfHostMemory
    Vk.VK_ERROR_OUT_OF_DEVICE_MEMORY -> throwIO OutOfDeviceMemory
    Vk.VK_ERROR_INITIALIZATION_FAILED -> throwIO InitializationFailed
    Vk.VK_ERROR_LAYER_NOT_PRESENT -> throwIO LayerNotPresent
    Vk.VK_ERROR_EXTENSION_NOT_PRESENT -> throwIO ExtensionNotPresent
    Vk.VK_ERROR_INCOMPATIBLE_DRIVER -> throwIO IncompatibleDriver
