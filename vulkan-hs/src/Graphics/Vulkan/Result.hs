{-# language DeriveDataTypeable #-}
module Graphics.Vulkan.Result where

import Control.Exception (Exception, throwIO)
import Data.Typeable (Typeable)

import qualified Graphics.Vulkan.Core_1_0 as Vk
import qualified Graphics.Vulkan.Core_1_1 as Vk
-- import qualified Graphics.Vulkan.Ext.VK_EXT_buffer_device_address as Vk
import qualified Graphics.Vulkan.Ext.VK_EXT_debug_report as Vk
import qualified Graphics.Vulkan.Ext.VK_EXT_descriptor_indexing as Vk
import qualified Graphics.Vulkan.Ext.VK_EXT_external_memory_host as Vk
-- import qualified Graphics.Vulkan.Ext.VK_EXT_full_screen_exclusive as Vk
import qualified Graphics.Vulkan.Ext.VK_EXT_global_priority as Vk
-- import qualified Graphics.Vulkan.Ext.VK_EXT_image_drm_format_modifier as Vk
import qualified Graphics.Vulkan.Ext.VK_KHR_display_swapchain as Vk
import qualified Graphics.Vulkan.Ext.VK_KHR_external_memory as Vk
import qualified Graphics.Vulkan.Ext.VK_KHR_maintenance1 as Vk
import qualified Graphics.Vulkan.Ext.VK_KHR_surface as Vk
import qualified Graphics.Vulkan.Ext.VK_KHR_swapchain as Vk
import qualified Graphics.Vulkan.Ext.VK_NV_glsl_shader as Vk

data VkError
  = NotReady
  | Timeout
  | EventSet
  | EventReset
  | Incomplete
  | OutOfHostMemory
  | OutOfDeviceMemory
  | InitializationFailed
  | DeviceLost
  | MemoryMapFailed
  | LayerNotPresent
  | ExtensionNotPresent
  | FeatureNotPresent
  | IncompatibleDriver
  | TooManyObjects
  | FormatNotSupported
  | FragmentedPool
  | OutOfPoolMemory
  | InvalidExternalHandle
  | SurfaceLostKHR
  | NativeWindowInUseKHR
  | SuboptimalKHR
  | OutOfDateKHR
  | IncompatibleDisplayKHR
  | ValidationFailedEXT
  | InvalidShaderNV
  | InvalidDrmFormatModifierPlaneLayoutEXT
  | FragmentationEXT
  | NotPermittedEXT
  | InvalidDeviceAddressEXT
  | FullScreenExclusiveModeLostEXT
  | OutOfPoolMemoryKHR
  | InvalidExternalHandleKHR
  deriving (Eq, Ord, Show, Typeable)

instance Exception VkError

vkResult :: Vk.VkResult -> IO ()
vkResult res =
  case res of
    Vk.VK_SUCCESS -> pure ()
    Vk.VK_NOT_READY -> throwIO NotReady
    Vk.VK_TIMEOUT -> throwIO Timeout
    Vk.VK_EVENT_SET -> throwIO EventSet
    Vk.VK_EVENT_RESET -> throwIO EventReset
    Vk.VK_INCOMPLETE -> throwIO Incomplete
    Vk.VK_ERROR_OUT_OF_HOST_MEMORY -> throwIO OutOfHostMemory
    Vk.VK_ERROR_OUT_OF_DEVICE_MEMORY -> throwIO OutOfDeviceMemory
    Vk.VK_ERROR_INITIALIZATION_FAILED -> throwIO InitializationFailed
    Vk.VK_ERROR_DEVICE_LOST -> throwIO DeviceLost
    Vk.VK_ERROR_MEMORY_MAP_FAILED -> throwIO MemoryMapFailed
    Vk.VK_ERROR_LAYER_NOT_PRESENT -> throwIO LayerNotPresent
    Vk.VK_ERROR_EXTENSION_NOT_PRESENT -> throwIO ExtensionNotPresent
    Vk.VK_ERROR_FEATURE_NOT_PRESENT -> throwIO FeatureNotPresent
    Vk.VK_ERROR_INCOMPATIBLE_DRIVER -> throwIO IncompatibleDriver
    Vk.VK_ERROR_TOO_MANY_OBJECTS -> throwIO TooManyObjects
    Vk.VK_ERROR_FORMAT_NOT_SUPPORTED -> throwIO FormatNotSupported
    Vk.VK_ERROR_FRAGMENTED_POOL -> throwIO FragmentedPool
    Vk.VK_ERROR_OUT_OF_POOL_MEMORY -> throwIO OutOfPoolMemory
    Vk.VK_ERROR_INVALID_EXTERNAL_HANDLE -> throwIO InvalidExternalHandle
    Vk.VK_ERROR_SURFACE_LOST_KHR -> throwIO SurfaceLostKHR
    Vk.VK_ERROR_NATIVE_WINDOW_IN_USE_KHR -> throwIO NativeWindowInUseKHR
    Vk.VK_SUBOPTIMAL_KHR -> throwIO SuboptimalKHR
    Vk.VK_ERROR_OUT_OF_DATE_KHR -> throwIO OutOfDateKHR
    Vk.VK_ERROR_INCOMPATIBLE_DISPLAY_KHR -> throwIO IncompatibleDisplayKHR
    Vk.VK_ERROR_VALIDATION_FAILED_EXT -> throwIO ValidationFailedEXT
    Vk.VK_ERROR_INVALID_SHADER_NV -> throwIO InvalidShaderNV
    -- Vk.VK_ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT ->
      -- throwIO InvalidDrmFormatModifierPlaneLayoutEXT
    Vk.VK_ERROR_FRAGMENTATION_EXT -> throwIO FragmentationEXT
    Vk.VK_ERROR_NOT_PERMITTED_EXT -> throwIO NotPermittedEXT
    -- Vk.VK_ERROR_INVALID_DEVICE_ADDRESS_EXT -> throwIO InvalidDeviceAddressEXT
    -- Vk.VK_ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT -> throwIO FullScreenExclusiveModeLostEXT
    Vk.VK_ERROR_OUT_OF_POOL_MEMORY_KHR -> throwIO OutOfPoolMemoryKHR
    Vk.VK_ERROR_INVALID_EXTERNAL_HANDLE_KHR -> throwIO InvalidExternalHandleKHR
