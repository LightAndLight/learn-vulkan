{-# language DataKinds, TypeApplications #-}
{-# language PatternSynonyms #-}
module Graphics.Vulkan.Ext where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (fold)
import Data.Word (Word32)
import Graphics.Vulkan.Ext.VK_KHR_device_group_creation
  (pattern VK_KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME)
import Graphics.Vulkan.Ext.VK_KHR_external_fence_capabilities
  (pattern VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME)
import Graphics.Vulkan.Ext.VK_KHR_external_memory_capabilities
  (pattern VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME)
import Graphics.Vulkan.Ext.VK_KHR_external_semaphore_capabilities
  (pattern VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME)
import Graphics.Vulkan.Ext.VK_KHR_get_physical_device_properties2
  (pattern VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME)
import Graphics.Vulkan.Ext.VK_KHR_get_surface_capabilities2
  (pattern VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME)
import Graphics.Vulkan.Ext.VK_KHR_surface
  (pattern VK_KHR_SURFACE_EXTENSION_NAME)
{-
import Graphics.Vulkan.Ext.VK_KHR_wayland_surface
  (pattern VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME)
import Graphics.Vulkan.Ext.VK_KHR_xcb_surface
  (pattern VK_KHR_XCB_SURFACE_EXTENSION_NAME)
import Graphics.Vulkan.Ext.VK_KHR_xlib_surface
  (pattern VK_KHR_XLIB_SURFACE_EXTENSION_NAME)
-}
import Graphics.Vulkan.Ext.VK_EXT_debug_report
  (pattern VK_EXT_DEBUG_REPORT_EXTENSION_NAME)
import Graphics.Vulkan.Ext.VK_EXT_debug_utils
  (pattern VK_EXT_DEBUG_UTILS_EXTENSION_NAME)

import qualified Foreign.C.String as Foreign
import qualified Foreign.Marshal.Alloc as Foreign
import qualified Foreign.Marshal.Array as Foreign
import qualified Foreign.Ptr as Foreign
import qualified Foreign.Storable as Foreign
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Vulkan.Core_1_0 as Vk
import qualified Graphics.Vulkan.Marshal as Vk

import Graphics.Vulkan.Result (vkResult)

data VkExtension
  = DeviceGroupCreation
  | ExternalFenceCapabilities
  | ExternalMemoryCapabilities
  | ExternalSemaphoreCapabilities
  | GetPhysicalDeviceProperties2
  | GetSurfaceCapabilities2
  | Surface
  | WaylandSurface
  | XcbSurface
  | XlibSurface
  | DebugReport
  | DebugUtils
  | UnknownExtension Foreign.CString
  deriving (Eq, Show, Ord)

vkExtension :: Foreign.CString -> VkExtension
vkExtension ext =
  case ext of
    VK_KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME -> DeviceGroupCreation
    VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME -> ExternalFenceCapabilities
    VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME -> ExternalMemoryCapabilities
    VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME -> ExternalSemaphoreCapabilities
    VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME -> GetPhysicalDeviceProperties2
    VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME -> GetSurfaceCapabilities2
    VK_KHR_SURFACE_EXTENSION_NAME -> Surface
    -- VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME -> pure WaylandSurface
    -- VK_KHR_XCB_SURFACE_EXTENSION_NAME -> pure XcbSurface
    -- VK_KHR_XLIB_SURFACE_EXTENSION_NAME -> pure XlibSurface
    VK_EXT_DEBUG_REPORT_EXTENSION_NAME -> DebugReport
    VK_EXT_DEBUG_UTILS_EXTENSION_NAME -> DebugUtils
    _ -> UnknownExtension ext

unVkExtension :: VkExtension -> Foreign.CString
unVkExtension ext =
  case ext of
    DeviceGroupCreation -> VK_KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME
    ExternalFenceCapabilities -> VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME
    ExternalMemoryCapabilities -> VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME
    ExternalSemaphoreCapabilities -> VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME
    GetPhysicalDeviceProperties2 -> VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
    GetSurfaceCapabilities2 -> VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME
    Surface -> VK_KHR_SURFACE_EXTENSION_NAME
    -- WaylandSurface -> VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME
    -- XcbSurface -> VK_KHR_XCB_SURFACE_EXTENSION_NAME
    -- XlibSurface -> VK_KHR_XLIB_SURFACE_EXTENSION_NAME
    DebugReport -> VK_EXT_DEBUG_REPORT_EXTENSION_NAME
    DebugUtils -> VK_EXT_DEBUG_UTILS_EXTENSION_NAME
    UnknownExtension str -> str

getRequiredInstanceExtensions :: MonadIO m => m [VkExtension]
getRequiredInstanceExtensions = liftIO $ fmap vkExtension <$> GLFW.getRequiredInstanceExtensions

data VkExtensionProperties
  = VkExtensionProperties
  { extensionName :: VkExtension
  , specVersion :: Word32
  } deriving (Eq, Ord, Show)

vkExtensionProperties :: MonadIO m => Vk.VkExtensionProperties -> m VkExtensionProperties
vkExtensionProperties a =
  liftIO $
  (\ename ->
     VkExtensionProperties
     { extensionName = ename
     , specVersion = Vk.getField @"specVersion" a
     }) <$>
  Vk.withCStringField @"extensionName" a (pure . vkExtension)

vkEnumerateInstanceExtensionProperties :: MonadIO m => Maybe String -> m [VkExtensionProperties]
vkEnumerateInstanceExtensionProperties mLayerName =
  liftIO $
  Foreign.withCString (fold mLayerName) $ \layerName ->
  Foreign.alloca $ \countPtr -> do
    vkResult =<< Vk.vkEnumerateInstanceExtensionProperties layerName countPtr Foreign.nullPtr
    count <- fromIntegral <$> Foreign.peek countPtr
    Foreign.allocaArray count $ \propertiesPtr -> do
      vkResult =<< Vk.vkEnumerateInstanceExtensionProperties layerName countPtr propertiesPtr
      properties <- Foreign.peekArray count propertiesPtr
      traverse vkExtensionProperties properties
