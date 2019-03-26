{-# language DeriveDataTypeable #-}
{-# language DataKinds #-}
{-# language MagicHash #-}
{-# language PatternSynonyms, ViewPatterns #-}
{-# language TypeApplications #-}
module Main where

import Control.Exception (Exception(..), throwIO)
import Control.Monad (unless)
import Data.Foldable (fold, traverse_)
import Data.Traversable (for)
import Data.Typeable (Typeable)
import Data.Word (Word32)
import Graphics.UI.GLFW (ClientAPI(..), WindowHint(..))
import Graphics.Vulkan (_VK_MAKE_VERSION)
import Graphics.Vulkan.Core_1_0
  ( VkApplicationInfo, VkInstanceCreateInfo, VkInstance, VkAllocationCallbacks
  , VkInstanceCreateInfo
  )

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
import qualified Graphics.Vulkan.Core_1_0 as Vk
import qualified Graphics.Vulkan.Marshal as Vk
import qualified Graphics.UI.GLFW as GLFW

import Graphics.Vulkan.Layer.VK_LAYER_LUNARG_standard_validation
  (pattern VK_LAYER_LUNARG_standard_validation)

mainLoop :: GLFW.Window -> IO ()
mainLoop window = go
  where
    go = do
      close <- GLFW.windowShouldClose window
      unless close $ GLFW.pollEvents *> go

vulkanGLFW :: IO a -> IO a
vulkanGLFW m = do
  initSucceeded <- GLFW.init
  unless initSucceeded $ error "glfw init failed"
  vkSupported <- GLFW.vulkanSupported
  unless vkSupported $ error "glfw vulkan not supported"
  m <* GLFW.terminate

withWindow ::
  [GLFW.WindowHint] ->
  Int ->
  Int ->
  String ->
  Maybe GLFW.Monitor ->
  Maybe GLFW.Window ->
  (GLFW.Window -> IO ()) ->
  IO ()
withWindow hints w h name mMonitor mWindow f = do
  traverse_ GLFW.windowHint hints
  mWindow <- GLFW.createWindow w h name mMonitor mWindow
  case mWindow of
    Nothing -> error "glfw window create failed"
    Just window -> f window *> GLFW.destroyWindow window

type VkVersion = Vk.Word32

newAppInfo ::
  String -> -- ^ Application name
  String -> -- ^ Engine name
  VkVersion -> -- ^ Application version
  VkVersion -> -- ^ Engine version
  VkVersion -> -- ^ Api version
  IO VkApplicationInfo
newAppInfo appName engineName appVersion engineVersion apiVersion =
  Foreign.withCString appName $ \appNamePtr ->
  Foreign.withCString engineName $ \engineNamePtr ->
  Vk.newVkData @VkApplicationInfo $ \ptr -> do
    Vk.writeField @"sType" ptr Vk.VK_STRUCTURE_TYPE_APPLICATION_INFO
    Vk.writeField @"pApplicationName" ptr appNamePtr
    Vk.writeField @"applicationVersion" ptr appVersion
    Vk.writeField @"pEngineName" ptr engineNamePtr
    Vk.writeField @"engineVersion" ptr engineVersion
    Vk.writeField @"apiVersion" ptr apiVersion

data VkLayer
  = LunargStandardValidation
  | UnknownLayer Foreign.CString

vkLayer :: Foreign.CString -> VkLayer
vkLayer str =
  case str of
    VK_LAYER_LUNARG_standard_validation -> LunargStandardValidation
    _ -> UnknownLayer str

unVkLayer :: VkLayer -> Foreign.CString
unVkLayer layer =
  case layer of
    LunargStandardValidation -> VK_LAYER_LUNARG_standard_validation
    UnknownLayer str -> str


newInstanceCreateInfo ::
  Vk.Ptr VkApplicationInfo ->
  Vk.Word32 ->
  -- Vk.Ptr Vk.CString ->
  [VkLayer] ->
  Vk.Word32 ->
  [VkExtension] ->
  IO VkInstanceCreateInfo
newInstanceCreateInfo appInfo layerCount layerNames extCount extNames =
  Foreign.withArray (unVkLayer <$> layerNames) $ \layerNamesPtr ->
  Foreign.withArray (unVkExtension <$> extNames) $ \extNamesPtr ->
  Vk.newVkData @VkInstanceCreateInfo $ \ptr -> do
    Vk.writeField @"sType" ptr Vk.VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
    Vk.writeField @"pApplicationInfo" ptr appInfo
    Vk.writeField @"enabledLayerCount" ptr layerCount
    Vk.writeField @"ppEnabledLayerNames" ptr layerNamesPtr
    Vk.writeField @"enabledExtensionCount" ptr extCount
    Vk.writeField @"ppEnabledExtensionNames" ptr extNamesPtr

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

withInstance ::
  Vk.Ptr VkInstanceCreateInfo ->
  Vk.Ptr VkAllocationCallbacks ->
  (VkInstance -> IO ()) ->
  IO ()
withInstance createInstanceInfo allocationCallbacks f = do
  Foreign.alloca $ \instancePtr -> do
    vkResult =<< Vk.vkCreateInstance createInstanceInfo allocationCallbacks instancePtr
    instance_ <- Foreign.peek instancePtr
    f instance_
    Vk.vkDestroyInstance instance_ allocationCallbacks

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
  | UnknownExtension Vk.CString
  deriving (Eq, Show, Ord)

vkExtension :: Vk.CString -> VkExtension
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

unVkExtension :: VkExtension -> Vk.CString
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

vkEnumerateInstanceExtensionProperties :: Maybe String -> IO [(VkExtension, Word32)]
vkEnumerateInstanceExtensionProperties mLayerName =
  Foreign.withCString (fold mLayerName) $ \layerName ->
  Foreign.alloca $ \countPtr -> do
    vkResult =<< Vk.vkEnumerateInstanceExtensionProperties layerName countPtr Foreign.nullPtr
    count <- fromIntegral <$> Foreign.peek countPtr
    Foreign.allocaArray count $ \propertiesPtr -> do
      vkResult =<< Vk.vkEnumerateInstanceExtensionProperties layerName countPtr propertiesPtr
      properties <- Foreign.peekArray count propertiesPtr
      for properties $ \property ->
        (,) <$>
        Vk.withCStringField @"extensionName" property (pure . vkExtension) <*>
        Vk.readField @"specVersion" (Vk.unsafePtr property)

vkEnumerateInstanceLayerProperties :: IO [(String, Word32, Word32, String)]
vkEnumerateInstanceLayerProperties =
  Foreign.alloca $ \countPtr -> do
    vkResult =<< Vk.vkEnumerateInstanceLayerProperties countPtr Foreign.nullPtr
    count <- fromIntegral <$> Foreign.peek countPtr
    Foreign.allocaArray count $ \propertiesPtr -> do
      vkResult =<< Vk.vkEnumerateInstanceLayerProperties countPtr propertiesPtr
      properties <- Foreign.peekArray count propertiesPtr
      for properties $ \property ->
        (,,,) (Vk.getStringField @"layerName" property) <$>
        Vk.readField @"specVersion" (Vk.unsafePtr property) <*>
        Vk.readField @"implementationVersion" (Vk.unsafePtr property) <*>
        pure (Vk.getStringField @"description" property)

getRequiredInstanceExtensions :: IO (Word32, [VkExtension])
getRequiredInstanceExtensions = do
  exts <- GLFW.getRequiredInstanceExtensions
  let extsCount = fromIntegral $ length exts
  let extsNames = vkExtension <$> exts
  pure (extsCount, extsNames)

main :: IO ()
main =
  vulkanGLFW $
  withWindow hints 1280 960 "vulkan" Nothing Nothing $ \window -> do
    appInfo <-
      newAppInfo
        "Demo"
        "No Engine"
        (_VK_MAKE_VERSION 1 0 0)
        (_VK_MAKE_VERSION 1 0 0)
        (_VK_MAKE_VERSION 1 1 82)
    (extsCount, extsNames) <- getRequiredInstanceExtensions
    instanceCreateInfo <-
      newInstanceCreateInfo
        (Vk.unsafePtr appInfo) -- can we avoid this?
        1
        [LunargStandardValidation]
        extsCount
        extsNames
    withInstance (Vk.unsafePtr instanceCreateInfo) Foreign.nullPtr $ \instance_ -> do
      print =<< vkEnumerateInstanceExtensionProperties Nothing
      print =<< vkEnumerateInstanceLayerProperties
      mainLoop window
  where
    hints =
      [ WindowHint'ClientAPI ClientAPI'NoAPI
      , WindowHint'Resizable False
      ]
