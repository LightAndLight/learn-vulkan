module Graphics.Vulkan.Ext.Surface where

import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Managed.Safe (MonadManaged, using, managed)
import Data.Word (Word32)

import qualified Foreign.Marshal.Alloc as Foreign
import qualified Foreign.Storable as Foreign
import qualified Foreign.Ptr as Foreign
import qualified Graphics.Vulkan.Ext.VK_KHR_surface as Vk
import qualified Graphics.Vulkan.Marshal as Vk
import qualified Graphics.UI.GLFW as GLFW

import Graphics.Vulkan.Result (vkResult)
import Graphics.Vulkan.Utils (vkBool32)

glfwCreateWindowSurface ::
  (MonadManaged m, MonadIO m) =>
  Vk.VkInstance ->
  GLFW.Window ->
  Foreign.Ptr Vk.VkAllocationCallbacks ->
  m Vk.VkSurfaceKHR
glfwCreateWindowSurface inst win cbs = do
  surfacePtr <- using $ managed Foreign.alloca
  liftIO $ vkResult =<< GLFW.createWindowSurface inst win cbs surfacePtr
  using $ managed (bracket
    (Foreign.peek surfacePtr)
    (\s -> Vk.vkDestroySurfaceKHR inst s cbs))

vkGetPhysicalDeviceSurfaceSupportKHR ::
  MonadIO m =>
  Vk.VkPhysicalDevice ->
  Word32 ->
  Vk.VkSurfaceKHR ->
  m Bool
vkGetPhysicalDeviceSurfaceSupportKHR pd qfix surf =
  liftIO . Foreign.alloca $ \bPtr -> do
    vkResult =<< Vk.vkGetPhysicalDeviceSurfaceSupportKHR pd qfix surf bPtr
    vkBool32 <$> Foreign.peek bPtr
