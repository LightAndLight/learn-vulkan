module Graphics.Vulkan.Instance where

import qualified Foreign.Marshal.Alloc as Foreign
import qualified Foreign.Ptr as Foreign
import qualified Foreign.Storable as Foreign
import qualified Graphics.Vulkan.Core_1_0 as Vk

import Graphics.Vulkan.InstanceCreateInfo (VkInstanceCreateInfo, withInstanceCreateInfo)
import Graphics.Vulkan.Result (vkResult)

withInstance ::
  VkInstanceCreateInfo ->
  Foreign.Ptr Vk.VkAllocationCallbacks ->
  (Vk.VkInstance -> IO ()) ->
  IO ()
withInstance icInfo allocationCallbacks f =
  withInstanceCreateInfo icInfo $ \icInfoPtr ->
  Foreign.alloca $ \instancePtr -> do
    vkResult =<< Vk.vkCreateInstance icInfoPtr allocationCallbacks instancePtr
    inst <- Foreign.peek instancePtr
    f inst
    Vk.vkDestroyInstance inst allocationCallbacks
