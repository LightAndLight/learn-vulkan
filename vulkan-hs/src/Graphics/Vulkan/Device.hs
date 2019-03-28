module Graphics.Vulkan.Device where

import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Managed.Safe (MonadManaged, using, managed)
import Data.Word (Word32)

import qualified Foreign.Marshal.Alloc as Foreign
import qualified Foreign.Ptr as Foreign
import qualified Foreign.Storable as Foreign
import qualified Graphics.Vulkan.Core_1_0 as Vk
import qualified Graphics.Vulkan.Marshal as Vk

import Graphics.Vulkan.DeviceCreateInfo
  (VkDeviceCreateInfo, unVkDeviceCreateInfo)
import Graphics.Vulkan.Result (vkResult)

vkCreateDevice ::
  (MonadManaged m, MonadIO m) =>
  Vk.VkPhysicalDevice ->
  VkDeviceCreateInfo ->
  Foreign.Ptr Vk.VkAllocationCallbacks ->
  m Vk.VkDevice
vkCreateDevice pd ci cbs = do
  devicePtr <- using $ managed Foreign.alloca
  ciPtr <- Vk.unsafePtr <$> unVkDeviceCreateInfo ci
  liftIO $ vkResult =<< Vk.vkCreateDevice pd ciPtr cbs devicePtr
  using $ managed (bracket
    (Foreign.peek devicePtr)
    (\d -> Vk.vkDestroyDevice d cbs))

vkGetDeviceQueue ::
  (MonadManaged m, MonadIO m) =>
  Vk.VkDevice ->
  Word32 ->
  Word32 ->
  m Vk.VkQueue
vkGetDeviceQueue d qfix qix = do
  qPtr <- using $ managed Foreign.alloca
  liftIO $ do
    Vk.vkGetDeviceQueue d qfix qix qPtr
    Foreign.peek qPtr
