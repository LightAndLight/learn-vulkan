module Graphics.Vulkan.Instance
  ( Vk.VkInstance
  , mkInstance
  )
where

import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Managed.Safe (MonadManaged, using, managed)

import qualified Foreign.Marshal.Alloc as Foreign
import qualified Foreign.Ptr as Foreign
import qualified Foreign.Storable as Foreign
import qualified Graphics.Vulkan.Core_1_0 as Vk

import Graphics.Vulkan.InstanceCreateInfo (VkInstanceCreateInfo, mkInstanceCreateInfo)
import Graphics.Vulkan.Result (vkResult)

mkInstance ::
  (MonadManaged m, MonadIO m) =>
  VkInstanceCreateInfo ->
  Foreign.Ptr Vk.VkAllocationCallbacks ->
  m Vk.VkInstance
mkInstance icInfo allocationCallbacks = do
  icInfoPtr <- mkInstanceCreateInfo icInfo
  instancePtr <- using $ managed Foreign.alloca
  liftIO $ vkResult =<< Vk.vkCreateInstance icInfoPtr allocationCallbacks instancePtr
  using $ managed (bracket
    (Foreign.peek instancePtr)
    (\i -> Vk.vkDestroyInstance i allocationCallbacks))
