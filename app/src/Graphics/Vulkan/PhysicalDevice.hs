module Graphics.Vulkan.PhysicalDevice where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Word (Word32)

import qualified Graphics.Vulkan.Core_1_0 as Vk
import qualified Foreign.Marshal.Alloc as Foreign
import qualified Foreign.Marshal.Array as Foreign
import qualified Foreign.Ptr as Foreign
import qualified Foreign.Storable as Foreign

import Graphics.Vulkan.Result (vkResult)

vkEnumeratePhysicalDevices :: MonadIO m => Vk.VkInstance -> m [Vk.VkPhysicalDevice]
vkEnumeratePhysicalDevices inst =
  liftIO $
  Foreign.alloca $ \countPtr -> do
    vkResult =<< Vk.vkEnumeratePhysicalDevices inst countPtr Foreign.nullPtr
    count <- fromIntegral <$> Foreign.peek countPtr
    Foreign.allocaArray count $ \arrayPtr -> do
      vkResult =<< Vk.vkEnumeratePhysicalDevices inst countPtr arrayPtr
      Foreign.peekArray count arrayPtr

data VkPhysicalDeviceType
  = Other
  | IntegratedGpu
  | DiscreteGpu
  | VirtualGpu
  | Cpu
  deriving (Eq, Ord, Show)

vkPhysicalDeviceType :: Vk.VkPhysicalDeviceType -> VkPhysicalDeviceType
vkPhysicalDeviceType a =
  case a of
    Vk.VK_PHYSICAL_DEVICE_TYPE_OTHER -> Other
    Vk.VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU -> IntegratedGpu
    Vk.VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU -> DiscreteGpu
    Vk.VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU -> VirtualGpu

unVkPhysicalDeviceType :: VkPhysicalDeviceType -> Vk.VkPhysicalDeviceType
unVkPhysicalDeviceType a =
  case a of
    Other -> Vk.VK_PHYSICAL_DEVICE_TYPE_OTHER
    IntegratedGpu -> Vk.VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU
    DiscreteGpu -> Vk.VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU
    VirtualGpu -> Vk.VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU

data VkPhysicalDeviceLimits = VkPhysicalDeviceLimits deriving (Eq, Ord, Show)
data VkPhysicalDeviceSparseProperties = VkPhysicalDeviceSparseProperties deriving (Eq, Ord, Show)

data VkPhysicalDeviceProperties
  = VkPhysicalDeviceProperties
  { apiVersion :: Word32
  , driverVersion :: Word32
  , vendorID :: Word32
  , deviceID :: Word32
  , deviceType :: VkPhysicalDeviceType
  , deviceName :: String
  , pipelineCacheUUID :: [Word32]
  , limits :: VkPhysicalDeviceLimits
  , sparseProperties :: VkPhysicalDeviceSparseProperties
  } deriving (Eq, Ord, Show)

vkGetPhysicalDeviceProperties ::
  MonadIO m =>
  Vk.VkPhysicalDevice -> m
  VkPhysicalDeviceProperties
vkGetPhysicalDeviceProperties d = _
