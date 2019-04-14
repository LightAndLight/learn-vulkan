{-# language DataKinds, TypeApplications #-}
{-# language EmptyCase, EmptyDataDeriving #-}
{-# language ScopedTypeVariables #-}
module Graphics.Vulkan.Device
  ( Vk.VkDevice
  , vkCreateDevice
  , vkGetDeviceQueue
  , vkDeviceWaitIdle
  , Vk.VkDeviceMemory
  , vkAllocateMemory
  , vkMapMemory
  , VkMemoryMapFlag(..), unVkMemoryMapBits
  , VkMemoryAllocateInfo(..), unVkMemoryAllocateInfo
  )
where

import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Managed.Safe (MonadManaged, using, managed)
import Data.Maybe (fromMaybe)
import Data.Void (Void)
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

vkDeviceWaitIdle :: MonadIO m => Vk.VkDevice -> m ()
vkDeviceWaitIdle d = liftIO $ vkResult =<< Vk.vkDeviceWaitIdle d

data VkMemoryAllocateInfo
  = VkMemoryAllocateInfo
  { allocationSize :: Vk.VkDeviceSize
  , memoryTypeIndex :: Word32
  } deriving (Eq, Ord, Show)

unVkMemoryAllocateInfo ::
  MonadIO m =>
  VkMemoryAllocateInfo ->
  m Vk.VkMemoryAllocateInfo
unVkMemoryAllocateInfo info =
  liftIO . Vk.newVkData $ \ptr -> do
    Vk.writeField @"sType" ptr Vk.VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO
    Vk.writeField @"pNext" ptr Foreign.nullPtr
    Vk.writeField @"allocationSize" ptr $ allocationSize info
    Vk.writeField @"memoryTypeIndex" ptr $ memoryTypeIndex info

vkAllocateMemory ::
  MonadManaged m =>
  Vk.VkDevice ->
  VkMemoryAllocateInfo ->
  Foreign.Ptr Vk.VkAllocationCallbacks ->
  m Vk.VkDeviceMemory
vkAllocateMemory dev info cbs = do
  memPtr <- using $ managed Foreign.alloca
  info' <- unVkMemoryAllocateInfo info
  liftIO $ vkResult =<< Vk.vkAllocateMemory dev (Vk.unsafePtr info') cbs memPtr
  using $ managed (bracket (Foreign.peek memPtr) (\m -> Vk.vkFreeMemory dev m cbs))

data VkMemoryMapFlag
  deriving (Eq, Ord, Show)

unVkMemoryMapBits ::
  [VkMemoryMapFlag] ->
  Vk.VkMemoryMapFlags
unVkMemoryMapBits [] = 0
unVkMemoryMapBits (x:_) = case x of

vkMapMemory ::
  MonadManaged m =>
  Vk.VkDevice ->
  Vk.VkDeviceMemory ->
  Vk.VkDeviceSize ->
  Maybe Vk.VkDeviceSize ->
  [VkMemoryMapFlag] ->
  m (Foreign.Ptr a)
vkMapMemory dev mem off m_size fs = do
  pPtr :: Foreign.Ptr (Foreign.Ptr Void) <- using $ managed Foreign.alloca
  liftIO $
    vkResult =<<
    Vk.vkMapMemory
      dev
      mem
      off
      (fromMaybe (fromIntegral Vk.VK_WHOLE_SIZE) m_size)
      (unVkMemoryMapBits fs)
      pPtr
  using $ managed (bracket
    (Foreign.peek $ Foreign.castPtr pPtr)
    (\_ -> Vk.vkUnmapMemory dev mem))
