{-# language DataKinds, TypeApplications #-}
{-# language ViewPatterns #-}
module Graphics.Vulkan.Queue
  ( Vk.VkQueue
  , Vk.FlagType(..)
  , VkQueueType(..)
  , vkQueueBit, unVkQueueBit
  , vkQueueBits, unVkQueueBits
  , VkExtent3D(..)
  , vkExtent3D
  , VkQueueFamilyProperties(..)
  , vkQueueFamilyProperties
  , VkSubmitInfo(..)
  , vkQueueSubmit
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits ((.&.), (.|.))
import Data.Maybe (fromMaybe)
import Data.Word (Word32)
import Unsafe.Coerce (unsafeCoerce)

import qualified Foreign
import qualified Graphics.Vulkan.Core_1_0 as Vk
import qualified Graphics.Vulkan.Core_1_1 as Vk
import qualified Graphics.Vulkan.Marshal as Vk

import Graphics.Vulkan.Extent (VkExtent3D, vkExtent3D)
import Graphics.Vulkan.PipelineStage (VkPipelineStageFlag, unVkPipelineStageBits)
import Graphics.Vulkan.Result (vkResult)

data VkQueueType
  = Graphics
  | Compute
  | Transfer
  | SparseBinding
  | Protected
  deriving (Eq, Ord, Show)

vkQueueBit ::
  Vk.VkQueueBitmask a ->
  VkQueueType
vkQueueBit a =
  case a of
    Vk.VK_QUEUE_GRAPHICS_BIT -> Graphics
    Vk.VK_QUEUE_COMPUTE_BIT -> Compute
    Vk.VK_QUEUE_TRANSFER_BIT -> Transfer
    Vk.VK_QUEUE_SPARSE_BINDING_BIT -> SparseBinding
    (unsafeCoerce -> Vk.VK_QUEUE_PROTECTED_BIT) -> unsafeCoerce Protected

unVkQueueBit ::
  VkQueueType ->
  Vk.VkQueueBitmask a
unVkQueueBit a =
  case a of
    Graphics -> Vk.VK_QUEUE_GRAPHICS_BIT
    Compute -> Vk.VK_QUEUE_COMPUTE_BIT
    Transfer -> Vk.VK_QUEUE_TRANSFER_BIT
    SparseBinding -> Vk.VK_QUEUE_SPARSE_BINDING_BIT
    -- see https://github.com/achirkin/vulkan/issues/29
    Protected -> unsafeCoerce Vk.VK_QUEUE_PROTECTED_BIT

vkQueueBits ::
  Vk.VkQueueFlags ->
  [VkQueueType]
vkQueueBits a =
  foldr
  (\(mask, val) rest -> if mask .&. a == mask then val : rest else rest)
  []
  [ (Vk.VK_QUEUE_GRAPHICS_BIT, Graphics)
  , (Vk.VK_QUEUE_COMPUTE_BIT, Compute)
  , (Vk.VK_QUEUE_TRANSFER_BIT, Transfer)
  , (Vk.VK_QUEUE_SPARSE_BINDING_BIT, SparseBinding)
  , (unsafeCoerce Vk.VK_QUEUE_PROTECTED_BIT, Protected)
  ]

unVkQueueBits ::
  [VkQueueType] ->
  Vk.VkQueueFlags
unVkQueueBits = foldr (\a b -> unVkQueueBit a .|. b) 0

data VkQueueFamilyProperties
  = VkQueueFamilyProperties
  { queueFlags :: [VkQueueType]
  , queueCount :: Word32
  , timestampValidBits :: Word32
  , minImageTransferGranularity :: VkExtent3D
  } deriving (Eq, Ord, Show)

vkQueueFamilyProperties ::
  Vk.VkQueueFamilyProperties ->
  VkQueueFamilyProperties
vkQueueFamilyProperties p =
  VkQueueFamilyProperties
  { queueFlags = vkQueueBits $ Vk.getField @"queueFlags" p
  , queueCount = Vk.getField @"queueCount" p
  , timestampValidBits = Vk.getField @"timestampValidBits" p
  , minImageTransferGranularity =
      vkExtent3D $ Vk.getField @"minImageTransferGranularity" p
  }

data VkSubmitInfo
  = VkSubmitInfo
  { pWaitSemaphores :: [Vk.VkSemaphore]
  , pWaitDstStageMask :: [[VkPipelineStageFlag]]
  , pCommandBuffers :: [Vk.VkCommandBuffer]
  , pSignalSemaphores :: [Vk.VkSemaphore]
  } deriving (Eq, Ord, Show)

unVkSubmitInfos ::
  MonadIO m =>
  [VkSubmitInfo] ->
  m (Foreign.Ptr Vk.VkSubmitInfo)
unVkSubmitInfos as =
  liftIO $ do
    (ptr, _) <- Vk.mallocVkDataArray (fromIntegral $ length as)
    ptr <$ go ptr as
  where
    go ::
      Foreign.Ptr Vk.VkSubmitInfo ->
      [VkSubmitInfo] ->
      IO ()
    go ptr [] = pure ()
    go ptr (x:xs) =
      Foreign.withArray (pWaitSemaphores x) $ \wsPtr ->
      Foreign.withArray (unVkPipelineStageBits <$> pWaitDstStageMask x) $ \wsmPtr ->
      Foreign.withArray (pCommandBuffers x) $ \cbPtr ->
      Foreign.withArray (pSignalSemaphores x) $ \ssPtr -> do
        Vk.writeField @"sType" ptr Vk.VK_STRUCTURE_TYPE_SUBMIT_INFO
        Vk.writeField @"waitSemaphoreCount" ptr (fromIntegral . length $ pWaitSemaphores x)
        Vk.writeField @"pWaitSemaphores" ptr wsPtr
        Vk.writeField @"pWaitDstStageMask" ptr wsmPtr
        Vk.writeField @"commandBufferCount" ptr (fromIntegral . length $ pCommandBuffers x)
        Vk.writeField @"pCommandBuffers" ptr cbPtr
        Vk.writeField @"signalSemaphoreCount" ptr (fromIntegral . length $ pSignalSemaphores x)
        Vk.writeField @"pSignalSemaphores" ptr ssPtr
        go (Foreign.plusPtr ptr $ Foreign.sizeOf (undefined::Vk.VkSubmitInfo)) xs

vkQueueSubmit :: MonadIO m => Vk.VkQueue -> [VkSubmitInfo] -> Maybe Vk.VkFence -> m ()
vkQueueSubmit queue infos fence = do
  infosPtr <- unVkSubmitInfos infos
  liftIO $
    vkResult =<<
    Vk.vkQueueSubmit
      queue
      (fromIntegral $ length infos)
      infosPtr
      (fromMaybe Vk.VK_NULL_HANDLE fence)
