{-# language DataKinds, TypeApplications #-}
module Graphics.Vulkan.CommandBuffer
  ( Vk.VkCommandBuffer
  , VkCommandBufferLevel(..)
  , VkCommandBufferAllocateInfo(..)
  , vkAllocateCommandBuffers
  , VkCommandBufferUsageFlag(..)
  , unVkCommandBufferUsageBit, unVkCommandBufferUsageBits
  , VkCommandBufferInheritanceInfo(..)
  , unVkCommandBufferInheritanceInfo
  , VkCommandBufferBeginInfo(..)
  , unVkCommandBufferBeginInfo
  , vkBeginCommandBuffer
  , vkEndCommandBuffer
  , withCommandBuffer
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits ((.|.))
import Data.Word (Word32)

import qualified Foreign
import qualified Graphics.Vulkan.Core_1_0 as Vk

import Graphics.Vulkan.Query.Control (VkQueryControlFlag, unVkQueryControlBits)
import Graphics.Vulkan.Query.PipelineStatistic (VkQueryPipelineStatisticFlag, unVkQueryPipelineStatisticBits)
import Graphics.Vulkan.Result (vkResult)
import Graphics.Vulkan.Utils (unVkBool32)

data VkCommandBufferLevel
  = Primary
  | Secondary
  deriving (Eq, Ord, Show)

unVkCommandBufferLevel ::
  VkCommandBufferLevel ->
  Vk.VkCommandBufferLevel
unVkCommandBufferLevel a =
  case a of
    Primary -> Vk.VK_COMMAND_BUFFER_LEVEL_PRIMARY
    Secondary -> Vk.VK_COMMAND_BUFFER_LEVEL_SECONDARY

data VkCommandBufferAllocateInfo
  = VkCommandBufferAllocateInfo
  { commandPool :: Vk.VkCommandPool
  , level :: VkCommandBufferLevel
  , commandBufferCount :: Word32
  } deriving (Eq, Ord, Show)

unVkCommandBufferAllocateInfo ::
  MonadIO m =>
  VkCommandBufferAllocateInfo ->
  m Vk.VkCommandBufferAllocateInfo
unVkCommandBufferAllocateInfo a =
  liftIO . Vk.newVkData $ \ptr -> do
    Vk.writeField @"sType" ptr Vk.VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO
    Vk.writeField @"commandPool" ptr (commandPool a)
    Vk.writeField @"level" ptr (unVkCommandBufferLevel $ level a)
    Vk.writeField @"commandBufferCount" ptr (commandBufferCount a)

vkAllocateCommandBuffers ::
  MonadIO m =>
  Vk.VkDevice ->
  VkCommandBufferAllocateInfo ->
  m [Vk.VkCommandBuffer]
vkAllocateCommandBuffers d info = do
  info' <- unVkCommandBufferAllocateInfo info
  let count = (fromIntegral $ commandBufferCount info)
  liftIO . Foreign.allocaArray count $ \bPtr -> do
    vkResult =<< Vk.vkAllocateCommandBuffers d (Vk.unsafePtr info') bPtr
    Foreign.peekArray count bPtr

data VkCommandBufferUsageFlag
  = OneTimeSubmit
  | RenderPassContinue
  | SimultaneousUse
  deriving (Eq, Ord, Show)

unVkCommandBufferUsageBit ::
  VkCommandBufferUsageFlag ->
  Vk.VkCommandBufferUsageBitmask a
unVkCommandBufferUsageBit a =
  case a of
    OneTimeSubmit -> Vk.VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
    RenderPassContinue -> Vk.VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT
    SimultaneousUse -> Vk.VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT

unVkCommandBufferUsageBits ::
  [VkCommandBufferUsageFlag] ->
  Vk.VkCommandBufferUsageFlags
unVkCommandBufferUsageBits = foldr (\a b -> unVkCommandBufferUsageBit a .|. b) 0

data VkCommandBufferInheritanceInfo
  = VkCommandBufferInheritanceInfo
  { renderPass :: Vk.VkRenderPass
  , subpass :: Word32
  , framebuffer :: Vk.VkFramebuffer
  , occlusionQueryEnable :: Bool
  , queryFlags :: [VkQueryControlFlag]
  , pipelineStatistics :: [VkQueryPipelineStatisticFlag]
  } deriving (Eq, Ord, Show)

unVkCommandBufferInheritanceInfo ::
  MonadIO m =>
  VkCommandBufferInheritanceInfo ->
  m Vk.VkCommandBufferInheritanceInfo
unVkCommandBufferInheritanceInfo a =
  liftIO . Vk.newVkData $ \ptr -> do
    Vk.writeField @"sType" ptr Vk.VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO
    Vk.writeField @"renderPass" ptr (renderPass a)
    Vk.writeField @"subpass" ptr (subpass a)
    Vk.writeField @"framebuffer" ptr (framebuffer a)
    Vk.writeField @"occlusionQueryEnable" ptr (unVkBool32 $ occlusionQueryEnable a)
    Vk.writeField @"queryFlags" ptr (unVkQueryControlBits $ queryFlags a)
    Vk.writeField @"pipelineStatistics" ptr (unVkQueryPipelineStatisticBits $ pipelineStatistics a)

data VkCommandBufferBeginInfo
  = VkCommandBufferBeginInfo
  { flags :: [VkCommandBufferUsageFlag]
  , pInheritanceInfo :: Maybe VkCommandBufferInheritanceInfo
  } deriving (Eq, Ord, Show)

unVkCommandBufferBeginInfo ::
  MonadIO m =>
  VkCommandBufferBeginInfo ->
  m Vk.VkCommandBufferBeginInfo
unVkCommandBufferBeginInfo a =
  liftIO . Vk.newVkData $ \ptr -> do
    Vk.writeField @"sType" ptr Vk.VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO
    Vk.writeField @"flags" ptr (unVkCommandBufferUsageBits $ flags a)
    Vk.writeField @"pInheritanceInfo" ptr =<<
      maybe
        (pure Vk.VK_NULL_HANDLE)
        (fmap Vk.unsafePtr . unVkCommandBufferInheritanceInfo)
        (pInheritanceInfo a)

vkBeginCommandBuffer :: MonadIO m => Vk.VkCommandBuffer -> VkCommandBufferBeginInfo -> m ()
vkBeginCommandBuffer buf info =
  liftIO $
  vkResult =<<
  Vk.vkBeginCommandBuffer buf . Vk.unsafePtr =<<
  unVkCommandBufferBeginInfo info

vkEndCommandBuffer :: MonadIO m => Vk.VkCommandBuffer -> m ()
vkEndCommandBuffer buf = liftIO $ vkResult =<< Vk.vkEndCommandBuffer buf

withCommandBuffer :: MonadIO m => Vk.VkCommandBuffer -> VkCommandBufferBeginInfo -> m a -> m a
withCommandBuffer buf info m = vkBeginCommandBuffer buf info *> m <* vkEndCommandBuffer buf
