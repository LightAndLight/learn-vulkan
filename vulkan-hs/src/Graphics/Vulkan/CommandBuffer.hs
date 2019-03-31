{-# language DataKinds, TypeApplications #-}
module Graphics.Vulkan.CommandBuffer
  ( Vk.VkCommandBuffer
  , VkCommandBufferLevel(..)
  , VkCommandBufferAllocateInfo(..)
  , vkAllocateCommandBuffers
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits ((.|.))
import Data.Word (Word32)

import qualified Foreign
import qualified Graphics.Vulkan.Core_1_0 as Vk

import Graphics.Vulkan.Result (vkResult)

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
