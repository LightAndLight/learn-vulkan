{-# language DataKinds, TypeApplications #-}
{-# language EmptyCase, EmptyDataDeriving #-}
module Graphics.Vulkan.Semaphore
  ( Vk.VkSemaphore
  , VkSemaphoreCreateInfo(..)
  , unVkSemaphoreCreateInfo
  , vkCreateSemaphore
  )
where

import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Managed.Safe (MonadManaged, using, managed)

import qualified Foreign
import qualified Graphics.Vulkan.Core_1_0 as Vk

import Graphics.Vulkan.Result (vkResult)

data VkSemaphoreCreateFlag
  deriving (Eq, Ord, Show)

unVkSemaphoreCreateBits ::
  [VkSemaphoreCreateFlag] ->
  Vk.VkSemaphoreCreateFlags
unVkSemaphoreCreateBits [] = 0
unVkSemaphoreCreateBits (x:_) = case x of

data VkSemaphoreCreateInfo
  = VkSemaphoreCreateInfo
  { flags :: [VkSemaphoreCreateFlag]
  } deriving (Eq, Ord, Show)

unVkSemaphoreCreateInfo ::
  MonadIO m =>
  VkSemaphoreCreateInfo ->
  m Vk.VkSemaphoreCreateInfo
unVkSemaphoreCreateInfo a =
  liftIO . Vk.newVkData $ \ptr -> do
    Vk.writeField @"sType" ptr Vk.VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO
    Vk.writeField @"pNext" ptr Vk.VK_NULL
    Vk.writeField @"flags" ptr (unVkSemaphoreCreateBits $ flags a)

vkCreateSemaphore ::
  (MonadManaged m, MonadIO m) =>
  Vk.VkDevice ->
  VkSemaphoreCreateInfo ->
  Foreign.Ptr Vk.VkAllocationCallbacks ->
  m Vk.VkSemaphore
vkCreateSemaphore d info cbs = do
  info' <- unVkSemaphoreCreateInfo info
  sPtr <- using $ managed Foreign.alloca
  liftIO $ vkResult =<< Vk.vkCreateSemaphore d (Vk.unsafePtr info') cbs sPtr
  using $ managed (bracket (Foreign.peek sPtr) (\s -> Vk.vkDestroySemaphore d s cbs))
