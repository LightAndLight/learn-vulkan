{-# language DataKinds, TypeApplications #-}
module Graphics.Vulkan.Fence
  ( Vk.VkFence
  , VkFenceCreateFlag(..), unVkFenceCreateBit, unVkFenceCreateBits
  , VkFenceCreateInfo(..), unVkFenceCreateInfo
  , vkCreateFence
  , vkWaitForFences
  , vkResetFences
  )
where

import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Managed.Safe (MonadManaged, using, managed)
import Data.Bits ((.|.))
import Data.Word (Word64)

import qualified Foreign
import qualified Graphics.Vulkan.Core_1_0 as Vk

import Graphics.Vulkan.Result (vkResult)
import Graphics.Vulkan.Utils (unVkBool32)

data VkFenceCreateFlag
  = Signaled
  deriving (Eq, Ord, Show)

unVkFenceCreateBit :: VkFenceCreateFlag -> Vk.VkFenceCreateBitmask a
unVkFenceCreateBit Signaled = Vk.VK_FENCE_CREATE_SIGNALED_BIT

unVkFenceCreateBits :: [VkFenceCreateFlag] -> Vk.VkFenceCreateFlags
unVkFenceCreateBits = foldr (\a b -> unVkFenceCreateBit a .|. b) 0

data VkFenceCreateInfo
  = VkFenceCreateInfo
  { flags :: [VkFenceCreateFlag]
  } deriving (Eq, Ord, Show)

unVkFenceCreateInfo :: MonadIO m => VkFenceCreateInfo -> m Vk.VkFenceCreateInfo
unVkFenceCreateInfo info =
  liftIO . Vk.newVkData $ \ptr -> do
    Vk.writeField @"sType" ptr Vk.VK_STRUCTURE_TYPE_FENCE_CREATE_INFO
    Vk.writeField @"pNext" ptr Foreign.nullPtr
    Vk.writeField @"flags" ptr $ unVkFenceCreateBits (flags info)

vkCreateFence ::
  MonadManaged m =>
  Vk.VkDevice ->
  VkFenceCreateInfo ->
  Foreign.Ptr Vk.VkAllocationCallbacks ->
  m Vk.VkFence
vkCreateFence dev info cbs = do
  info' <- unVkFenceCreateInfo info
  fencePtr <- using $ managed Foreign.alloca
  liftIO $ vkResult =<< Vk.vkCreateFence dev (Vk.unsafePtr info') cbs fencePtr
  using $ managed (bracket (Foreign.peek fencePtr) (\f -> Vk.vkDestroyFence dev f cbs))

vkWaitForFences ::
  MonadIO m =>
  Vk.VkDevice ->
  [Vk.VkFence] ->
  Bool ->
  Word64 ->
  m ()
vkWaitForFences dev fs b time =
  liftIO . Foreign.withArray fs $ \fsPtr ->
    vkResult =<< Vk.vkWaitForFences dev (fromIntegral $ length fs) fsPtr (unVkBool32 b) time

vkResetFences ::
  MonadIO m =>
  Vk.VkDevice ->
  [Vk.VkFence] ->
  m ()
vkResetFences dev fs =
  liftIO . Foreign.withArray fs $ \fsPtr ->
    vkResult =<< Vk.vkResetFences dev (fromIntegral $ length fs) fsPtr
