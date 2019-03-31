{-# language DataKinds, TypeApplications #-}
module Graphics.Vulkan.CommandPoolCreateInfo where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits ((.|.))
import Data.Word (Word32)
import Unsafe.Coerce (unsafeCoerce)

import qualified Graphics.Vulkan.Core_1_0 as Vk
import qualified Graphics.Vulkan.Core_1_1 as Vk

data VkCommandPoolCreateFlag
  = Transient
  | ResetCommandBuffer
  | Protected
  deriving (Eq, Ord, Show)

unVkCommandPoolCreateBit ::
  VkCommandPoolCreateFlag ->
  Vk.VkCommandPoolCreateBitmask a
unVkCommandPoolCreateBit a =
  case a of
    Transient -> Vk.VK_COMMAND_POOL_CREATE_TRANSIENT_BIT
    ResetCommandBuffer -> Vk.VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
    Protected -> unsafeCoerce Vk.VK_COMMAND_POOL_CREATE_PROTECTED_BIT

unVkCommandPoolCreateBits ::
  [VkCommandPoolCreateFlag] ->
  Vk.VkCommandPoolCreateFlags
unVkCommandPoolCreateBits = foldr (\a b -> unVkCommandPoolCreateBit a .|. b) 0

data VkCommandPoolCreateInfo
  = VkCommandPoolCreateInfo
  { flags :: [VkCommandPoolCreateFlag]
  , queueFamilyIndex :: Word32
  } deriving (Eq, Ord, Show)

unVkCommandPoolCreateInfo ::
  MonadIO m =>
  VkCommandPoolCreateInfo ->
  m Vk.VkCommandPoolCreateInfo
unVkCommandPoolCreateInfo a =
  liftIO . Vk.newVkData $ \ptr -> do
    Vk.writeField @"sType" ptr Vk.VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO
    Vk.writeField @"pNext" ptr Vk.VK_NULL
    Vk.writeField @"flags" ptr (unVkCommandPoolCreateBits $ flags a)
    Vk.writeField @"queueFamilyIndex" ptr (queueFamilyIndex a)
