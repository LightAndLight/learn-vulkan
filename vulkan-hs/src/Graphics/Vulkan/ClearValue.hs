{-# language DataKinds, TypeApplications #-}
module Graphics.Vulkan.ClearValue where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Int (Int32)
import Data.Word (Word32)

import qualified Graphics.Vulkan.Core_1_0 as Vk
import qualified Graphics.Vulkan.Marshal as Vk

data VkClearColorValue
  = Float32 Float Float Float Float
  | Int32 Int32 Int32 Int32 Int32
  | Uint32 Word32 Word32 Word32 Word32
  deriving (Eq, Ord, Show)

unVkClearColorValue ::
  MonadIO m =>
  VkClearColorValue ->
  m Vk.VkClearColorValue
unVkClearColorValue a =
  liftIO . Vk.newVkData $ \ptr -> do
  case a of
    Float32 f0 f1 f2 f3 -> do
      Vk.writeFieldArray @"float32" @0 ptr f0
      Vk.writeFieldArray @"float32" @1 ptr f1
      Vk.writeFieldArray @"float32" @2 ptr f2
      Vk.writeFieldArray @"float32" @3 ptr f3
    Int32 i0 i1 i2 i3 -> do
      Vk.writeFieldArray @"int32" @0 ptr i0
      Vk.writeFieldArray @"int32" @1 ptr i1
      Vk.writeFieldArray @"int32" @2 ptr i2
      Vk.writeFieldArray @"int32" @3 ptr i3
    Uint32 ui0 ui1 ui2 ui3 -> do
      Vk.writeFieldArray @"uint32" @0 ptr ui0
      Vk.writeFieldArray @"uint32" @1 ptr ui1
      Vk.writeFieldArray @"uint32" @2 ptr ui2
      Vk.writeFieldArray @"uint32" @3 ptr ui3

data VkClearDepthStencilValue
  = VkClearDepthStencilValue
  { depth :: Float
  , stencil :: Word32
  } deriving (Eq, Ord, Show)

unVkClearDepthStencilValue ::
  MonadIO m =>
  VkClearDepthStencilValue ->
  m Vk.VkClearDepthStencilValue
unVkClearDepthStencilValue a =
  liftIO . Vk.newVkData $ \ptr -> do
    Vk.writeField @"depth" ptr (depth a)
    Vk.writeField @"stencil" ptr (stencil a)

data VkClearValue
  = Color VkClearColorValue
  | DepthStencil VkClearDepthStencilValue
  deriving (Eq, Ord, Show)

unVkClearValue ::
  MonadIO m =>
  VkClearValue ->
  m Vk.VkClearValue
unVkClearValue a =
  liftIO . Vk.newVkData $ \ptr ->
  case a of
    Color aa -> Vk.writeField @"color" ptr =<< unVkClearColorValue aa
    DepthStencil aa -> Vk.writeField @"depthStencil" ptr =<< unVkClearDepthStencilValue aa
