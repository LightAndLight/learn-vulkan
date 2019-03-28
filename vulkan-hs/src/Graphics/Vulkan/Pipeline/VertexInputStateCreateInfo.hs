{-# language DataKinds, TypeApplications #-}
{-# language DuplicateRecordFields #-}
{-# language EmptyCase, EmptyDataDeriving #-}
module Graphics.Vulkan.Pipeline.VertexInputStateCreateInfo where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Word (Word32)

import qualified Foreign
import qualified Graphics.Vulkan.Core_1_0 as Vk
import qualified Graphics.Vulkan.Marshal as Vk

import Graphics.Vulkan.Format (VkFormat, vkFormat, unVkFormat)

data VkPipelineVertexInputStateCreateFlag
  deriving (Eq, Ord, Show)

vkPipelineVertexInputStateCreateBits ::
  Vk.VkPipelineVertexInputStateCreateFlags ->
  [VkPipelineVertexInputStateCreateFlag]
vkPipelineVertexInputStateCreateBits _ = []

unVkPipelineVertexInputStateCreateBits ::
  [VkPipelineVertexInputStateCreateFlag] ->
  Vk.VkPipelineVertexInputStateCreateFlags
unVkPipelineVertexInputStateCreateBits [] = 0
unVkPipelineVertexInputStateCreateBits (x:_) = case x of

data VkVertexInputRate
  = Vertex
  | Instance
  deriving (Eq, Ord, Show)

vkVertexInputRate ::
  Vk.VkVertexInputRate ->
  VkVertexInputRate
vkVertexInputRate a =
  case a of
    Vk.VK_VERTEX_INPUT_RATE_VERTEX -> Vertex
    Vk.VK_VERTEX_INPUT_RATE_INSTANCE -> Instance

unVkVertexInputRate ::
  VkVertexInputRate ->
  Vk.VkVertexInputRate
unVkVertexInputRate a =
  case a of
    Vertex -> Vk.VK_VERTEX_INPUT_RATE_VERTEX
    Instance -> Vk.VK_VERTEX_INPUT_RATE_INSTANCE

data VkVertexInputBindingDescription
  = VkVertexInputBindingDescription
  { binding :: Word32
  , stride :: Word32
  , inputRate :: VkVertexInputRate
  } deriving (Eq, Ord, Show)

vkVertexInputBindingDescription ::
  Vk.VkVertexInputBindingDescription ->
  VkVertexInputBindingDescription
vkVertexInputBindingDescription a =
  VkVertexInputBindingDescription
  { binding = Vk.getField @"binding" a
  , stride = Vk.getField @"stride" a
  , inputRate = vkVertexInputRate $ Vk.getField @"inputRate" a
  }

unVkVertexInputBindingDescription ::
  MonadIO m =>
  VkVertexInputBindingDescription ->
  m Vk.VkVertexInputBindingDescription
unVkVertexInputBindingDescription a =
  liftIO $
  Vk.newVkData $ \ptr -> do
    Vk.writeField @"binding" ptr (binding (a :: VkVertexInputBindingDescription))
    Vk.writeField @"stride" ptr (stride a)
    Vk.writeField @"inputRate" ptr (unVkVertexInputRate $ inputRate a)

data VkVertexInputAttributeDescription
  = VkVertexInputAttributeDescription
  { location :: Word32
  , binding :: Word32
  , format :: VkFormat
  , offset :: Word32
  } deriving (Eq, Ord, Show)

vkVertexInputAttributeDescription ::
  Vk.VkVertexInputAttributeDescription ->
  VkVertexInputAttributeDescription
vkVertexInputAttributeDescription a =
  VkVertexInputAttributeDescription
  { location = Vk.getField @"location" a
  , binding = Vk.getField @"binding" a
  , format = vkFormat $ Vk.getField @"format" a
  , offset = Vk.getField @"offset" a
  }

unVkVertexInputAttributeDescription ::
  MonadIO m =>
  VkVertexInputAttributeDescription ->
  m Vk.VkVertexInputAttributeDescription
unVkVertexInputAttributeDescription a =
  liftIO $
  Vk.newVkData $ \ptr -> do
    Vk.writeField @"location" ptr (location a)
    Vk.writeField @"binding" ptr (binding (a :: VkVertexInputAttributeDescription))
    Vk.writeField @"format" ptr (unVkFormat $ format a)
    Vk.writeField @"offset" ptr (offset a)

data VkPipelineVertexInputStateCreateInfo
  = VkPipelineVertexInputStateCreateInfo
  { flags :: [VkPipelineVertexInputStateCreateFlag]
  , pVertexBindingDescriptions :: [VkVertexInputBindingDescription]
  , pVertexAttributeDescriptions :: [VkVertexInputAttributeDescription]
  } deriving (Eq, Ord, Show)

vkPipelineVertexInputStateCreateInfo ::
  MonadIO m =>
  Vk.VkPipelineVertexInputStateCreateInfo ->
  m VkPipelineVertexInputStateCreateInfo
vkPipelineVertexInputStateCreateInfo a =
  liftIO $
  (\bs as ->
  VkPipelineVertexInputStateCreateInfo
  { flags = vkPipelineVertexInputStateCreateBits $ Vk.getField @"flags" a
  , pVertexBindingDescriptions = vkVertexInputBindingDescription <$> bs
  , pVertexAttributeDescriptions = vkVertexInputAttributeDescription <$> as
  }) <$>
  Foreign.peekArray
    (fromIntegral $ Vk.getField @"vertexBindingDescriptionCount" a)
    (Vk.getField @"pVertexBindingDescriptions" a) <*>
  Foreign.peekArray
    (fromIntegral $ Vk.getField @"vertexAttributeDescriptionCount" a)
    (Vk.getField @"pVertexAttributeDescriptions" a)

unVkPipelineVertexInputStateCreateInfo ::
  MonadIO m =>
  VkPipelineVertexInputStateCreateInfo ->
  m Vk.VkPipelineVertexInputStateCreateInfo
unVkPipelineVertexInputStateCreateInfo a =
  liftIO $ do
    bs <- traverse unVkVertexInputBindingDescription (pVertexBindingDescriptions a)
    as <- traverse unVkVertexInputAttributeDescription (pVertexAttributeDescriptions a)
    Foreign.withArray bs $ \bPtr ->
      Foreign.withArray as $ \aPtr ->
      Vk.newVkData $ \ptr -> do
        Vk.writeField @"sType" ptr Vk.VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO
        Vk.writeField @"flags" ptr (unVkPipelineVertexInputStateCreateBits $ flags a)
        Vk.writeField @"vertexBindingDescriptionCount" ptr bLen
        Vk.writeField @"pVertexBindingDescriptions" ptr bPtr
        Vk.writeField @"vertexAttributeDescriptionCount" ptr aLen
        Vk.writeField @"pVertexAttributeDescriptions" ptr aPtr
  where
    bLen = fromIntegral . length $ pVertexBindingDescriptions a
    aLen = fromIntegral . length $ pVertexAttributeDescriptions a
