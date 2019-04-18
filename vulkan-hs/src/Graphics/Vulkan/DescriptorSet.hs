{-# language DataKinds, TypeApplications #-}
{-# language DuplicateRecordFields #-}
module Graphics.Vulkan.DescriptorSet
  ( Vk.VkDescriptorSet
  , vkAllocateDescriptorSets
  , vkUpdateDescriptorSets
  , VkCopyDescriptorSet(..), unVkCopyDescriptorSet
  , VkWriteDescriptorSet(..), unVkWriteDescriptorSet
  , VkDescriptorBufferInfo(..), unVkDescriptorBufferInfo
  , VkDescriptorImageInfo(..), unVkDescriptorImageInfo
  , VkDescriptorSetAllocateInfo(..), unVkDescriptorSetAllocateInfo
  )
where

import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Managed.Safe (MonadManaged, using, managed)
import Data.Word (Word32)

import qualified Foreign
import qualified Graphics.Vulkan.Core_1_0 as Vk
import qualified Graphics.Vulkan.Marshal as Vk

import Graphics.Vulkan.DescriptorType (VkDescriptorType, unVkDescriptorType)
import Graphics.Vulkan.ImageLayout (VkImageLayout, unVkImageLayout)
import Graphics.Vulkan.Result (vkResult)

data VkDescriptorSetAllocateInfo
  = VkDescriptorSetAllocateInfo
  { descriptorPool :: Vk.VkDescriptorPool
  , pSetLayouts :: [Vk.VkDescriptorSetLayout]
  } deriving (Eq, Ord, Show)

unVkDescriptorSetAllocateInfo ::
  MonadIO m =>
  VkDescriptorSetAllocateInfo ->
  m Vk.VkDescriptorSetAllocateInfo
unVkDescriptorSetAllocateInfo a =
  liftIO . Foreign.withArray (pSetLayouts a) $ \lPtr ->
  Vk.newVkData $ \ptr -> do
    Vk.writeField @"sType" ptr Vk.VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO
    Vk.writeField @"pNext" ptr Foreign.nullPtr
    Vk.writeField @"descriptorPool" ptr (descriptorPool a)
    Vk.writeField @"descriptorSetCount" ptr (fromIntegral . length $ pSetLayouts a)
    Vk.writeField @"pSetLayouts" ptr lPtr

vkAllocateDescriptorSets ::
  MonadManaged m =>
  Vk.VkDevice ->
  VkDescriptorSetAllocateInfo ->
  m [Vk.VkDescriptorSet]
vkAllocateDescriptorSets dev info = do
  let size = length $ pSetLayouts info
  sPtr <- using $ managed (Foreign.allocaArray size)
  info' <- unVkDescriptorSetAllocateInfo info
  liftIO $ vkResult =<< Vk.vkAllocateDescriptorSets dev (Vk.unsafePtr info') sPtr
  using $ managed (bracket
    (Foreign.peekArray size sPtr)
    (\_ -> Vk.vkFreeDescriptorSets dev (descriptorPool info) (fromIntegral size) sPtr))

data VkDescriptorImageInfo
  = VkDescriptorImageInfo
  { sampler :: Vk.VkSampler
  , imageView :: Vk.VkImageView
  , imageLayout :: VkImageLayout
  } deriving (Eq, Ord, Show)

unVkDescriptorImageInfo ::
  MonadIO m =>
  VkDescriptorImageInfo ->
  m Vk.VkDescriptorImageInfo
unVkDescriptorImageInfo a =
  liftIO . Vk.newVkData $ \ptr -> do
    Vk.writeField @"sampler" ptr (sampler a)
    Vk.writeField @"imageView" ptr (imageView a)
    Vk.writeField @"imageLayout" ptr (unVkImageLayout $ imageLayout a)

data VkDescriptorBufferInfo
  = VkDescriptorBufferInfo
  { buffer :: Vk.VkBuffer
  , offset :: Vk.VkDeviceSize
  , range :: Vk.VkDeviceSize
  } deriving (Eq, Ord, Show)

unVkDescriptorBufferInfo ::
  MonadIO m =>
  VkDescriptorBufferInfo ->
  m Vk.VkDescriptorBufferInfo
unVkDescriptorBufferInfo a =
  liftIO . Vk.newVkData $ \ptr -> do
    Vk.writeField @"buffer" ptr (buffer a)
    Vk.writeField @"offset" ptr (offset a)
    Vk.writeField @"range" ptr (range a)

data VkWriteDescriptorSet
  = VkWriteDescriptorSet
  { dstSet :: Vk.VkDescriptorSet
  , dstBinding :: Word32
  , dstArrayElement :: Word32
  , descriptorCount :: Word32
  , descriptorType :: VkDescriptorType
  , pImageInfo :: Maybe [VkDescriptorImageInfo]
  , pBufferInfo :: Maybe [VkDescriptorBufferInfo]
  , pTexelBufferView :: Maybe [Vk.VkBufferView]
  } deriving (Eq, Ord, Show)

unVkWriteDescriptorSet ::
  MonadIO m =>
  VkWriteDescriptorSet ->
  m Vk.VkWriteDescriptorSet
unVkWriteDescriptorSet a =
  case pImageInfo a of
    Nothing -> liftIO $ go1 Foreign.nullPtr
    Just as -> do
      as' <- traverse unVkDescriptorImageInfo as
      liftIO $ Foreign.withArray as' go1
  where
    go1 aPtr =
      case pBufferInfo a of
        Nothing -> go2 aPtr Foreign.nullPtr
        Just bs -> do
          bs' <- traverse unVkDescriptorBufferInfo bs
          liftIO $ Foreign.withArray bs' (go2 aPtr)

    go2 aPtr bPtr =
      case pTexelBufferView a of
        Nothing -> go3 aPtr bPtr Foreign.nullPtr
        Just cs -> liftIO $ Foreign.withArray cs (go3 aPtr bPtr)

    go3 ::
      Foreign.Ptr Vk.VkDescriptorImageInfo ->
      Foreign.Ptr Vk.VkDescriptorBufferInfo ->
      Foreign.Ptr Vk.VkBufferView ->
      IO Vk.VkWriteDescriptorSet
    go3 aPtr bPtr cPtr =
      Vk.newVkData $ \ptr -> do
        Vk.writeField @"sType" ptr Vk.VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET
        Vk.writeField @"pNext" ptr Foreign.nullPtr
        Vk.writeField @"dstSet" ptr (dstSet (a :: VkWriteDescriptorSet))
        Vk.writeField @"dstBinding" ptr (dstBinding (a :: VkWriteDescriptorSet))
        Vk.writeField @"dstArrayElement" ptr (dstArrayElement (a :: VkWriteDescriptorSet))
        Vk.writeField @"descriptorCount" ptr (descriptorCount (a :: VkWriteDescriptorSet))
        Vk.writeField @"descriptorType" ptr (unVkDescriptorType $ descriptorType a)
        Vk.writeField @"pImageInfo" ptr aPtr
        Vk.writeField @"pBufferInfo" ptr bPtr
        Vk.writeField @"pTexelBufferView" ptr cPtr

data VkCopyDescriptorSet
  = VkCopyDescriptorSet
  { srcSet :: Vk.VkDescriptorSet
  , srcBinding :: Word32
  , srcArrayElement :: Word32
  , dstSet :: Vk.VkDescriptorSet
  , dstBinding :: Word32
  , dstArrayElement :: Word32
  , descriptorCount :: Word32
  } deriving (Eq, Ord, Show)

unVkCopyDescriptorSet ::
  MonadIO m =>
  VkCopyDescriptorSet ->
  m Vk.VkCopyDescriptorSet
unVkCopyDescriptorSet a =
  liftIO . Vk.newVkData $ \ptr -> do
    Vk.writeField @"sType" ptr Vk.VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET
    Vk.writeField @"pNext" ptr Foreign.nullPtr
    Vk.writeField @"srcSet" ptr (srcSet a)
    Vk.writeField @"srcBinding" ptr (srcBinding a)
    Vk.writeField @"srcArrayElement" ptr (srcArrayElement a)
    Vk.writeField @"dstSet" ptr (dstSet (a :: VkCopyDescriptorSet))
    Vk.writeField @"dstBinding" ptr (dstBinding (a :: VkCopyDescriptorSet))
    Vk.writeField @"dstArrayElement" ptr (dstArrayElement (a :: VkCopyDescriptorSet))
    Vk.writeField @"descriptorCount" ptr (descriptorCount (a :: VkCopyDescriptorSet))

vkUpdateDescriptorSets ::
  MonadIO m =>
  Vk.VkDevice ->
  [VkWriteDescriptorSet] ->
  [VkCopyDescriptorSet] ->
  m ()
vkUpdateDescriptorSets dev ws cs =
  liftIO $ do
    ws' <- traverse unVkWriteDescriptorSet ws
    cs' <- traverse unVkCopyDescriptorSet cs
    Foreign.withArray ws' $ \wPtr ->
      Foreign.withArray cs' $ \cPtr ->
      Vk.vkUpdateDescriptorSets dev (fromIntegral $ length ws) wPtr (fromIntegral $ length cs) cPtr
