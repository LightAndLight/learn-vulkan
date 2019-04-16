{-# language DataKinds, TypeApplications #-}
module Graphics.Vulkan.DescriptorSet
  ( Vk.VkDescriptorSet
  , vkAllocateDescriptorSets
  , VkDescriptorSetAllocateInfo(..), unVkDescriptorSetAllocateInfo
  )
where

import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Managed.Safe (MonadManaged, using, managed)

import qualified Foreign
import qualified Graphics.Vulkan.Core_1_0 as Vk
import qualified Graphics.Vulkan.Marshal as Vk

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
    Nothing -> go1 Foreign.nullPtr
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

    go3 aPtr bPtr cPtr =
      Vk.newVkData $ \ptr -> do
        Vk.writeField @"sType" ptr Vk.VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET
        Vk.writeField @"pNext" ptr Foreign.nullPtr
        Vk.writeField @"dstSet" ptr (dstSet a)
        Vk.writeField @"dstBinding" ptr (dstBinding a)
        Vk.writeField @"dstArrayElement" ptr (dstArrayElement a)
        Vk.writeField @"descriptorCount" ptr (descriptorCount a)
        Vk.writeField @"descriptorType" ptr (unVkDescriptorType $ descriptorType a)

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
    Foreign.newArray ws' $ \wPtr ->
      Foreign.newArray cs $ \cPtr ->
      Vk.vkUpdateDescriptorSets dev (fromIntegral $ length ws) wPtr (fromIntegral $ length cs) cPtr
