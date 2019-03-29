{-# language BangPatterns #-}
{-# language DataKinds, GADTs, KindSignatures, TypeOperators #-}
{-# language EmptyCase, EmptyDataDeriving #-}
{-# language FlexibleContexts, FlexibleInstances #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language TypeApplications #-}
{-# language UndecidableInstances #-}
module Graphics.Vulkan.Pipeline.ShaderStageCreateInfo where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits ((.&.), (.|.))
import Data.Void (Void)
import Data.Word (Word32)

import qualified Foreign
import qualified Foreign.C.String as Foreign
import qualified Foreign.C.Types as Foreign
import qualified Graphics.Vulkan.Core_1_0 as Vk

import Data.Constraint.All (All)
import Data.Type.Function (EqSym0, OrdSym0, ShowSym0)
import Graphics.Vulkan.ShaderStage

data VkPipelineShaderStageCreateFlag
  deriving (Eq, Ord, Show)

vkPipelineShaderStageCreateBits ::
  Vk.VkPipelineShaderStageCreateFlags ->
  [VkPipelineShaderStageCreateFlag]
vkPipelineShaderStageCreateBits _ = []

unVkPipelineShaderStageCreateBits ::
  [VkPipelineShaderStageCreateFlag] ->
  Vk.VkPipelineShaderStageCreateFlags
unVkPipelineShaderStageCreateBits [] = 0
unVkPipelineShaderStageCreateBits (x:_) = case x of

data VkSpecializationInfo :: [*] -> * where
  Nil :: VkSpecializationInfo '[]
  Cons ::
    Foreign.Storable t =>
    Word32 ->
    t ->
    VkSpecializationInfo ts ->
    VkSpecializationInfo (t ': ts)
deriving instance All EqSym0 ts => Eq (VkSpecializationInfo ts)
deriving instance (All EqSym0 ts, All OrdSym0 ts) => Ord (VkSpecializationInfo ts)
deriving instance All ShowSym0 ts => Show (VkSpecializationInfo ts)

specializationInfoCount :: VkSpecializationInfo a -> Word32
specializationInfoCount = go 0
  where
    go :: Word32 -> VkSpecializationInfo a -> Word32
    go !n Nil = n
    go !n (Cons _ _ a) = go (n+1) a

specializationInfoSize :: VkSpecializationInfo a -> Foreign.CSize
specializationInfoSize = go 0
  where
    go :: Foreign.CSize -> VkSpecializationInfo a -> Foreign.CSize
    go !n Nil = 0
    go !n (Cons _ a b) = go (n + fromIntegral (Foreign.sizeOf a)) b

withSpecializationInfoEntries ::
  forall a b.
  Word32 ->
  VkSpecializationInfo a ->
  (Foreign.Ptr Vk.VkSpecializationMapEntry -> IO b) ->
  IO b
withSpecializationInfoEntries count si f = do
  (ptr, _) <- Vk.mallocVkDataArray (fromIntegral count)
  go 0 si ptr
  where
    go ::
      forall x.
      Word32 -> -- offset into the array, in bytes
      VkSpecializationInfo x ->
      Foreign.Ptr Vk.VkSpecializationMapEntry ->
      IO b
    go !offset Nil !ptr = f ptr
    go !offset (Cons cid a b) !ptr = do
      let aSize = Foreign.sizeOf a
      Vk.writeField @"constantID" ptr cid
      Vk.writeField @"offset" ptr offset
      Vk.writeField @"size" ptr (fromIntegral aSize)
      go (offset + fromIntegral aSize) b (Foreign.plusPtr ptr aSize)

withSpecializationInfoData ::
  forall a b.
  Foreign.CSize ->
  VkSpecializationInfo a ->
  (Foreign.Ptr Void -> IO b) ->
  IO b
withSpecializationInfoData bufSize si f = do
  Foreign.allocaBytes (fromIntegral bufSize) $ go si
  where
    go ::
      forall x.
      VkSpecializationInfo x ->
      Foreign.Ptr Void ->
      IO b
    go Nil !ptr = f ptr
    go (Cons cid a b) !ptr = do
      let aSize = Foreign.sizeOf a
      Foreign.poke (Foreign.castPtr ptr) a
      go b (Foreign.plusPtr ptr aSize)

unVkSpecializationInfo ::
  MonadIO m =>
  VkSpecializationInfo a ->
  m Vk.VkSpecializationInfo
unVkSpecializationInfo si =
  liftIO $
  withSpecializationInfoEntries entryCount si $ \entriesPtr ->
  withSpecializationInfoData bufSize si $ \dataPtr ->
  Vk.newVkData $ \siPtr -> do
    Vk.writeField @"mapEntryCount" siPtr entryCount
    Vk.writeField @"pMapEntries" siPtr entriesPtr
    Vk.writeField @"dataSize" siPtr bufSize
    Vk.writeField @"pData" siPtr dataPtr
  where
    entryCount = specializationInfoCount si
    bufSize = specializationInfoSize si

data VkPipelineShaderStageCreateInfo ts
  = VkPipelineShaderStageCreateInfo
  { flags :: [VkPipelineShaderStageCreateFlag]
  , stage :: VkShaderStageFlag
  , module_ :: Vk.VkShaderModule
  , pName :: String
  , pSpecializationInfo :: Maybe (VkSpecializationInfo ts)
  }
deriving instance All EqSym0 ts => Eq (VkPipelineShaderStageCreateInfo ts)
deriving instance (All EqSym0 ts, All OrdSym0 ts) => Ord (VkPipelineShaderStageCreateInfo ts)
deriving instance All ShowSym0 ts => Show (VkPipelineShaderStageCreateInfo ts)

unVkPipelineShaderStageCreateInfo ::
  MonadIO m =>
  VkPipelineShaderStageCreateInfo ts ->
  m Vk.VkPipelineShaderStageCreateInfo
unVkPipelineShaderStageCreateInfo info =
  liftIO $
  Foreign.withCString (pName info) $ \strPtr -> do
  specInfo <-
    maybe
      (pure Vk.VK_NULL_HANDLE)
      (fmap Vk.unsafePtr . unVkSpecializationInfo)
      (pSpecializationInfo info)
  Vk.newVkData $ \ptr -> do
    Vk.writeField @"sType" ptr Vk.VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
    Vk.writeField @"flags" ptr (unVkPipelineShaderStageCreateBits $ flags info)
    Vk.writeField @"stage" ptr (unVkShaderStageBit $ stage info)
    Vk.writeField @"module" ptr (module_ info)
    Vk.writeField @"pName" ptr strPtr
    Vk.writeField @"pSpecializationInfo" ptr specInfo
