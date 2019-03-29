{-# language DataKinds, TypeApplications #-}
{-# language DuplicateRecordFields #-}
{-# language EmptyCase, EmptyDataDeriving #-}
module Graphics.Vulkan.RenderPassCreateInfo where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits ((.|.))
import Data.Maybe (fromMaybe)
import Data.Word (Word32)
import Unsafe.Coerce (unsafeCoerce)

import qualified Foreign
import qualified Graphics.Vulkan.Constants as Vk
import qualified Graphics.Vulkan.Core_1_0 as Vk
-- import qualified Graphics.Vulkan.Ext.VK_NV_ray_tracing as Vk
import qualified Graphics.Vulkan.Ext.VK_NVX_multiview_per_view_attributes as Vk
import qualified Graphics.Vulkan.Marshal as Vk

import Graphics.Vulkan.Access (VkAccessFlag, unVkAccessBits)
import Graphics.Vulkan.Dependency (VkDependencyFlag, unVkDependencyBits)
import Graphics.Vulkan.Format (VkFormat, unVkFormat)
import Graphics.Vulkan.ImageLayout (VkImageLayout, unVkImageLayout)
import Graphics.Vulkan.PipelineStage (VkPipelineStageFlag, unVkPipelineStageBits)
import Graphics.Vulkan.SampleCount (VkSampleCount, unVkSampleCountBit)

data VkRenderPassCreateFlag
  deriving (Eq, Ord, Show)

vkRenderPassCreateBits ::
  Vk.VkRenderPassCreateFlags ->
  [VkRenderPassCreateFlag]
vkRenderPassCreateBits _ = []

unVkRenderPassCreateBits ::
  [VkRenderPassCreateFlag] ->
  Vk.VkRenderPassCreateFlags
unVkRenderPassCreateBits [] = 0
unVkRenderPassCreateBits (x:_) = case x of

data VkAttachmentDescriptionFlag
  = MayAliasBit
  deriving (Eq, Ord, Show)

unVkAttachmentDescriptionBit ::
  VkAttachmentDescriptionFlag ->
  Vk.VkAttachmentDescriptionBitmask a
unVkAttachmentDescriptionBit a =
  case a of
    MayAliasBit -> Vk.VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT

unVkAttachmentDescriptionBits ::
  [VkAttachmentDescriptionFlag] ->
  Vk.VkAttachmentDescriptionFlags
unVkAttachmentDescriptionBits = foldr (\a b -> unVkAttachmentDescriptionBit a .|. b) 0

data VkAttachmentLoadOp
  = Load
  | Clear
  | LoadOpDontCare
  deriving (Eq, Ord, Show)

unVkAttachmentLoadOp :: VkAttachmentLoadOp -> Vk.VkAttachmentLoadOp
unVkAttachmentLoadOp a =
  case a of
    Load -> Vk.VK_ATTACHMENT_LOAD_OP_LOAD
    Clear -> Vk.VK_ATTACHMENT_LOAD_OP_CLEAR
    LoadOpDontCare -> Vk.VK_ATTACHMENT_LOAD_OP_DONT_CARE

data VkAttachmentStoreOp
  = Store
  | StoreOpDontCare
  deriving (Eq, Ord, Show)

unVkAttachmentStoreOp :: VkAttachmentStoreOp -> Vk.VkAttachmentStoreOp
unVkAttachmentStoreOp a =
  case a of
    Store -> Vk.VK_ATTACHMENT_STORE_OP_STORE
    StoreOpDontCare -> Vk.VK_ATTACHMENT_STORE_OP_DONT_CARE

data VkAttachmentDescription
  = VkAttachmentDescription
  { flags :: [VkAttachmentDescriptionFlag]
  , format :: VkFormat
  , samples :: VkSampleCount
  , loadOp :: VkAttachmentLoadOp
  , storeOp :: VkAttachmentStoreOp
  , stencilLoadOp :: VkAttachmentLoadOp
  , stencilStoreOp :: VkAttachmentStoreOp
  , initialLayout :: VkImageLayout
  , finalLayout :: VkImageLayout
  } deriving (Eq, Ord, Show)

unVkAttachmentDescription ::
  MonadIO m =>
  VkAttachmentDescription ->
  m Vk.VkAttachmentDescription
unVkAttachmentDescription a =
  liftIO . Vk.newVkData $ \ptr -> do
    Vk.writeField @"flags" ptr
      (unVkAttachmentDescriptionBits $ flags (a :: VkAttachmentDescription))
    Vk.writeField @"format" ptr (unVkFormat $ format a)
    Vk.writeField @"samples" ptr (unVkSampleCountBit $ samples a)
    Vk.writeField @"loadOp" ptr (unVkAttachmentLoadOp $ loadOp a)
    Vk.writeField @"storeOp" ptr (unVkAttachmentStoreOp $ storeOp a)
    Vk.writeField @"stencilLoadOp" ptr (unVkAttachmentLoadOp $ loadOp a)
    Vk.writeField @"stencilStoreOp" ptr (unVkAttachmentStoreOp $ storeOp a)
    Vk.writeField @"initialLayout" ptr (unVkImageLayout $ initialLayout a)
    Vk.writeField @"finalLayout" ptr (unVkImageLayout $ finalLayout a)

data VkSubpassDescriptionFlag
  = PerViewAttributesNVX
  | PerViewPositionXOnlyNVX
  deriving (Eq, Ord, Show)

unVkSubpassDescriptionBit ::
  VkSubpassDescriptionFlag ->
  Vk.VkSubpassDescriptionBitmask a
unVkSubpassDescriptionBit a =
  case a of
    PerViewAttributesNVX -> unsafeCoerce Vk.VK_SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX
    PerViewPositionXOnlyNVX ->
      unsafeCoerce Vk.VK_SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX

unVkSubpassDescriptionBits ::
  [VkSubpassDescriptionFlag] ->
  Vk.VkSubpassDescriptionFlags
unVkSubpassDescriptionBits = foldr (\a b -> unVkSubpassDescriptionBit a .|. b) 0

data VkPipelineBindPoint
  = Graphics
  | Compute
  -- RayTracingNV
  deriving (Eq, Ord, Show)

unVkPipelineBindPoint ::
  VkPipelineBindPoint ->
  Vk.VkPipelineBindPoint
unVkPipelineBindPoint a =
  case a of
    Graphics -> Vk.VK_PIPELINE_BIND_POINT_GRAPHICS
    Compute -> Vk.VK_PIPELINE_BIND_POINT_COMPUTE
    -- RayTracingNV -> Vk.VK_PIPELINE_BIND_POINT_RAY_TRACING_NV

data VkAttachmentReference
  = VkAttachmentReference
  { attachment :: Maybe Word32
  , layout :: VkImageLayout
  } deriving (Eq, Ord, Show)

unVkAttachmentReference ::
  MonadIO m =>
  VkAttachmentReference ->
  m Vk.VkAttachmentReference
unVkAttachmentReference a =
  liftIO . Vk.newVkData $ \ptr -> do
    Vk.writeField @"attachment" ptr
      (fromMaybe Vk.VK_ATTACHMENT_UNUSED $ attachment a)
    Vk.writeField @"layout" ptr (unVkImageLayout $ layout a)

data VkSubpassDescription
  = VkSubpassDescription
  { flags :: [VkSubpassDescriptionFlag]
  , pipelineBindPoint :: VkPipelineBindPoint
  , pInputAttachments :: [VkAttachmentReference]
  , pColorAttachments :: [VkAttachmentReference]
  , pResolveAttachments :: Maybe [VkAttachmentReference]
  , pDepthStencilAttachment :: Maybe VkAttachmentReference
  , pPreserveAttachments :: [Word32]
  } deriving (Eq, Ord, Show)

unVkSubpassDescription ::
  MonadIO m =>
  VkSubpassDescription ->
  m Vk.VkSubpassDescription
unVkSubpassDescription a = do
  inputs <- traverse unVkAttachmentReference (pInputAttachments a)
  colors <- traverse unVkAttachmentReference (pColorAttachments a)
  mresolves <- (traverse.traverse) unVkAttachmentReference (pResolveAttachments a)
  dPtr <-
    maybe
      (pure Vk.VK_NULL_HANDLE)
      (fmap Vk.unsafePtr . unVkAttachmentReference)
      (pDepthStencilAttachment a)
  liftIO . Foreign.withArray inputs $ \inputsPtr ->
    Foreign.withArray colors $ \colorsPtr ->
    case mresolves of
      Nothing -> go inputsPtr colorsPtr dPtr Foreign.nullPtr
      Just resolves -> Foreign.withArray resolves (go inputsPtr colorsPtr dPtr)
  where
    go ::
      Foreign.Ptr Vk.VkAttachmentReference ->
      Foreign.Ptr Vk.VkAttachmentReference ->
      Foreign.Ptr Vk.VkAttachmentReference ->
      Foreign.Ptr Vk.VkAttachmentReference ->
      IO Vk.VkSubpassDescription
    go inputsPtr colorsPtr dPtr resolvesPtr =
      Foreign.withArray (pPreserveAttachments a) $ \preservesPtr ->
      Vk.newVkData $ \ptr -> do
        Vk.writeField @"flags" ptr (unVkSubpassDescriptionBits $ flags (a :: VkSubpassDescription))
        Vk.writeField @"pipelineBindPoint" ptr (unVkPipelineBindPoint $ pipelineBindPoint a)
        Vk.writeField @"inputAttachmentCount" ptr (fromIntegral . length $ pInputAttachments a)
        Vk.writeField @"pInputAttachments" ptr inputsPtr
        Vk.writeField @"colorAttachmentCount" ptr (fromIntegral . length $ pColorAttachments a)
        Vk.writeField @"pColorAttachments" ptr colorsPtr
        Vk.writeField @"pResolveAttachments" ptr resolvesPtr
        Vk.writeField @"pDepthStencilAttachment" ptr dPtr
        Vk.writeField @"preserveAttachmentCount" ptr
          (fromIntegral . length $ pPreserveAttachments a)
        Vk.writeField @"pPreserveAttachments" ptr preservesPtr

data VkSubpassDependency
  = VkSubpassDependency
  { srcSubpass :: Word32
  , dstSubpass :: Word32
  , srcStageMask :: [VkPipelineStageFlag]
  , dstStageMask :: [VkPipelineStageFlag]
  , srcAccessMask :: [VkAccessFlag]
  , dstAccessMask :: [VkAccessFlag]
  , dependencyFlags :: [VkDependencyFlag]
  } deriving (Eq, Ord, Show)

unVkSubpassDependency ::
  MonadIO m =>
  VkSubpassDependency ->
  m Vk.VkSubpassDependency
unVkSubpassDependency a =
  liftIO . Vk.newVkData $ \ptr -> do
    Vk.writeField @"srcSubpass" ptr (srcSubpass a)
    Vk.writeField @"dstSubpass" ptr (dstSubpass a)
    Vk.writeField @"srcStageMask" ptr (unVkPipelineStageBits $ srcStageMask a)
    Vk.writeField @"dstStageMask" ptr (unVkPipelineStageBits $ dstStageMask a)
    Vk.writeField @"srcAccessMask" ptr (unVkAccessBits $ srcAccessMask a)
    Vk.writeField @"dstAccessMask" ptr (unVkAccessBits $ dstAccessMask a)
    Vk.writeField @"dependencyFlags" ptr (unVkDependencyBits $ dependencyFlags a)

data VkRenderPassCreateInfo
  = VkRenderPassCreateInfo
  { flags :: [VkRenderPassCreateFlag]
  , pAttachments :: [VkAttachmentDescription]
  , pSubpasses :: [VkSubpassDescription]
  , pDependencies :: [VkSubpassDependency]
  } deriving (Eq, Ord, Show)

unVkRenderPassCreateInfo ::
  MonadIO m =>
  VkRenderPassCreateInfo ->
  m Vk.VkRenderPassCreateInfo
unVkRenderPassCreateInfo a = do
  as <- traverse unVkAttachmentDescription (pAttachments a)
  ss <- traverse unVkSubpassDescription (pSubpasses a)
  ds <- traverse unVkSubpassDependency (pDependencies a)
  liftIO . Foreign.withArray as $ \aPtr ->
    Foreign.withArray ss $ \sPtr ->
    Foreign.withArray ds $ \dPtr ->
    Vk.newVkData $ \ptr -> do
      Vk.writeField @"sType" ptr Vk.VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO
      Vk.writeField @"flags" ptr (unVkRenderPassCreateBits $ flags (a :: VkRenderPassCreateInfo))
      Vk.writeField @"attachmentCount" ptr (fromIntegral . length $ pAttachments a)
      Vk.writeField @"pAttachments" ptr aPtr
      Vk.writeField @"subpassCount" ptr (fromIntegral . length $ pSubpasses a)
      Vk.writeField @"pSubpasses" ptr sPtr
      Vk.writeField @"dependencyCount" ptr (fromIntegral . length $ pDependencies a)
      Vk.writeField @"pDependencies" ptr dPtr
