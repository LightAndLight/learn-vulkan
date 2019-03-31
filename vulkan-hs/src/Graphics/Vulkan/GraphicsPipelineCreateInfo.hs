{-# language DataKinds, TypeApplications #-}
{-# language GADTs, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Graphics.Vulkan.GraphicsPipelineCreateInfo where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits ((.|.))
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.Word (Word32)
import Unsafe.Coerce (unsafeCoerce)

import qualified Foreign
import qualified Graphics.Vulkan.Core_1_0 as Vk
import qualified Graphics.Vulkan.Core_1_1 as Vk
import qualified Graphics.Vulkan.Ext.VK_KHR_device_group as Vk
-- import qualified Graphics.Vulkan.Ext.VK_NV_ray_tracing as Vk

import Data.Constraint.All (All, AllSym1)
import Data.Type.Function (Apply, EqSym0, OrdSym0, ShowSym0)
import Graphics.Vulkan.Pipeline.ColorBlendStateCreateInfo
  (VkPipelineColorBlendStateCreateInfo, unVkPipelineColorBlendStateCreateInfo)
import Graphics.Vulkan.Pipeline.DynamicStateCreateInfo
  (VkPipelineDynamicStateCreateInfo, unVkPipelineDynamicStateCreateInfo)
import Graphics.Vulkan.Pipeline.InputAssemblyStateCreateInfo
  (VkPipelineInputAssemblyStateCreateInfo, unVkPipelineInputAssemblyStateCreateInfo)
import Graphics.Vulkan.Pipeline.MultisampleStateCreateInfo
  (VkPipelineMultisampleStateCreateInfo, unVkPipelineMultisampleStateCreateInfo)
import Graphics.Vulkan.Pipeline.RasterizationStateCreateInfo
  (VkPipelineRasterizationStateCreateInfo, unVkPipelineRasterizationStateCreateInfo)
import Graphics.Vulkan.Pipeline.DepthStencilStateCreateInfo
  (VkPipelineDepthStencilStateCreateInfo, unVkPipelineDepthStencilStateCreateInfo)
import Graphics.Vulkan.Pipeline.ShaderStageCreateInfo
  (VkPipelineShaderStageCreateInfo, unVkPipelineShaderStageCreateInfo)
import Graphics.Vulkan.Pipeline.TessellationStateCreateInfo
  (VkPipelineTessellationStateCreateInfo, unVkPipelineTessellationStateCreateInfo)
import Graphics.Vulkan.Pipeline.VertexInputStateCreateInfo
  (VkPipelineVertexInputStateCreateInfo, unVkPipelineVertexInputStateCreateInfo)
import Graphics.Vulkan.Pipeline.ViewportStateCreateInfo
  (VkPipelineViewportStateCreateInfo, unVkPipelineViewportStateCreateInfo)
import Graphics.Vulkan.RenderPass (VkRenderPass)

data VkPipelineCreateFlag
  = DisableOptimization
  | AllowDerivatives
  | Derivative
  | ViewIndexFromDeviceIndex
  | DispatchBase
  -- DeferCompileNV
  | ViewIndexFromDeviceIndexKHR
  | DispatchBaseKHR
  deriving (Eq, Ord, Show)

unVkPipelineCreateBit :: VkPipelineCreateFlag -> Vk.VkPipelineCreateBitmask a
unVkPipelineCreateBit a =
  case a of
    DisableOptimization -> Vk.VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT
    AllowDerivatives -> Vk.VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT
    Derivative -> Vk.VK_PIPELINE_CREATE_DERIVATIVE_BIT
    ViewIndexFromDeviceIndex -> unsafeCoerce Vk.VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT
    DispatchBase -> unsafeCoerce Vk.VK_PIPELINE_CREATE_DISPATCH_BASE
    -- DeferCompileNV -> Vk.VK_PIPELINE_CREATE_DEFER_COMPILE_BIT_NV
    ViewIndexFromDeviceIndexKHR ->
      unsafeCoerce Vk.VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT_KHR
    DispatchBaseKHR ->
      unsafeCoerce Vk.VK_PIPELINE_CREATE_DISPATCH_BASE_KHR

unVkPipelineCreateBits :: [VkPipelineCreateFlag] -> Vk.VkPipelineCreateFlags
unVkPipelineCreateBits = foldr (\a b -> unVkPipelineCreateBit a .|. b) 0

data Stages ts where
  Nil :: Stages '[]
  Cons :: VkPipelineShaderStageCreateInfo ts -> Stages tss -> Stages (ts ': tss)
deriving instance All (AllSym1 EqSym0) ts => Eq (Stages ts)
deriving instance (All (AllSym1 EqSym0) ts, All (AllSym1 OrdSym0) ts) => Ord (Stages ts)
deriving instance All (AllSym1 ShowSym0) ts => Show (Stages ts)

stageCount :: Stages ts -> Word32
stageCount Nil = 0
stageCount (Cons _ a) = 1 + stageCount a

unStages :: MonadIO m => Stages ts -> m [Vk.VkPipelineShaderStageCreateInfo]
unStages Nil = pure []
unStages (Cons a b) = (:) <$> unVkPipelineShaderStageCreateInfo a <*> unStages b

data VkGraphicsPipelineCreateInfo ts
  = VkGraphicsPipelineCreateInfo
  { flags :: [VkPipelineCreateFlag]
  , pStages :: Stages ts
  , pVertexInputState :: Maybe VkPipelineVertexInputStateCreateInfo
  , pInputAssemblyState :: Maybe VkPipelineInputAssemblyStateCreateInfo
  , pTessellationState :: Maybe VkPipelineTessellationStateCreateInfo
  , pViewportState :: Maybe VkPipelineViewportStateCreateInfo
  , pRasterizationState :: Maybe VkPipelineRasterizationStateCreateInfo
  , pMultisampleState :: Maybe VkPipelineMultisampleStateCreateInfo
  , pDepthStencilState :: Maybe VkPipelineDepthStencilStateCreateInfo
  , pColorBlendState :: Maybe VkPipelineColorBlendStateCreateInfo
  , pDynamicState :: Maybe VkPipelineDynamicStateCreateInfo
  , layout :: Vk.VkPipelineLayout
  , renderPass :: Vk.VkRenderPass
  , subpass :: Word32
  , basePipelineHandle :: Maybe Vk.VkPipeline
  , basePipelineIndex :: Maybe Int32
  }
deriving instance
  All (AllSym1 EqSym0) ts => Eq (VkGraphicsPipelineCreateInfo ts)
deriving instance
  (All (AllSym1 EqSym0) ts, All (AllSym1 OrdSym0) ts) => Ord (VkGraphicsPipelineCreateInfo ts)
deriving instance
  All (AllSym1 ShowSym0) ts => Show (VkGraphicsPipelineCreateInfo ts)

unVkGraphicsPipelineCreateInfo ::
  MonadIO m =>
  VkGraphicsPipelineCreateInfo ts ->
  m Vk.VkGraphicsPipelineCreateInfo
unVkGraphicsPipelineCreateInfo a = do
  ss <- unStages $ pStages a
  liftIO . Foreign.withArray ss $ \sPtr ->
    Vk.newVkData $ \ptr -> do
      Vk.writeField @"sType" ptr Vk.VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO
      Vk.writeField @"pNext" ptr Vk.VK_NULL
      Vk.writeField @"flags" ptr (unVkPipelineCreateBits $ flags a)
      Vk.writeField @"stageCount" ptr (stageCount $ pStages a)
      Vk.writeField @"pStages" ptr sPtr
      Vk.writeField @"pVertexInputState" ptr =<<
        maybe
          (pure Vk.VK_NULL_HANDLE)
          (fmap Vk.unsafePtr . unVkPipelineVertexInputStateCreateInfo)
          (pVertexInputState a)
      Vk.writeField @"pInputAssemblyState" ptr =<<
        maybe
          (pure Vk.VK_NULL_HANDLE)
          (fmap Vk.unsafePtr . unVkPipelineInputAssemblyStateCreateInfo)
          (pInputAssemblyState a)
      Vk.writeField @"pTessellationState" ptr =<<
        maybe
          (pure Vk.VK_NULL_HANDLE)
          (fmap Vk.unsafePtr . unVkPipelineTessellationStateCreateInfo)
          (pTessellationState a)
      Vk.writeField @"pViewportState" ptr =<<
        maybe
          (pure Vk.VK_NULL_HANDLE)
          (fmap Vk.unsafePtr . unVkPipelineViewportStateCreateInfo)
          (pViewportState a)
      Vk.writeField @"pRasterizationState" ptr =<<
        maybe
          (pure Vk.VK_NULL_HANDLE)
          (fmap Vk.unsafePtr . unVkPipelineRasterizationStateCreateInfo)
          (pRasterizationState a)
      Vk.writeField @"pMultisampleState" ptr =<<
        maybe
          (pure Vk.VK_NULL_HANDLE)
          (fmap Vk.unsafePtr . unVkPipelineMultisampleStateCreateInfo)
          (pMultisampleState a)
      Vk.writeField @"pDepthStencilState" ptr =<<
        maybe
          (pure Vk.VK_NULL_HANDLE)
          (fmap Vk.unsafePtr . unVkPipelineDepthStencilStateCreateInfo)
          (pDepthStencilState a)
      Vk.writeField @"pColorBlendState" ptr =<<
        maybe
          (pure Vk.VK_NULL_HANDLE)
          (fmap Vk.unsafePtr . unVkPipelineColorBlendStateCreateInfo)
          (pColorBlendState a)
      Vk.writeField @"pDynamicState" ptr =<<
        maybe
          (pure Vk.VK_NULL_HANDLE)
          (fmap Vk.unsafePtr . unVkPipelineDynamicStateCreateInfo)
          (pDynamicState a)
      Vk.writeField @"layout" ptr (layout a)
      Vk.writeField @"renderPass" ptr (renderPass a)
      Vk.writeField @"subpass" ptr (subpass a)
      Vk.writeField @"basePipelineHandle" ptr (fromMaybe Vk.VK_NULL_HANDLE $ basePipelineHandle a)
      Vk.writeField @"basePipelineIndex" ptr (fromMaybe (-1) $ basePipelineIndex a)
