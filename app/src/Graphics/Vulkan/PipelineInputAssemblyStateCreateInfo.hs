{-# language DataKinds, TypeApplications #-}
{-# language EmptyCase, EmptyDataDeriving #-}
module Graphics.Vulkan.PipelineInputAssemblyStateCreateInfo where

import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Graphics.Vulkan.Core_1_0 as Vk
import qualified Graphics.Vulkan.Marshal as Vk

import Graphics.Vulkan.Utils (vkBool32, unVkBool32)

data VkPipelineInputAssemblyStateCreateFlag
  deriving (Eq, Ord, Show)

vkPipelineInputAssemblyStateCreateBits ::
  Vk.VkPipelineInputAssemblyStateCreateFlags ->
  [VkPipelineInputAssemblyStateCreateFlag]
vkPipelineInputAssemblyStateCreateBits _ = []

unVkPipelineInputAssemblyStateCreateBits ::
  [VkPipelineInputAssemblyStateCreateFlag] ->
  Vk.VkPipelineInputAssemblyStateCreateFlags
unVkPipelineInputAssemblyStateCreateBits [] = 0
unVkPipelineInputAssemblyStateCreateBits (x:_) = case x of

data VkPrimitiveTopology
  = PointList
  | LineList
  | LineStrip
  | TriangleList
  | TriangleStrip
  | TriangleFan
  | LineListWithAdjacency
  | LineStripWithAdjacency
  | TriangleListWithAdjacency
  | TriangleStripWithAdjacency
  | PatchList
  deriving (Eq, Ord, Show)

vkPrimitiveTopology ::
  Vk.VkPrimitiveTopology ->
  VkPrimitiveTopology
vkPrimitiveTopology a =
  case a of
    Vk.VK_PRIMITIVE_TOPOLOGY_POINT_LIST -> PointList
    Vk.VK_PRIMITIVE_TOPOLOGY_LINE_LIST -> LineList
    Vk.VK_PRIMITIVE_TOPOLOGY_LINE_STRIP -> LineStrip
    Vk.VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST -> TriangleList
    Vk.VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP -> TriangleStrip
    Vk.VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN -> TriangleFan
    Vk.VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY -> LineListWithAdjacency
    Vk.VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY -> LineStripWithAdjacency
    Vk.VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY -> TriangleListWithAdjacency
    Vk.VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY -> TriangleStripWithAdjacency
    Vk.VK_PRIMITIVE_TOPOLOGY_PATCH_LIST -> PatchList

unVkPrimitiveTopology ::
  VkPrimitiveTopology ->
  Vk.VkPrimitiveTopology
unVkPrimitiveTopology a =
  case a of
    PointList -> Vk.VK_PRIMITIVE_TOPOLOGY_POINT_LIST
    LineList -> Vk.VK_PRIMITIVE_TOPOLOGY_LINE_LIST
    LineStrip -> Vk.VK_PRIMITIVE_TOPOLOGY_LINE_STRIP
    TriangleList -> Vk.VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
    TriangleStrip -> Vk.VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP
    TriangleFan -> Vk.VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN
    LineListWithAdjacency -> Vk.VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY
    LineStripWithAdjacency -> Vk.VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY
    TriangleListWithAdjacency -> Vk.VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY
    TriangleStripWithAdjacency -> Vk.VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY
    PatchList -> Vk.VK_PRIMITIVE_TOPOLOGY_PATCH_LIST

data VkPipelineInputAssemblyStateCreateInfo
  = VkPipelineInputAssemblyStateCreateInfo
  { flags :: [VkPipelineInputAssemblyStateCreateFlag]
  , topology :: VkPrimitiveTopology
  , primitiveRestartEnable :: Bool
  } deriving (Eq, Ord, Show)

vkPipelineInputAssemblyStateCreateInfo ::
  Vk.VkPipelineInputAssemblyStateCreateInfo ->
  VkPipelineInputAssemblyStateCreateInfo
vkPipelineInputAssemblyStateCreateInfo a =
  VkPipelineInputAssemblyStateCreateInfo
  { flags = vkPipelineInputAssemblyStateCreateBits $ Vk.getField @"flags" a
  , topology = vkPrimitiveTopology $ Vk.getField @"topology" a
  , primitiveRestartEnable = vkBool32 $ Vk.getField @"primitiveRestartEnable" a
  }

unVkPipelineInputAssemblyStateCreateInfo ::
  MonadIO m =>
  VkPipelineInputAssemblyStateCreateInfo ->
  m Vk.VkPipelineInputAssemblyStateCreateInfo
unVkPipelineInputAssemblyStateCreateInfo a =
  liftIO . Vk.newVkData $ \ptr -> do
    Vk.writeField @"sType" ptr Vk.VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO
    Vk.writeField @"flags" ptr (unVkPipelineInputAssemblyStateCreateBits $ flags a)
    Vk.writeField @"topology" ptr (unVkPrimitiveTopology $ topology a)
    Vk.writeField @"primitiveRestartEnable" ptr (unVkBool32 $ primitiveRestartEnable a)
