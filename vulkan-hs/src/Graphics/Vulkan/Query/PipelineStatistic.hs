module Graphics.Vulkan.Query.PipelineStatistic where

import Data.Bits ((.|.))

import qualified Graphics.Vulkan.Core_1_0 as Vk

data VkQueryPipelineStatisticFlag
  = InputAssemblyVertices
  | InputAssemblyPrimitives
  | VertexShaderInvocations
  | GeometryShaderInvocations
  | GeometryShaderPrimitives
  | ClippingInvocations
  | ClippingPrimitives
  | FragmentShaderInvocations
  | TessellationControlShaderPatches
  | TessellationEvaluationShaderInvocations
  | ComputeShaderInvocations
  deriving (Eq, Ord, Show)

unVkQueryPipelineStatisticBit ::
  VkQueryPipelineStatisticFlag ->
  Vk.VkQueryPipelineStatisticBitmask a
unVkQueryPipelineStatisticBit a =
  case a of
    InputAssemblyVertices -> Vk.VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT
    InputAssemblyPrimitives -> Vk.VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT
    VertexShaderInvocations -> Vk.VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT
    GeometryShaderInvocations -> Vk.VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT
    GeometryShaderPrimitives -> Vk.VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT
    ClippingInvocations -> Vk.VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT
    ClippingPrimitives -> Vk.VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT
    FragmentShaderInvocations -> Vk.VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT
    TessellationControlShaderPatches ->
      Vk.VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT
    TessellationEvaluationShaderInvocations ->
      Vk.VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT
    ComputeShaderInvocations -> Vk.VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT

unVkQueryPipelineStatisticBits ::
  [VkQueryPipelineStatisticFlag] ->
  Vk.VkQueryPipelineStatisticFlags
unVkQueryPipelineStatisticBits = foldr (\a b -> unVkQueryPipelineStatisticBit a .|. b) 0
