module Graphics.Vulkan.ShaderStage where

import Data.Bits ((.&.), (.|.))

import qualified Graphics.Vulkan.Core_1_0 as Vk
-- import qualified Graphics.Vulkan.Ext.VK_NV_mesh_shader as Vk
-- import qualified Graphics.Vulkan.Ext.VK_NV_ray_tracing as Vk

data VkShaderStageFlag
  = Vertex
  | TessellationControl
  | TessellationEvaluation
  | Geometry
  | Fragment
  | Compute
  | AllGraphics
  | All
  | RaygenNV
  | AnyHitNV
  | ClosestHitNV
  | MissNV
  | IntersectionNV
  | CallableNV
  | TaskNV
  | MeshNV
  deriving (Eq, Ord, Show)

vkShaderStageBit ::
  Vk.VkShaderStageBitmask a ->
  VkShaderStageFlag
vkShaderStageBit a =
  case a of
    Vk.VK_SHADER_STAGE_VERTEX_BIT -> Vertex
    Vk.VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT -> TessellationControl
    Vk.VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT -> TessellationEvaluation
    Vk.VK_SHADER_STAGE_GEOMETRY_BIT -> Geometry
    Vk.VK_SHADER_STAGE_FRAGMENT_BIT -> Fragment
    Vk.VK_SHADER_STAGE_COMPUTE_BIT -> Compute
    Vk.VK_SHADER_STAGE_ALL_GRAPHICS -> AllGraphics
    Vk.VK_SHADER_STAGE_ALL -> All
    -- Vk.VK_SHADER_STAGE_RAYGEN_BIT_NV -> RaygenNV
    -- Vk.VK_SHADER_STAGE_ANY_HIT_BIT_NV -> AnyHitNV
    -- Vk.VK_SHADER_STAGE_CLOSEST_HIT_BIT_NV -> ClosestHitNV
    -- Vk.Vk.VK_SHADER_STAGE_MISS_BIT_NV -> MissNV
    -- Vk.VK_SHADER_STAGE_INTERSECTION_BIT_NV -> IntersectionNV
    -- Vk.VK_SHADER_STAGE_CALLABLE_BIT_NV -> CallableNV
    -- Vk.VK_SHADER_STAGE_TASK_BIT_NV -> TaskNV
    -- Vk.VK_SHADER_STAGE_MESH_BIT_NV -> MeshNV

unVkShaderStageBit ::
  VkShaderStageFlag ->
  Vk.VkShaderStageBitmask a
unVkShaderStageBit a =
  case a of
    Vertex -> Vk.VK_SHADER_STAGE_VERTEX_BIT
    TessellationControl -> Vk.VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT
    TessellationEvaluation -> Vk.VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT
    Geometry -> Vk.VK_SHADER_STAGE_GEOMETRY_BIT
    Fragment -> Vk.VK_SHADER_STAGE_FRAGMENT_BIT
    Compute -> Vk.VK_SHADER_STAGE_COMPUTE_BIT
    AllGraphics -> Vk.VK_SHADER_STAGE_ALL_GRAPHICS
    All -> Vk.VK_SHADER_STAGE_ALL
    -- RaygenNV -> Vk.VK_SHADER_STAGE_RAYGEN_BIT_NV
    -- AnyHitNV -> Vk.VK_SHADER_STAGE_ANY_HIT_BIT_NV
    -- ClosestHitNV -> Vk.VK_SHADER_STAGE_CLOSEST_HIT_BIT_NV
    -- MissNV -> Vk.Vk.VK_SHADER_STAGE_MISS_BIT_NV
    -- IntersectionNV -> Vk.VK_SHADER_STAGE_INTERSECTION_BIT_NV
    -- CallableNV -> Vk.VK_SHADER_STAGE_CALLABLE_BIT_NV
    -- TaskNV -> Vk.VK_SHADER_STAGE_TASK_BIT_NV
    -- MeshNV -> Vk.VK_SHADER_STAGE_MESH_BIT_NV

vkShaderStageBits ::
  Vk.VkShaderStageFlags ->
  [VkShaderStageFlag]
vkShaderStageBits bs =
  foldr
    (\(mask, val) b -> if mask .&. bs == mask then val : b else b)
    []
    [ (Vk.VK_SHADER_STAGE_VERTEX_BIT, Vertex)
    , (Vk.VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT, TessellationControl)
    , (Vk.VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT, TessellationEvaluation)
    , (Vk.VK_SHADER_STAGE_GEOMETRY_BIT, Geometry)
    , (Vk.VK_SHADER_STAGE_FRAGMENT_BIT, Fragment)
    , (Vk.VK_SHADER_STAGE_COMPUTE_BIT, Compute)
    , (Vk.VK_SHADER_STAGE_ALL_GRAPHICS, AllGraphics)
    , (Vk.VK_SHADER_STAGE_ALL, All)
    -- , (Vk.VK_SHADER_STAGE_RAYGEN_BIT_NV, RaygenNV)
    -- , (Vk.VK_SHADER_STAGE_ANY_HIT_BIT_NV, AnyHitNV)
    -- , (Vk.VK_SHADER_STAGE_CLOSEST_HIT_BIT_NV, ClosestHitNV)
    -- , (Vk.Vk.VK_SHADER_STAGE_MISS_BIT_NV, MissNV)
    -- , (Vk.VK_SHADER_STAGE_INTERSECTION_BIT_NV, IntersectionNV)
    -- , (Vk.VK_SHADER_STAGE_CALLABLE_BIT_NV, CallableNV)
    -- , (Vk.VK_SHADER_STAGE_TASK_BIT_NV, TaskNV)
    -- , (Vk.VK_SHADER_STAGE_MESH_BIT_NV, MeshNV)
    ]

unVkShaderStageBits ::
  [VkShaderStageFlag] ->
  Vk.VkShaderStageFlags
unVkShaderStageBits =
  foldr (\a b -> unVkShaderStageBit a .|. b) 0
