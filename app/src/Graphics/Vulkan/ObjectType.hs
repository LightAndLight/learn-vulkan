module Graphics.Vulkan.ObjectType where

import qualified Graphics.Vulkan.Core_1_0 as Vk

data VkObjectType
  = Unknown
  | Instance
  | PhysicalDevice
  | Device
  | Queue
  | Semaphore
  | CommandBuffer
  | Fence
  | DeviceMemory
  | Buffer
  | Image
  | Event
  | QueryPool
  | BufferView
  | ImageView
  | ShaderModule
  | PipelineCache
  | PipelineLayout
  | RenderPass
  | Pipeline
  | DescriptorSetLayout
  | Sampler
  | DescriptorPool
  | DescriptorSet
  | Framebuffer
  | CommandPool
  deriving (Eq, Ord, Show)

vkObjectType :: Vk.VkObjectType -> VkObjectType
vkObjectType input =
  case input of
    Vk.VK_OBJECT_TYPE_UNKNOWN -> Unknown
    Vk.VK_OBJECT_TYPE_INSTANCE -> Instance
    Vk.VK_OBJECT_TYPE_PHYSICAL_DEVICE -> PhysicalDevice
    Vk.VK_OBJECT_TYPE_DEVICE -> Device
    Vk.VK_OBJECT_TYPE_QUEUE -> Queue
    Vk.VK_OBJECT_TYPE_SEMAPHORE -> Semaphore
    Vk.VK_OBJECT_TYPE_COMMAND_BUFFER -> CommandBuffer
    Vk.VK_OBJECT_TYPE_FENCE -> Fence
    Vk.VK_OBJECT_TYPE_DEVICE_MEMORY -> DeviceMemory
    Vk.VK_OBJECT_TYPE_BUFFER -> Buffer
    Vk.VK_OBJECT_TYPE_IMAGE -> Image
    Vk.VK_OBJECT_TYPE_EVENT -> Event
    Vk.VK_OBJECT_TYPE_QUERY_POOL -> QueryPool
    Vk.VK_OBJECT_TYPE_BUFFER_VIEW -> BufferView
    Vk.VK_OBJECT_TYPE_IMAGE_VIEW -> ImageView
    Vk.VK_OBJECT_TYPE_SHADER_MODULE -> ShaderModule
    Vk.VK_OBJECT_TYPE_PIPELINE_CACHE -> PipelineCache
    Vk.VK_OBJECT_TYPE_PIPELINE_LAYOUT -> PipelineLayout
    Vk.VK_OBJECT_TYPE_RENDER_PASS -> RenderPass
    Vk.VK_OBJECT_TYPE_PIPELINE -> Pipeline
    Vk.VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT -> DescriptorSetLayout
    Vk.VK_OBJECT_TYPE_SAMPLER -> Sampler
    Vk.VK_OBJECT_TYPE_DESCRIPTOR_POOL -> DescriptorPool
    Vk.VK_OBJECT_TYPE_DESCRIPTOR_SET -> DescriptorSet
    Vk.VK_OBJECT_TYPE_FRAMEBUFFER -> Framebuffer
    Vk.VK_OBJECT_TYPE_COMMAND_POOL -> CommandPool

unVkObjectType :: VkObjectType -> Vk.VkObjectType
unVkObjectType input =
  case input of
    Unknown -> Vk.VK_OBJECT_TYPE_UNKNOWN
    Instance -> Vk.VK_OBJECT_TYPE_INSTANCE
    PhysicalDevice -> Vk.VK_OBJECT_TYPE_PHYSICAL_DEVICE
    Device -> Vk.VK_OBJECT_TYPE_DEVICE
    Queue -> Vk.VK_OBJECT_TYPE_QUEUE
    Semaphore -> Vk.VK_OBJECT_TYPE_SEMAPHORE
    CommandBuffer -> Vk.VK_OBJECT_TYPE_COMMAND_BUFFER
    Fence -> Vk.VK_OBJECT_TYPE_FENCE
    DeviceMemory -> Vk.VK_OBJECT_TYPE_DEVICE_MEMORY
    Buffer -> Vk.VK_OBJECT_TYPE_BUFFER
    Image -> Vk.VK_OBJECT_TYPE_IMAGE
    Event -> Vk.VK_OBJECT_TYPE_EVENT
    QueryPool -> Vk.VK_OBJECT_TYPE_QUERY_POOL
    BufferView -> Vk.VK_OBJECT_TYPE_BUFFER_VIEW
    ImageView -> Vk.VK_OBJECT_TYPE_IMAGE_VIEW
    ShaderModule -> Vk.VK_OBJECT_TYPE_SHADER_MODULE
    PipelineCache -> Vk.VK_OBJECT_TYPE_PIPELINE_CACHE
    PipelineLayout -> Vk.VK_OBJECT_TYPE_PIPELINE_LAYOUT
    RenderPass -> Vk.VK_OBJECT_TYPE_RENDER_PASS
    Pipeline -> Vk.VK_OBJECT_TYPE_PIPELINE
    DescriptorSetLayout -> Vk.VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT
    Sampler -> Vk.VK_OBJECT_TYPE_SAMPLER
    DescriptorPool -> Vk.VK_OBJECT_TYPE_DESCRIPTOR_POOL
    DescriptorSet -> Vk.VK_OBJECT_TYPE_DESCRIPTOR_SET
    Framebuffer -> Vk.VK_OBJECT_TYPE_FRAMEBUFFER
    CommandPool -> Vk.VK_OBJECT_TYPE_COMMAND_POOL
