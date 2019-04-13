module Graphics.Vulkan.GraphicsPipeline
  (Vk.VkPipeline, vkCreateGraphicsPipelines)
where

import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Managed.Safe (MonadManaged, using, managed)
import Data.Maybe (fromMaybe)

import qualified Foreign
import qualified Graphics.Vulkan.Core_1_0 as Vk

import Data.Some (Some(..))
import Graphics.Vulkan.GraphicsPipelineCreateInfo
  (VkGraphicsPipelineCreateInfo, unVkGraphicsPipelineCreateInfo)
import Graphics.Vulkan.Result (vkResult)

vkCreateGraphicsPipelines ::
  (MonadManaged m, MonadIO m) =>
  Vk.VkDevice ->
  Maybe Vk.VkPipelineCache ->
  [Some VkGraphicsPipelineCreateInfo] ->
  Foreign.Ptr Vk.VkAllocationCallbacks ->
  m [Vk.VkPipeline]
vkCreateGraphicsPipelines d mCache infos cbs = do
  infos' <- traverse (\(Some a) -> unVkGraphicsPipelineCreateInfo a) infos
  let infoCount = length infos'
  infosPtr <- using $ managed (Foreign.withArray infos')
  pPtr <- using $ managed (Foreign.allocaArray infoCount)
  liftIO $
    vkResult =<<
    Vk.vkCreateGraphicsPipelines
      d
      (fromMaybe Vk.VK_NULL_HANDLE mCache)
      (fromIntegral infoCount)
      infosPtr
      cbs
      pPtr
  using $ managed (bracket
    (Foreign.peekArray infoCount pPtr)
    (traverse $ \p -> Vk.vkDestroyPipeline d p cbs))
