module Graphics.Vulkan.ShaderModule
  ( Vk.VkShaderModule
  , shaderModuleFromFile
  , vkCreateShaderModule
  )
where

import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Managed.Safe (MonadManaged, using, managed)

import qualified Graphics.Vulkan.Core_1_0 as Vk
import qualified Foreign

import Graphics.Vulkan.Result (vkResult)
import Graphics.Vulkan.ShaderModuleCreateInfo
  ( VkShaderModuleCreateInfo(..), unVkShaderModuleCreateInfo
  , VkShaderModuleCreateFlag, readShader
  )

shaderModuleFromFile ::
  (MonadManaged m, MonadIO m) =>
  FilePath ->
  [VkShaderModuleCreateFlag] ->
  Vk.VkDevice ->
  Foreign.Ptr Vk.VkAllocationCallbacks ->
  m Vk.VkShaderModule
shaderModuleFromFile file smFlags d cbs = do
  (code, size) <- readShader file
  let
    info =
      VkShaderModuleCreateInfo
      { flags = smFlags
      , codeSize = size
      , pCode = code
      }
  vkCreateShaderModule d info cbs

vkCreateShaderModule ::
  (MonadManaged m, MonadIO m) =>
  Vk.VkDevice ->
  VkShaderModuleCreateInfo ->
  Foreign.Ptr Vk.VkAllocationCallbacks ->
  m Vk.VkShaderModule
vkCreateShaderModule d info cbs = do
  smPtr <- using $ managed Foreign.alloca
  info' <- unVkShaderModuleCreateInfo info
  liftIO $ vkResult =<< Vk.vkCreateShaderModule d (Vk.unsafePtr info') cbs smPtr
  using $ managed (bracket
    (Foreign.peek smPtr)
    (\sm -> Vk.vkDestroyShaderModule d sm cbs))
