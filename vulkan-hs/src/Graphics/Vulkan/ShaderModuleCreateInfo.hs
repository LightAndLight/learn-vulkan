{-# language DataKinds, TypeApplications #-}
{-# language EmptyCase, EmptyDataDeriving #-}
{-# language TupleSections #-}
module Graphics.Vulkan.ShaderModuleCreateInfo where

import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Managed.Safe (MonadManaged, using, managed)
import Data.Word (Word32)
import System.IO.MMap (Mode(..), mmapFilePtr, munmapFilePtr)

import qualified Foreign
import qualified Foreign.C.Types as Foreign
import qualified Graphics.Vulkan.Core_1_0 as Vk

readShader :: (MonadManaged m, MonadIO m) => FilePath -> m (Foreign.Ptr Word32, Foreign.CSize)
readShader fp = do
  (ptr, rawsize, _, size) <- liftIO $ mmapFilePtr fp ReadOnly Nothing
  (, fromIntegral size) <$> using (managed (bracket (pure ptr) (\p -> munmapFilePtr p rawsize)))

data VkShaderModuleCreateFlag deriving (Eq, Ord, Show)

{-
vkShaderModuleCreateBit ::
  Vk.VkShaderModuleCreateBitmask a ->
  VkShaderModuleCreateFlag
vkShaderModuleCreateBit a = case a of

unVkShaderModuleCreateBit ::
  VkShaderModuleCreateFlag ->
  Vk.VkShaderModuleCreateBitmask a
unVkShaderModuleCreateBit _ = 0
-}

vkShaderModuleCreateBits ::
  Vk.VkShaderModuleCreateFlags ->
  [VkShaderModuleCreateFlag]
vkShaderModuleCreateBits _ = []

unVkShaderModuleCreateBits ::
  [VkShaderModuleCreateFlag] ->
  Vk.VkShaderModuleCreateFlags
unVkShaderModuleCreateBits [] = 0
unVkShaderModuleCreateBits (x:_) = case x of

data VkShaderModuleCreateInfo
  = VkShaderModuleCreateInfo
  { flags :: [VkShaderModuleCreateFlag]
  , codeSize :: Foreign.CSize
  , pCode :: Foreign.Ptr Word32
  } deriving (Eq, Ord, Show)

vkShaderModuleCreateInfo ::
  Vk.VkShaderModuleCreateInfo ->
  VkShaderModuleCreateInfo
vkShaderModuleCreateInfo a =
  VkShaderModuleCreateInfo
  { flags = vkShaderModuleCreateBits $ Vk.getField @"flags" a
  , codeSize = Vk.getField @"codeSize" a
  , pCode = Vk.getField @"pCode" a
  }

unVkShaderModuleCreateInfo ::
  MonadIO m =>
  VkShaderModuleCreateInfo ->
  m Vk.VkShaderModuleCreateInfo
unVkShaderModuleCreateInfo a =
  liftIO $
  Vk.newVkData $ \ptr -> do
    Vk.writeField @"sType" ptr Vk.VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO
    Vk.writeField @"pNext" ptr Vk.VK_NULL
    Vk.writeField @"flags" ptr (unVkShaderModuleCreateBits $ flags a)
    Vk.writeField @"codeSize" ptr (codeSize a)
    Vk.writeField @"pCode" ptr (pCode a)
