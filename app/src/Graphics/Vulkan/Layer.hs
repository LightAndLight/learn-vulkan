{-# language DataKinds, TypeApplications #-}
{-# language PatternSynonyms #-}
module Graphics.Vulkan.Layer where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Word (Word32)
import Graphics.Vulkan.Layer.VK_LAYER_LUNARG_standard_validation
  (pattern VK_LAYER_LUNARG_standard_validation)
import Graphics.Vulkan.Layer.VK_LAYER_LUNARG_core_validation
  (pattern VK_LAYER_LUNARG_core_validation)
import Graphics.Vulkan.Layer.VK_LAYER_LUNARG_parameter_validation
  (pattern VK_LAYER_LUNARG_parameter_validation)
import Graphics.Vulkan.Layer.VK_LAYER_LUNARG_object_tracker
  (pattern VK_LAYER_LUNARG_object_tracker)
import Graphics.Vulkan.Layer.VK_LAYER_GOOGLE_threading
  (pattern VK_LAYER_GOOGLE_threading)
import Graphics.Vulkan.Layer.VK_LAYER_GOOGLE_unique_objects
  (pattern VK_LAYER_GOOGLE_unique_objects)

import qualified Foreign.C.String as Foreign
import qualified Foreign.Marshal.Alloc as Foreign
import qualified Foreign.Marshal.Array as Foreign
import qualified Foreign.Ptr as Foreign
import qualified Foreign.Storable as Foreign
import qualified Graphics.Vulkan.Core_1_0 as Vk

import Graphics.Vulkan.Result (vkResult)

data VkLayer
  = LunargStandardValidation
  | LunargCoreValidation
  | LunargParameterValidation
  | LunargObjectTracker
  | GoogleThreading
  | GoogleUniqueObjects
  | UnknownLayer Foreign.CString
  deriving (Eq, Ord, Show)

vkLayer :: Foreign.CString -> VkLayer
vkLayer str =
  case str of
    VK_LAYER_LUNARG_standard_validation -> LunargStandardValidation
    VK_LAYER_LUNARG_core_validation -> LunargCoreValidation
    VK_LAYER_LUNARG_parameter_validation -> LunargParameterValidation
    VK_LAYER_LUNARG_object_tracker -> LunargObjectTracker
    VK_LAYER_GOOGLE_threading -> GoogleThreading
    VK_LAYER_GOOGLE_unique_objects -> GoogleUniqueObjects
    _ -> UnknownLayer str

unVkLayer :: VkLayer -> Foreign.CString
unVkLayer layer =
  case layer of
    LunargStandardValidation -> VK_LAYER_LUNARG_standard_validation
    LunargCoreValidation -> VK_LAYER_LUNARG_core_validation
    LunargParameterValidation -> VK_LAYER_LUNARG_parameter_validation
    LunargObjectTracker -> VK_LAYER_LUNARG_object_tracker
    GoogleThreading -> VK_LAYER_GOOGLE_threading
    GoogleUniqueObjects -> VK_LAYER_GOOGLE_unique_objects
    UnknownLayer str -> str

data VkLayerProperties
  = VkLayerProperties
  { layerName :: VkLayer
  , specVersion :: Word32
  , implementationVersion :: Word32
  , description :: String
  } deriving (Eq, Ord, Show)

vkLayerProperties :: MonadIO m => Vk.VkLayerProperties -> m VkLayerProperties
vkLayerProperties a =
  liftIO $
  VkLayerProperties <$>
  Vk.withCStringField @"layerName" a (pure . vkLayer) <*>
  Vk.readField @"specVersion" aPtr <*>
  Vk.readField @"implementationVersion" aPtr <*>
  pure (Vk.getStringField @"description" a)
  where
    aPtr = Vk.unsafePtr a

vkEnumerateInstanceLayerProperties :: MonadIO m => m [VkLayerProperties]
vkEnumerateInstanceLayerProperties =
  liftIO $
  Foreign.alloca $ \countPtr -> do
    vkResult =<< Vk.vkEnumerateInstanceLayerProperties countPtr Foreign.nullPtr
    count <- fromIntegral <$> Foreign.peek countPtr
    Foreign.allocaArray count $ \propertiesPtr -> do
      vkResult =<< Vk.vkEnumerateInstanceLayerProperties countPtr propertiesPtr
      properties <- Foreign.peekArray count propertiesPtr
      traverse vkLayerProperties properties
