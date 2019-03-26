{-# language DataKinds, TypeApplications #-}
{-# language ScopedTypeVariables #-}
module Graphics.Vulkan.Ext.DebugUtils where

import Data.Bits ((.&.), (.|.))
import Data.Int (Int32)
import Data.Void (Void)
import Data.Word (Word32, Word64)

import qualified Foreign.C.String as Foreign
import qualified Foreign.Marshal.Alloc as Foreign
import qualified Foreign.Marshal.Array as Foreign
import qualified Foreign.Ptr as Foreign
import qualified Foreign.Storable as Foreign
import qualified Graphics.Vulkan.Constants as Vk
import qualified Graphics.Vulkan.Ext.VK_EXT_debug_utils as Vk
import qualified Graphics.Vulkan.Constants as Vk
import qualified Graphics.Vulkan.Marshal.Proc as Vk

import Graphics.Vulkan.ObjectType (VkObjectType(..), vkObjectType)
import Graphics.Vulkan.Result (vkResult)

data VkDebugUtilsObjectNameInfoEXT =
  VkDebugUtilsObjectNameInfoEXT
  { objectType :: VkObjectType
  , objectHandle :: Word64
  , pObjectName :: String
  } deriving (Eq, Show, Ord)

data VkDebugUtilsLabelEXT =
  VkDebugUtilsLabelEXT
  { pLabelName :: String
  , color :: (Float, Float, Float, Float)
  } deriving (Eq, Show, Ord)

data VkDebugUtilsMessengerCallbackDataEXT
  = VkDebugUtilsMessengerCallbackDataEXT
  { pMessageIdName :: String
  , messageIdNumber :: Int32
  , pMessage :: String
  , queueLabelCount :: Word32
  , pQueueLabels :: [VkDebugUtilsLabelEXT]
  , cmdBufLabelCount :: Word32
  , pCmdBufLabels :: [VkDebugUtilsLabelEXT]
  , objectCount :: Word32
  , pObjects :: [VkDebugUtilsObjectNameInfoEXT]
  } deriving (Eq, Show, Ord)

vkDebugUtilsLabelEXT :: Vk.VkDebugUtilsLabelEXT -> IO VkDebugUtilsLabelEXT
vkDebugUtilsLabelEXT d =
  VkDebugUtilsLabelEXT <$>
  (Foreign.peekCString =<< Vk.readField @"pLabelName" dPtr) <*>
  ((,,,) <$>
   Vk.readFieldArray @"color" @0 dPtr <*>
   Vk.readFieldArray @"color" @1 dPtr <*>
   Vk.readFieldArray @"color" @2 dPtr <*>
   Vk.readFieldArray @"color" @3 dPtr)
  where
    dPtr = (Vk.unsafePtr d)

vkDebugUtilsObjectNameInfoEXT ::
  Vk.VkDebugUtilsObjectNameInfoEXT ->
  IO VkDebugUtilsObjectNameInfoEXT
vkDebugUtilsObjectNameInfoEXT d =
  VkDebugUtilsObjectNameInfoEXT <$>
  (vkObjectType <$> Vk.readField @"objectType" dPtr) <*>
  (Vk.readField @"objectHandle" dPtr) <*>
  (Foreign.peekCString =<< Vk.readField @"pObjectName" dPtr)
  where
    dPtr = Vk.unsafePtr d

vkDebugUtilsMessengerCallbackDataEXT ::
  Foreign.Ptr Vk.VkDebugUtilsMessengerCallbackDataEXT ->
  IO VkDebugUtilsMessengerCallbackDataEXT
vkDebugUtilsMessengerCallbackDataEXT dataPtr = do
  data_ <- Foreign.peek dataPtr
  queueLabelCount <- Vk.readField @"queueLabelCount" dataPtr
  cmdBufLabelCount <- Vk.readField @"cmdBufLabelCount" dataPtr
  objectCount <- Vk.readField @"objectCount" dataPtr
  VkDebugUtilsMessengerCallbackDataEXT <$>
    (Foreign.peekCString =<< Vk.readField @"pMessageIdName" dataPtr) <*>
    Vk.readField @"messageIdNumber" dataPtr <*>
    (Foreign.peekCString =<< Vk.readField @"pMessage" dataPtr) <*>
    pure queueLabelCount <*>
    (traverse vkDebugUtilsLabelEXT =<<
     Foreign.peekArray (fromIntegral queueLabelCount) =<<
     Vk.readField @"pQueueLabels" dataPtr) <*>
    pure cmdBufLabelCount <*>
    (traverse vkDebugUtilsLabelEXT =<<
     Foreign.peekArray (fromIntegral queueLabelCount) =<<
     Vk.readField @"pCmdBufLabels" dataPtr) <*>
    pure objectCount <*>
    (traverse vkDebugUtilsObjectNameInfoEXT =<<
     Foreign.peekArray (fromIntegral queueLabelCount) =<<
     Vk.readField @"pObjects" dataPtr)

data VkDebugUtilsMessageSeverity
  = Verbose
  | Info
  | Warning
  | Error
  deriving (Eq, Show, Ord)

data VkDebugUtilsMessageType
  = General
  | Validation
  | Performance
  deriving (Eq, Show, Ord)

vkDebugUtilsMessageSeverityBit ::
  Vk.VkDebugUtilsMessageSeverityBitmaskEXT a ->
  VkDebugUtilsMessageSeverity
vkDebugUtilsMessageSeverityBit a =
  case a of
    Vk.VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT -> Verbose
    Vk.VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT -> Info
    Vk.VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT -> Warning
    Vk.VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT -> Error

unVkDebugUtilsMessageSeverityBit ::
  VkDebugUtilsMessageSeverity ->
  Vk.VkDebugUtilsMessageSeverityBitmaskEXT a
unVkDebugUtilsMessageSeverityBit a =
  case a of
    Verbose -> Vk.VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT
    Info -> Vk.VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT
    Warning -> Vk.VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
    Error -> Vk.VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT

vkDebugUtilsMessageTypeBit ::
  Vk.VkDebugUtilsMessageTypeBitmaskEXT a ->
  VkDebugUtilsMessageType
vkDebugUtilsMessageTypeBit a =
  case a of
    Vk.VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT -> General
    Vk.VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT -> Validation
    Vk.VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT -> Performance

unVkDebugUtilsMessageTypeBit ::
  VkDebugUtilsMessageType ->
  Vk.VkDebugUtilsMessageTypeBitmaskEXT a
unVkDebugUtilsMessageTypeBit a =
  case a of
    General -> Vk.VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
    Validation -> Vk.VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
    Performance -> Vk.VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT

vkDebugUtilsMessageTypeBits ::
  Vk.VkDebugUtilsMessageTypeFlagsEXT ->
  [VkDebugUtilsMessageType]
vkDebugUtilsMessageTypeBits bs =
  foldr
    (\(mask, val) b -> if mask .&. bs == mask then val : b else b)
    []
    [ (Vk.VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT, General)
    , (Vk.VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT, Validation)
    , (Vk.VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT, Performance)
    ]

unVkDebugUtilsMessageTypeBits ::
  [VkDebugUtilsMessageType] ->
  Vk.VkDebugUtilsMessageTypeFlagsEXT
unVkDebugUtilsMessageTypeBits = foldr (\a b -> unVkDebugUtilsMessageTypeBit a .|. b) 0

vkDebugUtilsMessageSeverityBits ::
  Vk.VkDebugUtilsMessageSeverityFlagsEXT ->
  [VkDebugUtilsMessageSeverity]
vkDebugUtilsMessageSeverityBits bs =
  foldr
    (\(mask, val) b -> if mask .&. bs == mask then val : b else b)
    []
    [ (Vk.VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT, Verbose)
    , (Vk.VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT, Info)
    , (Vk.VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT, Warning)
    , (Vk.VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT, Error)
    ]

unVkDebugUtilsMessageSeverityBits ::
  [VkDebugUtilsMessageSeverity] ->
  Vk.VkDebugUtilsMessageSeverityFlagsEXT
unVkDebugUtilsMessageSeverityBits = foldr (\a b -> unVkDebugUtilsMessageSeverityBit a .|. b) 0

withDebugUtilsMessengerCallback ::
  forall a.
  Foreign.Storable a =>
  (VkDebugUtilsMessageSeverity ->
   [VkDebugUtilsMessageType] ->
   VkDebugUtilsMessengerCallbackDataEXT ->
   a ->
   IO Bool) ->
  (Vk.PFN_vkDebugUtilsMessengerCallbackEXT -> IO ()) ->
  IO ()
withDebugUtilsMessengerCallback cb f = do
  cbPtr <- Vk.newVkDebugUtilsMessengerCallbackEXT $ \a b c d -> do
      c' <- vkDebugUtilsMessengerCallbackDataEXT c
      d' <- Foreign.peek (Foreign.castPtr d :: Foreign.Ptr a)
      res <-
        cb
          (vkDebugUtilsMessageSeverityBit a)
          (vkDebugUtilsMessageTypeBits b)
          c'
          d'
      pure $
        if res
        then Vk.VK_TRUE
        else Vk.VK_FALSE
  f cbPtr
  Foreign.freeHaskellFunPtr cbPtr

data VkDebugUtilsMessengerCreateInfoEXT a
  = VkDebugUtilsMessengerCreateInfoEXT
  { messageSeverity :: [VkDebugUtilsMessageSeverity]
  , messageType :: [VkDebugUtilsMessageType]
  , pfnUserCallback ::
      VkDebugUtilsMessageSeverity ->
      [VkDebugUtilsMessageType] ->
      VkDebugUtilsMessengerCallbackDataEXT ->
      a ->
      IO Bool
  , pUserData :: a
  }

withDebugUtilsMessengerCreateInfo ::
  forall a.
  Foreign.Storable a =>
  VkDebugUtilsMessengerCreateInfoEXT a ->
  (Foreign.Ptr Vk.VkDebugUtilsMessengerCreateInfoEXT -> IO ()) ->
  IO ()
withDebugUtilsMessengerCreateInfo ci f =
  withDebugUtilsMessengerCallback (pfnUserCallback ci) $ \cbPtr -> do
    Foreign.alloca $ \(userDataPtr :: Foreign.Ptr a) -> do
      Foreign.poke userDataPtr (pUserData ci)
      createInfo <-
        Vk.newVkData @Vk.VkDebugUtilsMessengerCreateInfoEXT $
        \createInfoPtr -> do
          Vk.writeField @"sType" createInfoPtr Vk.VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT

          Vk.writeField @"messageSeverity" createInfoPtr
            (unVkDebugUtilsMessageSeverityBits $ messageSeverity ci)
          Vk.writeField @"messageType" createInfoPtr
            (unVkDebugUtilsMessageTypeBits $ messageType ci)
          Vk.writeField @"pfnUserCallback" createInfoPtr cbPtr
          Vk.writeField @"pUserData" createInfoPtr (Foreign.castPtr userDataPtr)
      f $ Vk.unsafePtr createInfo

withDebugUtilsMessenger ::
  Foreign.Storable a =>
  Vk.VkInstance ->
  VkDebugUtilsMessengerCreateInfoEXT a ->
  Foreign.Ptr Vk.VkAllocationCallbacks ->
  (Vk.VkDebugUtilsMessengerEXT -> IO ()) ->
  IO ()
withDebugUtilsMessenger inst createInfo cbPtr f = do
  createMessenger <-
    Vk.vkGetInstanceProc @"vkCreateDebugUtilsMessengerEXT" inst
  withDebugUtilsMessengerCreateInfo createInfo $ \createInfoPtr ->
    Foreign.alloca $ \messengerPtr -> do
      vkResult =<< createMessenger inst createInfoPtr cbPtr messengerPtr
      messenger <- Foreign.peek messengerPtr
      f messenger
      destroyMessenger <-
        Vk.vkGetInstanceProc @"vkDestroyDebugUtilsMessengerEXT" inst
      destroyMessenger inst messenger cbPtr
