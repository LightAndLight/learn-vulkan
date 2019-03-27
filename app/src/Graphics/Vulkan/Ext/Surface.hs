{-# language DataKinds, TypeApplications #-}
module Graphics.Vulkan.Ext.Surface where

import Data.Bits ((.&.), (.|.))
import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Managed.Safe (MonadManaged, using, managed)
import Data.Word (Word32)

import qualified Foreign.Marshal.Alloc as Foreign
import qualified Foreign.Marshal.Array as Foreign
import qualified Foreign.Storable as Foreign
import qualified Foreign.Ptr as Foreign
import qualified Graphics.Vulkan.Ext.VK_KHR_shared_presentable_image as Vk
import qualified Graphics.Vulkan.Ext.VK_KHR_surface as Vk
import qualified Graphics.Vulkan.Marshal as Vk
import qualified Graphics.UI.GLFW as GLFW

import Graphics.Vulkan.Extent (VkExtent2D, vkExtent2D)
import Graphics.Vulkan.Ext.ColorSpace (VkColorSpaceKHR, vkColorSpaceKHR)
import Graphics.Vulkan.Format (VkFormat, vkFormat)
import Graphics.Vulkan.ImageCreateInfo (VkImageUsageFlag, vkImageUsageBits)
import Graphics.Vulkan.Result (vkResult)
import Graphics.Vulkan.Utils (vkBool32)

glfwCreateWindowSurface ::
  (MonadManaged m, MonadIO m) =>
  Vk.VkInstance ->
  GLFW.Window ->
  Foreign.Ptr Vk.VkAllocationCallbacks ->
  m Vk.VkSurfaceKHR
glfwCreateWindowSurface inst win cbs = do
  surfacePtr <- using $ managed Foreign.alloca
  liftIO $ vkResult =<< GLFW.createWindowSurface inst win cbs surfacePtr
  using $ managed (bracket
    (Foreign.peek surfacePtr)
    (\s -> Vk.vkDestroySurfaceKHR inst s cbs))

vkGetPhysicalDeviceSurfaceSupportKHR ::
  MonadIO m =>
  Vk.VkPhysicalDevice ->
  Word32 ->
  Vk.VkSurfaceKHR ->
  m Bool
vkGetPhysicalDeviceSurfaceSupportKHR pd qfix surf =
  liftIO . Foreign.alloca $ \bPtr -> do
    vkResult =<< Vk.vkGetPhysicalDeviceSurfaceSupportKHR pd qfix surf bPtr
    vkBool32 <$> Foreign.peek bPtr

data VkCompositeAlphaFlagKHR
  = Opaque
  | PreMultiplied
  | PostMultiplied
  | InheritCompositeAlpha
  deriving (Eq, Ord, Show)

vkCompositeAlphaBit ::
  Vk.VkCompositeAlphaBitmaskKHR a ->
  VkCompositeAlphaFlagKHR
vkCompositeAlphaBit a =
  case a of
    Vk.VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR -> Opaque
    Vk.VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR -> PreMultiplied
    Vk.VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR -> PostMultiplied
    Vk.VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR -> InheritCompositeAlpha

unVkCompositeAlphaBit ::
  VkCompositeAlphaFlagKHR ->
  Vk.VkCompositeAlphaBitmaskKHR a
unVkCompositeAlphaBit a =
  case a of
    Opaque -> Vk.VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR
    PreMultiplied -> Vk.VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR
    PostMultiplied -> Vk.VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR
    InheritCompositeAlpha -> Vk.VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR

vkCompositeAlphaBits ::
  Vk.VkCompositeAlphaFlagsKHR ->
  [VkCompositeAlphaFlagKHR]
vkCompositeAlphaBits bs =
  foldr
    (\(mask, val) b -> if mask .&. bs == mask then val : b else b)
    []
    [ (Vk.VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR, Opaque)
    , (Vk.VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR, PreMultiplied)
    , (Vk.VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR, PostMultiplied)
    , (Vk.VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR, InheritCompositeAlpha)
    ]

unVkCompositeAlphaBits ::
  [VkCompositeAlphaFlagKHR] ->
  Vk.VkCompositeAlphaFlagsKHR
unVkCompositeAlphaBits = foldr (\a b -> unVkCompositeAlphaBit a .|. b) 0

data VkSurfaceTransformFlagKHR
  = Identity
  | Rotate90
  | Rotate180
  | Rotate270
  | HorizontalMirror
  | HorizontalMirrorRotate90
  | HorizontalMirrorRotate180
  | HorizontalMirrorRotate270
  | InheritSurfaceTransform
  deriving (Eq, Ord, Show)

vkSurfaceTransformBit ::
  Vk.VkSurfaceTransformBitmaskKHR a ->
  VkSurfaceTransformFlagKHR
vkSurfaceTransformBit a =
  case a of
    Vk.VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR -> Identity
    Vk.VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR -> Rotate90
    Vk.VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR -> Rotate180
    Vk.VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR -> Rotate270
    Vk.VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR -> HorizontalMirror
    Vk.VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR -> HorizontalMirrorRotate90
    Vk.VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR -> HorizontalMirrorRotate180
    Vk.VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR -> HorizontalMirrorRotate270
    Vk.VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR -> InheritSurfaceTransform

unVkSurfaceTransformBit ::
  VkSurfaceTransformFlagKHR ->
  Vk.VkSurfaceTransformBitmaskKHR a
unVkSurfaceTransformBit a =
  case a of
    Identity -> Vk.VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR
    Rotate90 -> Vk.VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR
    Rotate180 -> Vk.VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR
    Rotate270 -> Vk.VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR
    HorizontalMirror -> Vk.VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR
    HorizontalMirrorRotate90 -> Vk.VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR
    HorizontalMirrorRotate180 -> Vk.VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR
    HorizontalMirrorRotate270 -> Vk.VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR
    InheritSurfaceTransform -> Vk.VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR

vkSurfaceTransformBits ::
  Vk.VkSurfaceTransformFlagsKHR ->
  [VkSurfaceTransformFlagKHR]
vkSurfaceTransformBits bs =
  foldr
    (\(mask, val) b -> if mask .&. bs == mask then val : b else b)
    []
    [ (Vk.VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR, Identity)
    , (Vk.VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR, Rotate90)
    , (Vk.VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR, Rotate180)
    , (Vk.VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR, Rotate270)
    , (Vk.VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR, HorizontalMirror)
    , (Vk.VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR, HorizontalMirrorRotate90)
    , (Vk.VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR, HorizontalMirrorRotate180)
    , (Vk.VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR, HorizontalMirrorRotate270)
    , (Vk.VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR, InheritSurfaceTransform)
    ]

unVkSurfaceTransformBits ::
  [VkSurfaceTransformFlagKHR] ->
  Vk.VkSurfaceTransformFlagsKHR
unVkSurfaceTransformBits = foldr (\a b -> unVkSurfaceTransformBit a .|. b) 0

data VkSurfaceCapabilitiesKHR
  = VkSurfaceCapabilitiesKHR
  { minImageCount :: Word32
  , maxImageCount :: Word32
  , currentExtent :: VkExtent2D
  , minImageExtent :: VkExtent2D
  , maxImageExtent :: VkExtent2D
  , maxImageArrayLayers :: Word32
  , supportedTransforms :: [VkSurfaceTransformFlagKHR]
  , currentTransform :: VkSurfaceTransformFlagKHR
  , supportedCompositeAlpha :: [VkCompositeAlphaFlagKHR]
  , supportedUsageFlags :: [VkImageUsageFlag]
  } deriving (Eq, Ord, Show)

vkSurfaceCapabilitiesKHR ::
  Vk.VkSurfaceCapabilitiesKHR ->
  VkSurfaceCapabilitiesKHR
vkSurfaceCapabilitiesKHR a =
  VkSurfaceCapabilitiesKHR
  { minImageCount = Vk.getField @"minImageCount" a
  , maxImageCount = Vk.getField @"maxImageCount" a
  , currentExtent = vkExtent2D $ Vk.getField @"currentExtent" a
  , minImageExtent = vkExtent2D $ Vk.getField @"minImageExtent" a
  , maxImageExtent = vkExtent2D $ Vk.getField @"maxImageExtent" a
  , maxImageArrayLayers = Vk.getField @"maxImageArrayLayers" a
  , supportedTransforms = vkSurfaceTransformBits $ Vk.getField @"supportedTransforms" a
  , currentTransform = vkSurfaceTransformBit $ Vk.getField @"currentTransform" a
  , supportedCompositeAlpha = vkCompositeAlphaBits $ Vk.getField @"supportedCompositeAlpha" a
  , supportedUsageFlags = vkImageUsageBits $ Vk.getField @"supportedUsageFlags" a
  }

vkGetPhysicalDeviceSurfaceCapabilitiesKHR ::
  MonadIO m =>
  Vk.VkPhysicalDevice ->
  Vk.VkSurfaceKHR ->
  m VkSurfaceCapabilitiesKHR
vkGetPhysicalDeviceSurfaceCapabilitiesKHR pd surf =
  liftIO . Foreign.alloca $ \scPtr -> do
    vkResult =<< Vk.vkGetPhysicalDeviceSurfaceCapabilitiesKHR pd surf scPtr
    vkSurfaceCapabilitiesKHR <$> Foreign.peek scPtr

data VkSurfaceFormatKHR
  = VkSurfaceFormatKHR
  { format :: VkFormat
  , colorSpace :: VkColorSpaceKHR
  } deriving (Eq, Ord, Show)

vkSurfaceFormatKHR :: Vk.VkSurfaceFormatKHR -> VkSurfaceFormatKHR
vkSurfaceFormatKHR a =
  VkSurfaceFormatKHR
  { format = vkFormat $ Vk.getField @"format" a
  , colorSpace = vkColorSpaceKHR $ Vk.getField @"colorSpace" a
  }

vkGetPhysicalDeviceSurfaceFormatsKHR ::
  MonadIO m =>
  Vk.VkPhysicalDevice ->
  Vk.VkSurfaceKHR ->
  m [VkSurfaceFormatKHR]
vkGetPhysicalDeviceSurfaceFormatsKHR pd surf =
  liftIO $
  Foreign.alloca $ \countPtr -> do
    vkResult =<< Vk.vkGetPhysicalDeviceSurfaceFormatsKHR pd surf countPtr Foreign.nullPtr
    count <- fromIntegral <$> Foreign.peek countPtr
    Foreign.allocaArray count $ \arrayPtr -> do
      vkResult =<< Vk.vkGetPhysicalDeviceSurfaceFormatsKHR pd surf countPtr arrayPtr
      fmap vkSurfaceFormatKHR <$> Foreign.peekArray count arrayPtr

data VkPresentModeKHR
  = ImmediateKHR
  | MailboxKHR
  | FifoKHR
  | FifoRelaxedKHR
  | SharedDemandRefreshKHR
  | SharedContinuousRefreshKHR
  deriving (Eq, Ord, Show)

vkPresentModeKHR :: Vk.VkPresentModeKHR -> VkPresentModeKHR
vkPresentModeKHR a =
  case a of
    Vk.VK_PRESENT_MODE_IMMEDIATE_KHR -> ImmediateKHR
    Vk.VK_PRESENT_MODE_MAILBOX_KHR -> MailboxKHR
    Vk.VK_PRESENT_MODE_FIFO_KHR -> FifoKHR
    Vk.VK_PRESENT_MODE_FIFO_RELAXED_KHR -> FifoRelaxedKHR
    Vk.VK_PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR -> SharedDemandRefreshKHR
    Vk.VK_PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR -> SharedContinuousRefreshKHR

unVkPresentModeKHR :: VkPresentModeKHR -> Vk.VkPresentModeKHR
unVkPresentModeKHR a =
  case a of
    ImmediateKHR -> Vk.VK_PRESENT_MODE_IMMEDIATE_KHR
    MailboxKHR -> Vk.VK_PRESENT_MODE_MAILBOX_KHR
    FifoKHR -> Vk.VK_PRESENT_MODE_FIFO_KHR
    FifoRelaxedKHR -> Vk.VK_PRESENT_MODE_FIFO_RELAXED_KHR
    SharedDemandRefreshKHR -> Vk.VK_PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR
    SharedContinuousRefreshKHR -> Vk.VK_PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR

vkGetPhysicalDeviceSurfacePresentModesKHR ::
  MonadIO m =>
  Vk.VkPhysicalDevice ->
  Vk.VkSurfaceKHR ->
  m [VkPresentModeKHR]
vkGetPhysicalDeviceSurfacePresentModesKHR pd surf =
  liftIO $
  Foreign.alloca $ \countPtr -> do
    vkResult =<< Vk.vkGetPhysicalDeviceSurfacePresentModesKHR pd surf countPtr Foreign.nullPtr
    count <- fromIntegral <$> Foreign.peek countPtr
    Foreign.allocaArray count $ \arrayPtr -> do
      vkResult =<< Vk.vkGetPhysicalDeviceSurfacePresentModesKHR pd surf countPtr arrayPtr
      fmap vkPresentModeKHR <$> Foreign.peekArray count arrayPtr
