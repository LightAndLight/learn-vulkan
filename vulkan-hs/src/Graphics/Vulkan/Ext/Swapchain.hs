{-# language DataKinds, TypeApplications #-}
{-# language ViewPatterns #-}
module Graphics.Vulkan.Ext.Swapchain where

import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Managed.Safe (MonadManaged, using, managed)
import Data.Bits ((.&.), (.|.))
import Data.Maybe (fromMaybe)
import Data.Word (Word32)
import Unsafe.Coerce (unsafeCoerce)

import qualified Foreign.Marshal.Alloc as Foreign
import qualified Foreign.Marshal.Array as Foreign
import qualified Foreign.Ptr as Foreign
import qualified Foreign.Storable as Foreign
import qualified Graphics.Vulkan.Ext.VK_KHR_surface as Vk
import qualified Graphics.Vulkan.Ext.VK_KHR_swapchain as Vk

import Graphics.Vulkan.Result (vkResult)

import Graphics.Vulkan.ImageCreateInfo
  ( VkImageUsageFlag, VkSharingMode
  , unVkImageUsageBits, unVkSharingMode
  )
import Graphics.Vulkan.Ext.ColorSpace (VkColorSpaceKHR, unVkColorSpaceKHR)
import Graphics.Vulkan.Ext.Surface
  ( VkSurfaceTransformFlagKHR, VkCompositeAlphaFlagKHR, VkPresentModeKHR
  , unVkSurfaceTransformBit, unVkCompositeAlphaBit, unVkPresentModeKHR
  )
import Graphics.Vulkan.Extent (VkExtent2D, unVkExtent2D)
import Graphics.Vulkan.Format (VkFormat, unVkFormat)
import Graphics.Vulkan.Utils (unVkBool32)
import Graphics.Vulkan.Result (vkResult)

data VkSwapchainCreateFlagKHR
  = SplitInstanceBindRegions
  | Protected
  -- MutableFormat
  deriving (Eq, Ord, Show)

vkSwapchainCreateBit ::
  Vk.VkSwapchainCreateBitmaskKHR a ->
  VkSwapchainCreateFlagKHR
vkSwapchainCreateBit a =
  case a of
    -- https://github.com/achirkin/vulkan/issues/29
    (unsafeCoerce -> Vk.VK_SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR) -> SplitInstanceBindRegions
    (unsafeCoerce -> Vk.VK_SWAPCHAIN_CREATE_PROTECTED_BIT_KHR) -> Protected
    -- Vk.VK_SWAPCHAIN_CREATE_MUTABLE_FORMAT_BIT_KHR -> MutableFormat

unVkSwapchainCreateBit ::
  VkSwapchainCreateFlagKHR ->
  Vk.VkSwapchainCreateBitmaskKHR a
unVkSwapchainCreateBit a =
  case a of
    SplitInstanceBindRegions -> unsafeCoerce Vk.VK_SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR
    Protected -> unsafeCoerce Vk.VK_SWAPCHAIN_CREATE_PROTECTED_BIT_KHR
    -- MutableFormat -> Vk.VK_SWAPCHAIN_CREATE_MUTABLE_FORMAT_BIT_KHR

vkSwapchainCreateBits ::
  Vk.VkSwapchainCreateFlagsKHR ->
  [VkSwapchainCreateFlagKHR]
vkSwapchainCreateBits bs =
  foldr
    (\(mask, val) b -> if mask .&. bs == mask then val : b else b)
    []
    [ (unsafeCoerce Vk.VK_SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR, SplitInstanceBindRegions)
    , (unsafeCoerce Vk.VK_SWAPCHAIN_CREATE_PROTECTED_BIT_KHR, Protected)
    -- , (Vk.VK_SWAPCHAIN_CREATE_MUTABLE_FORMAT_BIT_KHR, MutableFormat)
    ]

unVkSwapchainCreateBits ::
  [VkSwapchainCreateFlagKHR] ->
  Vk.VkSwapchainCreateFlagsKHR
unVkSwapchainCreateBits = foldr (\a b -> unVkSwapchainCreateBit a .|. b) 0

data VkSwapchainCreateInfoKHR
  = VkSwapchainCreateInfoKHR
  { flags :: [VkSwapchainCreateFlagKHR]
  , surface :: Vk.VkSurfaceKHR
  , minImageCount :: Word32
  , imageFormat :: VkFormat
  , imageColorSpace :: VkColorSpaceKHR
  , imageExtent :: VkExtent2D
  , imageArrayLayers :: Word32
  , imageUsage :: [VkImageUsageFlag]
  , imageSharingMode :: VkSharingMode
  , pQueueFamilyIndices :: [Word32]
  , preTransform :: VkSurfaceTransformFlagKHR
  , compositeAlpha :: VkCompositeAlphaFlagKHR
  , presentMode :: VkPresentModeKHR
  , clipped :: Bool
  , oldSwapchain :: Maybe Vk.VkSwapchainKHR
  } deriving (Eq, Ord, Show)

unVkSwapchainCreateInfoKHR ::
  MonadIO m =>
  VkSwapchainCreateInfoKHR ->
  m Vk.VkSwapchainCreateInfoKHR
unVkSwapchainCreateInfoKHR p =
  liftIO $
  Foreign.withArray (pQueueFamilyIndices p) $ \arrayPtr ->
  Vk.newVkData $ \infoPtr -> do
    Vk.writeField @"sType" infoPtr Vk.VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR
    Vk.writeField @"flags" infoPtr (unVkSwapchainCreateBits $ flags p)
    Vk.writeField @"surface" infoPtr (surface p)
    Vk.writeField @"minImageCount" infoPtr (minImageCount p)
    Vk.writeField @"imageFormat" infoPtr (unVkFormat $ imageFormat p)
    Vk.writeField @"imageColorSpace" infoPtr (unVkColorSpaceKHR $ imageColorSpace p)
    Vk.writeField @"imageExtent" infoPtr =<< unVkExtent2D (imageExtent p)
    Vk.writeField @"imageArrayLayers" infoPtr (imageArrayLayers p)
    Vk.writeField @"imageUsage" infoPtr (unVkImageUsageBits $ imageUsage p)
    Vk.writeField @"imageSharingMode" infoPtr (unVkSharingMode $ imageSharingMode p)
    Vk.writeField @"queueFamilyIndexCount" infoPtr (fromIntegral . length $ pQueueFamilyIndices p)
    Vk.writeField @"pQueueFamilyIndices" infoPtr arrayPtr
    Vk.writeField @"preTransform" infoPtr (unVkSurfaceTransformBit $ preTransform p)
    Vk.writeField @"compositeAlpha" infoPtr (unVkCompositeAlphaBit $ compositeAlpha p)
    Vk.writeField @"presentMode" infoPtr (unVkPresentModeKHR $ presentMode p)
    Vk.writeField @"clipped" infoPtr (unVkBool32 $ clipped p)
    Vk.writeField @"oldSwapchain" infoPtr (fromMaybe Vk.VK_NULL_HANDLE $ oldSwapchain p)

vkCreateSwapchainKHR ::
  (MonadManaged m, MonadIO m) =>
  Vk.VkDevice ->
  VkSwapchainCreateInfoKHR ->
  Foreign.Ptr Vk.VkAllocationCallbacks ->
  m Vk.VkSwapchainKHR
vkCreateSwapchainKHR d info cbs = do
  scPtr <- using $ managed Foreign.alloca
  liftIO $ do
    info' <- unVkSwapchainCreateInfoKHR info
    vkResult =<< Vk.vkCreateSwapchainKHR d (Vk.unsafePtr info') cbs scPtr
  using $ managed (bracket
    (Foreign.peek scPtr)
    (\sc -> Vk.vkDestroySwapchainKHR d sc cbs))

vkGetSwapchainImagesKHR ::
  MonadIO m =>
  Vk.VkDevice ->
  Vk.VkSwapchainKHR ->
  m [Vk.VkImage]
vkGetSwapchainImagesKHR d sc =
  liftIO $
  Foreign.alloca $ \countPtr -> do
    vkResult =<< Vk.vkGetSwapchainImagesKHR d sc countPtr Foreign.nullPtr
    count <- fromIntegral <$> Foreign.peek countPtr
    Foreign.allocaArray count $ \arrayPtr -> do
      vkResult =<< Vk.vkGetSwapchainImagesKHR d sc countPtr arrayPtr
      Foreign.peekArray count arrayPtr
