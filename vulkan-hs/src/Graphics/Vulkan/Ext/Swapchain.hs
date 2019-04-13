{-# language DataKinds, TypeApplications #-}
{-# language ViewPatterns #-}
module Graphics.Vulkan.Ext.Swapchain
  ( Vk.VkSwapchainKHR
  , VkSwapchainCreateFlagKHR(..)
  , vkSwapchainCreateBit, unVkSwapchainCreateBit
  , vkSwapchainCreateBits, unVkSwapchainCreateBits
  , VkSwapchainCreateInfoKHR(..), unVkSwapchainCreateInfoKHR
  , vkCreateSwapchainKHR
  , vkGetSwapchainImagesKHR
  , vkAcquireNextImageKHR
  , VkPresentInfoKHR(..)
  , unVkPresentInfoKHR
  , vkQueuePresentKHR
  )
where

import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Managed.Safe (MonadManaged, using, managed, runManaged)
import Data.Bits ((.&.), (.|.))
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.Word (Word32, Word64)
import Unsafe.Coerce (unsafeCoerce)

import qualified Foreign.Marshal.Alloc as Foreign
import qualified Foreign.Marshal.Array as Foreign
import qualified Foreign.Ptr as Foreign
import qualified Foreign.Storable as Foreign
import qualified Graphics.Vulkan.Ext.VK_KHR_surface as Vk
import qualified Graphics.Vulkan.Ext.VK_KHR_swapchain as Vk

import Graphics.Vulkan.Result (vkResult)

import Graphics.Vulkan.ImageCreateInfo (VkImageUsageFlag, unVkImageUsageBits)
import Graphics.Vulkan.Ext.ColorSpace (VkColorSpaceKHR, unVkColorSpaceKHR)
import Graphics.Vulkan.Ext.Surface
  ( VkSurfaceTransformFlagKHR, VkCompositeAlphaFlagKHR, VkPresentModeKHR
  , unVkSurfaceTransformBit, unVkCompositeAlphaBit, unVkPresentModeKHR
  )
import Graphics.Vulkan.Extent (VkExtent2D, unVkExtent2D)
import Graphics.Vulkan.Format (VkFormat, unVkFormat)
import Graphics.Vulkan.Utils (unVkBool32)
import Graphics.Vulkan.Result (vkResult)
import Graphics.Vulkan.SharingMode (VkSharingMode, unVkSharingMode)

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
    Vk.writeField @"pNext" infoPtr Vk.VK_NULL
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

vkAcquireNextImageKHR ::
  MonadIO m =>
  Vk.VkDevice ->
  Vk.VkSwapchainKHR ->
  Word64 ->
  Maybe Vk.VkSemaphore ->
  Maybe Vk.VkFence ->
  m Word32
vkAcquireNextImageKHR d sc time sem fen =
  liftIO . Foreign.alloca $ \resPtr -> do
    vkResult =<<
      Vk.vkAcquireNextImageKHR
        d
        sc
        time
        (fromMaybe Vk.VK_NULL_HANDLE sem)
        (fromMaybe Vk.VK_NULL_HANDLE fen)
        resPtr
    Foreign.peek resPtr

data VkPresentInfoKHR
  = VkPresentInfoKHR
  { pWaitSemaphores :: [Vk.VkSemaphore]
  , pSwapchains :: [Vk.VkSwapchainKHR]
  , pImageIndices :: [Word32]
  } deriving (Eq, Ord, Show)

unVkPresentInfoKHR ::
  (MonadManaged m, MonadIO m) =>
  VkPresentInfoKHR ->
  m (Int, Foreign.Ptr Vk.VkResult, Vk.VkPresentInfoKHR)
unVkPresentInfoKHR a = do
  let scsCount = length $ pSwapchains a
  resultsPtr <- using $ managed (Foreign.allocaArray scsCount)
  wsPtr <- using $ managed (Foreign.withArray $ pWaitSemaphores a)
  scsPtr <- using $ managed (Foreign.withArray $ pSwapchains a)
  ixsPtr <- using $ managed (Foreign.withArray $ pImageIndices a)
  val <- liftIO . Vk.newVkData $ \ptr -> do
    Vk.writeField @"sType" ptr Vk.VK_STRUCTURE_TYPE_PRESENT_INFO_KHR
    Vk.writeField @"pNext" ptr Vk.VK_NULL
    Vk.writeField @"waitSemaphoreCount" ptr (fromIntegral . length $ pWaitSemaphores a)
    Vk.writeField @"pWaitSemaphores" ptr wsPtr
    Vk.writeField @"swapchainCount" ptr $ fromIntegral scsCount
    Vk.writeField @"pSwapchains" ptr scsPtr
    Vk.writeField @"pImageIndices" ptr ixsPtr
    Vk.writeField @"pResults" ptr resultsPtr
  pure (scsCount, resultsPtr, val)

vkQueuePresentKHR :: MonadIO m => Vk.VkQueue -> VkPresentInfoKHR -> m ()
vkQueuePresentKHR queue info =
  liftIO . runManaged $ do
    (count, ptr, info') <- unVkPresentInfoKHR info
    liftIO $ do
      vkResult =<< Vk.vkQueuePresentKHR queue (Vk.unsafePtr info')
      traverse_ vkResult =<< Foreign.peekArray count ptr
