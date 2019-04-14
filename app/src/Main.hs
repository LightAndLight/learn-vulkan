{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language DuplicateRecordFields #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language PatternSynonyms, ViewPatterns #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
module Main where

import Control.Exception (Exception(..), bracket, throwIO, catch)
import Control.Monad (unless, replicateM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Managed.Safe (MonadManaged, using, managed, runManaged)
import Data.Bits ((.&.), shiftL)
import Data.Foldable (fold, for_, traverse_)
import Data.Int (Int32, Int64)
import Data.List (findIndex)
import Data.Maybe (fromMaybe)
import Data.Traversable (for)
import Data.Void (Void)
import Data.Word (Word16, Word32, Word64)
import Graphics.UI.GLFW (ClientAPI(..), WindowHint(..))
import Linear.V2 (V2(..))
import Linear.V3 (V3(..))

import qualified Control.Monad.Managed as Unsafe (with)
import qualified Data.Set as Set
import qualified Foreign.C.String as Foreign
import qualified Foreign.Marshal.Alloc as Foreign
import qualified Foreign.Marshal.Array as Foreign
import qualified Foreign.Ptr as Foreign
import qualified Foreign.Storable as Foreign
import qualified Graphics.UI.GLFW as GLFW

import Data.Some (Some(..))
import Graphics.Vulkan.ApplicationInfo (VkApplicationInfo(..))
import Graphics.Vulkan.Buffer
  ( VkBuffer
  , VkBufferCreateInfo(..)
  , VkBufferUsageFlag(..)
  , VkBufferCreateFlag(..)
  )
import Graphics.Vulkan.CommandBuffer
  ( VkCommandBuffer
  , VkCommandBufferAllocateInfo(..), VkCommandBufferLevel(..), vkAllocateCommandBuffers
  , VkCommandBufferBeginInfo(..), VkCommandBufferUsageFlag(..), withCommandBuffer
  )
import Graphics.Vulkan.CommandPool (VkCommandPool, vkCreateCommandPool)
import Graphics.Vulkan.CommandPoolCreateInfo (VkCommandPoolCreateInfo(..))
import Graphics.Vulkan.Command.BindPipeline (vkCmdBindPipeline)
import Graphics.Vulkan.Command.BindIndexBuffer (vkCmdBindIndexBuffer)
import Graphics.Vulkan.Command.BindVertexBuffers (vkCmdBindVertexBuffers)
import Graphics.Vulkan.Command.Draw (vkCmdDrawIndexed)
import Graphics.Vulkan.Command.RenderPass (VkRenderPassBeginInfo(..), withCmdRenderPass)
import Graphics.Vulkan.Device
  ( VkDevice, vkCreateDevice, vkGetDeviceQueue, vkDeviceWaitIdle
  )
import Graphics.Vulkan.DeviceCreateInfo (VkDeviceCreateInfo(..))
import Graphics.Vulkan.DeviceQueueCreateInfo
  (VkDeviceQueueCreateInfo(..))
import Graphics.Vulkan.Ext
  ( VkInstanceExtension(..)
  , glfwGetRequiredInstanceExtensions
  , VkDeviceExtension(..)
  , vkEnumerateDeviceExtensionProperties
  )
import Graphics.Vulkan.Ext.ColorSpace (VkColorSpaceKHR(..))
import Graphics.Vulkan.Ext.DebugUtils
  ( VkDebugUtilsMessengerEXT
  , VkDebugUtilsMessengerCreateInfoEXT(..)
  , VkDebugUtilsMessageSeverity(..)
  , VkDebugUtilsMessageType(..)
  , mkDebugUtilsMessenger
  )
import Graphics.Vulkan.Ext.Surface
  ( VkSurfaceKHR
  , VkSurfaceFormatKHR(..)
  , VkPresentModeKHR(..)
  , VkSurfaceCapabilitiesKHR(..)
  , VkCompositeAlphaFlagKHR(..)
  , glfwCreateWindowSurface
  , vkGetPhysicalDeviceSurfaceSupportKHR
  , vkGetPhysicalDeviceSurfaceCapabilitiesKHR
  , vkGetPhysicalDeviceSurfaceFormatsKHR
  , vkGetPhysicalDeviceSurfacePresentModesKHR
  )
import Graphics.Vulkan.Ext.Swapchain
  ( VkSwapchainKHR
  , VkSwapchainCreateInfoKHR(..)
  , vkCreateSwapchainKHR, vkGetSwapchainImagesKHR
  , vkAcquireNextImageKHR
  , VkPresentInfoKHR(..), vkQueuePresentKHR
  )
import Graphics.Vulkan.Extent (VkExtent2D(..))
import Graphics.Vulkan.Format (VkFormat(..))
import Graphics.Vulkan.Fence
  (VkFence, VkFenceCreateInfo(..), VkFenceCreateFlag(..)
  , vkCreateFence
  , vkWaitForFences
  , vkResetFences
  )
import Graphics.Vulkan.Framebuffer (VkFramebuffer, vkCreateFramebuffer)
import Graphics.Vulkan.FramebufferCreateInfo (VkFramebufferCreateInfo(..))
import Graphics.Vulkan.GraphicsPipelineCreateInfo (VkGraphicsPipelineCreateInfo(..))
import Graphics.Vulkan.GraphicsPipeline (VkPipeline, vkCreateGraphicsPipelines)
import Graphics.Vulkan.IndexType (VkIndexType(..))
import Graphics.Vulkan.ImageCreateInfo (VkImageUsageFlag(..))
import Graphics.Vulkan.ImageView (VkImageView, vkCreateImageView)
import Graphics.Vulkan.ImageViewCreateInfo
  ( VkImageViewCreateInfo(..), VkImageSubresourceRange(..), VkComponentMapping(..)
  , VkImageAspectFlag(..), VkComponentSwizzle(..), VkImageViewType(..)
  )
import Graphics.Vulkan.Instance (VkInstance, mkInstance)
import Graphics.Vulkan.InstanceCreateInfo (VkInstanceCreateInfo(..))
import Graphics.Vulkan.Layer
  ( VkLayer(..), vkLayer, unVkLayer
  )
import Graphics.Vulkan.Offset (VkOffset2D(..))
import Graphics.Vulkan.PhysicalDevice
  ( VkDeviceSize
  , VkPhysicalDevice
  , VkMemoryPropertyFlag(..)
  , vkEnumeratePhysicalDevices
  , vkGetPhysicalDeviceProperties
  , vkGetPhysicalDeviceFeatures
  , vkGetPhysicalDeviceQueueFamilyProperties
  )
import Graphics.Vulkan.Pipeline.ColorBlendAttachmentState
  ( VkPipelineColorBlendAttachmentState(..)
  , VkColorComponentFlag(..), VkBlendOp(..), VkBlendFactor(..)
  )
import Graphics.Vulkan.Pipeline.ColorBlendStateCreateInfo
  ( VkPipelineColorBlendStateCreateInfo(..), VkLogicOp(..)
  )
import Graphics.Vulkan.Pipeline.Layout (VkPipelineLayout, vkCreatePipelineLayout)
import Graphics.Vulkan.Pipeline.LayoutCreateInfo
  ( VkPipelineLayoutCreateInfo(..)
  )
import Graphics.Vulkan.Pipeline.MultisampleStateCreateInfo
  ( VkPipelineMultisampleStateCreateInfo(..)
  )
import Graphics.Vulkan.Pipeline.RasterizationStateCreateInfo
  ( VkPipelineRasterizationStateCreateInfo(..)
  , VkFrontFace(..), VkCullModeFlag(..), VkPolygonMode(..)
  )
import Graphics.Vulkan.Pipeline.ShaderStageCreateInfo
  ( VkPipelineShaderStageCreateInfo(..)
  )
import Graphics.Vulkan.Pipeline.VertexInputStateCreateInfo
  ( VkPipelineVertexInputStateCreateInfo(..)
  , VkVertexInputBindingDescription(..)
  , VkVertexInputAttributeDescription(..)
  )
import Graphics.Vulkan.Pipeline.ViewportStateCreateInfo (VkPipelineViewportStateCreateInfo(..))
import Graphics.Vulkan.Pipeline.InputAssemblyStateCreateInfo
  (VkPipelineInputAssemblyStateCreateInfo(..), VkPrimitiveTopology(..))
import Graphics.Vulkan.Queue (VkQueue, VkQueueFamilyProperties(..), VkSubmitInfo(..), vkQueueSubmit)
import Graphics.Vulkan.Rect (VkRect2D(..))
import Graphics.Vulkan.RenderPass (VkRenderPass, vkCreateRenderPass)
import Graphics.Vulkan.RenderPassCreateInfo
  ( VkRenderPassCreateInfo(..)
  , VkAttachmentDescription(..)
  , VkSubpassDescription(..)
  , VkSubpassDependency(..)
  , VkAttachmentReference(..)
  , VkAttachmentStoreOp(..)
  )
import Graphics.Vulkan.Result (VkError(..), vkResult)
import Graphics.Vulkan.SampleCount (VkSampleCount(..))
import Graphics.Vulkan.Semaphore (VkSemaphore, VkSemaphoreCreateInfo(..), vkCreateSemaphore)
import Graphics.Vulkan.ShaderModule (VkShaderModule, shaderModuleFromFile)
import Graphics.Vulkan.SharingMode (VkSharingMode(..))
import Graphics.Vulkan.Version (_VK_MAKE_VERSION)
import Graphics.Vulkan.Viewport (VkViewport(..))

import qualified Graphics.Vulkan.Access as Access (VkAccessFlag(..))
import qualified Graphics.Vulkan.ClearValue as ClearValue (VkClearValue(..), VkClearColorValue(..))
import qualified Graphics.Vulkan.Ext.Surface as SurfaceCapabilities (VkSurfaceCapabilitiesKHR(..))
import qualified Graphics.Vulkan.Ext.Surface as SurfaceFormat (VkSurfaceFormatKHR(..))
import qualified Graphics.Vulkan.Extent as Extent2D (VkExtent2D(..))
import qualified Graphics.Vulkan.ImageLayout as ImageLayout (VkImageLayout(..))
import qualified Graphics.Vulkan.Ext.DebugUtils as MessageType (VkDebugUtilsMessageType(..))
import qualified Graphics.Vulkan.GraphicsPipelineCreateInfo as Stages (Stages(..))
import qualified Graphics.Vulkan.RenderPassCreateInfo as LoadOp (VkAttachmentLoadOp(..))
import qualified Graphics.Vulkan.Pipeline.BindPoint as BindPoint (VkPipelineBindPoint(..))
import qualified Graphics.Vulkan.Pipeline.VertexInputStateCreateInfo as InputRate
  (VkVertexInputRate(..))
import qualified Graphics.Vulkan.PipelineStage as PipelineStage (VkPipelineStageFlag(..))
import qualified Graphics.Vulkan.ShaderStage as ShaderStage (VkShaderStageFlag(..))
import qualified Graphics.Vulkan.SubpassContents as Subpass (VkSubpassContents(..))
import qualified Graphics.Vulkan.Queue as QueueType (VkQueueType(..))

import Vertex
import Subresources
  ( AllocateSubresourcesInfo(..)
  , InitSubresource(..)
  , SubresourceLoc
  , Subresources(..)
  , allocateSubresources
  )
import qualified Subresources as SubresourceLoc (SubresourceLoc(..))

framesInFlight :: Int
framesInFlight = 2

data LoopState = Quit | Recreate

mainLoop ::
  MonadIO m =>
  GLFW.Window ->
  (forall m. MonadManaged m => m (VkSwapchainKHR, [VkCommandBuffer])) ->
  VkDevice ->
  Queues ->
  [(VkSemaphore, VkSemaphore, VkFence)] ->
  m ()
mainLoop window mkSC device qs syncObjects = do
  next <- liftIO . flip Unsafe.with pure $ do
    (swapchain, commandBuffers) <- mkSC
    go swapchain commandBuffers (cycle syncObjects) <*
      vkDeviceWaitIdle device
  case next of
    Quit -> pure ()
    Recreate -> mainLoop window mkSC device qs syncObjects
  where
    go ::
      MonadManaged m =>
      VkSwapchainKHR ->
      [VkCommandBuffer] ->
      [(VkSemaphore, VkSemaphore, VkFence)] ->
      m LoopState
    go swapchain cmdBuffers ((imageAvailableSem, renderFinishedSem, inFlightFence) : rest) = do
      close <- liftIO $ GLFW.windowShouldClose window
      if close
        then pure Quit
        else do
          liftIO GLFW.pollEvents

          vkWaitForFences device [inFlightFence] True maxBound

          m_ix <-
            liftIO $
            catch
              (Just <$>
               vkAcquireNextImageKHR
                 device
                 swapchain
                 maxBound
                 (Just imageAvailableSem)
                 Nothing)
              (\err -> do
                 case err of
                   OutOfDateKHR -> pure Nothing
                   _ -> throwIO err)

          case m_ix of
            Nothing -> pure Recreate
            Just ix -> do
              let
                submitInfo =
                  VkSubmitInfo
                  { pWaitSemaphores = [imageAvailableSem]
                  , pWaitDstStageMask = [[PipelineStage.ColorAttachmentOutput]]
                  , pCommandBuffers = [cmdBuffers !! fromIntegral ix]
                  , pSignalSemaphores = [renderFinishedSem]
                  }
              vkResetFences device [inFlightFence]
              vkQueueSubmit (graphicsQ qs) [submitInfo] (Just inFlightFence)
              let
                presentInfo =
                  VkPresentInfoKHR
                  { pWaitSemaphores = [renderFinishedSem]
                  , pSwapchains = [swapchain]
                  , pImageIndices = [ix]
                  }
              vkQueuePresentKHR (presentQ qs) presentInfo
              go swapchain cmdBuffers rest

vulkanGLFW :: IO a -> IO a
vulkanGLFW m = do
  initSucceeded <- GLFW.init
  unless initSucceeded $ error "glfw init failed"
  vkSupported <- GLFW.vulkanSupported
  unless vkSupported $ error "glfw vulkan not supported"
  m <* GLFW.terminate

mkWindow ::
  (MonadManaged m, MonadIO m) =>
  [GLFW.WindowHint] ->
  Int ->
  Int ->
  String ->
  Maybe GLFW.Monitor ->
  Maybe GLFW.Window ->
  m GLFW.Window
mkWindow hints w h name mMonitor mWindow = do
  liftIO $ traverse_ GLFW.windowHint hints
  using $ managed (bracket (liftIO create) GLFW.destroyWindow)
  where
    create :: IO GLFW.Window
    create =
      fromMaybe (error "glfw window create failed") <$>
      GLFW.createWindow w h name mMonitor mWindow

ifindIndexM ::
  (Num ix, Monad m) =>
  (ix -> a -> m Bool) ->
  [a] ->
  m (Maybe ix)
ifindIndexM p = go 0
  where
    go !n [] = pure Nothing
    go !n (x:xs) = do
      b <- p n x
      if b
        then pure $ Just n
        else go (n+1) xs


initInstance :: MonadManaged m => m VkInstance
initInstance = do
  requiredExts <- glfwGetRequiredInstanceExtensions

  let
    instanceInfo =
      VkInstanceCreateInfo
      { pApplicationInfo =
        VkApplicationInfo
        { pApplicationName = "Demo"
        , applicationVersion = _VK_MAKE_VERSION 1 0 0
        , pEngineName = "No Engine"
        , engineVersion = _VK_MAKE_VERSION 1 0 0
        , apiVersion = _VK_MAKE_VERSION 1 0 82
        }
      , ppEnabledLayerNames =
          [LunargStandardValidation]
      , ppEnabledExtensionNames = DebugUtils : requiredExts
      }

  mkInstance instanceInfo Foreign.nullPtr

initDebugMessenger :: MonadManaged m => VkInstance -> m VkDebugUtilsMessengerEXT
initDebugMessenger inst = do
  let
    messengerInfo =
      VkDebugUtilsMessengerCreateInfoEXT
      { messageSeverity = [Verbose, Warning, Error]
      , messageType =
        [ MessageType.General
        , MessageType.Validation
        , MessageType.Performance
        ]
      , pfnUserCallback = \sev types cbData userData -> do
          putStrLn "debug callback:"
          print sev
          print types
          print cbData
          print userData
          pure False
      , pUserData = ()
      }
  mkDebugUtilsMessenger @() inst messengerInfo Foreign.nullPtr

initPhysicalDevice :: MonadManaged m => VkInstance -> m VkPhysicalDevice
initPhysicalDevice inst =
  (\case; [] -> error "vulkan no devices"; d:_ -> d) <$>
  vkEnumeratePhysicalDevices inst

data QueueFamilyIndices
  = QFIxs
  { graphicsQfIx :: Word32
  , presentQfIx :: Word32
  }

initQueueFamilies :: MonadIO m => VkSurfaceKHR -> VkPhysicalDevice -> m QueueFamilyIndices
initQueueFamilies surf physicalDevice = do
  qfProps <- vkGetPhysicalDeviceQueueFamilyProperties physicalDevice

  graphicsQfIx <-
    maybe (error "vulkan no suitable queue families") (pure . fromIntegral) $
    findIndex (elem QueueType.Graphics . queueFlags) qfProps

  presentQfIx <-
    maybe (error "vulkan no suitable queue families") (pure . fromIntegral) =<<
    ifindIndexM
      (\ix p -> do
          vkGetPhysicalDeviceSurfaceSupportKHR
            physicalDevice
            ix
            surf)
      qfProps

  pure $ QFIxs graphicsQfIx presentQfIx

initDevice :: MonadManaged m => VkPhysicalDevice -> QueueFamilyIndices -> m VkDevice
initDevice physDevice qfIxs = do
  dFeatures <- vkGetPhysicalDeviceFeatures physDevice

  let
    deviceInfo =
      VkDeviceCreateInfo
      { flags = []
      , pQueueCreateInfos =
        foldr
          (\ix rest ->
            VkDeviceQueueCreateInfo
            { flags = []
            , queueFamilyIndex = ix
            , queueCount = 1
            , pQueuePriorities = [1.0]
            } :
            rest)
          []
          (Set.fromList [graphicsQfIx qfIxs, presentQfIx qfIxs])
      , ppEnabledLayerNames = []
      , ppEnabledExtensionNames = [Swapchain]
      , pEnabledFeatures = dFeatures
      }

  vkCreateDevice physDevice deviceInfo Foreign.nullPtr

data Queues
  = Qs
  { graphicsQ :: VkQueue
  , presentQ :: VkQueue
  }

initQueues :: MonadManaged m => VkDevice -> QueueFamilyIndices -> m Queues
initQueues device qfIxs = do
  gq <- vkGetDeviceQueue device (graphicsQfIx qfIxs) 0
  pq <- vkGetDeviceQueue device (presentQfIx qfIxs) 0
  pure $ Qs gq pq

data SwapchainConfig
  = SCC
  { scExtent :: VkExtent2D
  , scFormat :: VkSurfaceFormatKHR
  }

initSwapchain ::
  MonadManaged m =>
  GLFW.Window ->
  VkPhysicalDevice ->
  QueueFamilyIndices ->
  VkSurfaceKHR ->
  VkDevice ->
  m (VkSwapchainKHR, SwapchainConfig)
initSwapchain window physDevice qfIxs surf device = do
  availableFormats <- vkGetPhysicalDeviceSurfaceFormatsKHR physDevice surf
  surfaceFormat <-
    case availableFormats of
      [] -> error "vulkan no surface formats"
      h:_ ->
        if
          SurfaceFormat.format h == UNDEFINED ||
          preferredFormat `elem` availableFormats
        then pure preferredFormat
        else error "vulkan couldn't find preferred format"

  availablePresentModes <- vkGetPhysicalDeviceSurfacePresentModesKHR physDevice surf
  surfaceCapabilities <- vkGetPhysicalDeviceSurfaceCapabilitiesKHR physDevice surf

  let
    presentMode = mkPresentMode availablePresentModes
    imageCount = mkImageCount surfaceCapabilities
    differentQfIxs = graphicsQfIx qfIxs /= presentQfIx qfIxs

  swapExtent <- liftIO $ mkSwapExtent surfaceCapabilities

  let
    swapchainCreateInfo =
      VkSwapchainCreateInfoKHR
      { flags = []
      , surface = surf
      , minImageCount = imageCount
      , imageFormat = SurfaceFormat.format surfaceFormat
      , imageColorSpace = colorSpace surfaceFormat
      , imageExtent = swapExtent
      , imageArrayLayers = 1
      , imageUsage = [ColorAttachment]
      , imageSharingMode =
          if differentQfIxs
          then Concurrent
          else Exclusive
      , pQueueFamilyIndices =
          if differentQfIxs
          then [graphicsQfIx qfIxs, presentQfIx qfIxs]
          else []
      , preTransform = currentTransform surfaceCapabilities
      , compositeAlpha = Opaque
      , presentMode = presentMode
      , clipped = True
      , oldSwapchain = Nothing
      }

  sc <- vkCreateSwapchainKHR device swapchainCreateInfo Foreign.nullPtr
  pure (sc, SCC swapExtent surfaceFormat)

  where

    preferredFormat =
      VkSurfaceFormatKHR
      { format = B8G8R8A8_UNORM
      , colorSpace = SRGB_NONLINEAR_KHR
      }

    mkPresentMode pms =
      if MailboxKHR `elem` pms
      then MailboxKHR
      else
        if FifoKHR `elem` pms
        then FifoKHR
        else ImmediateKHR

    mkImageCount scs =
      if SurfaceCapabilities.maxImageCount scs == 0
      then SurfaceCapabilities.minImageCount scs + 1
      else min (SurfaceCapabilities.minImageCount scs + 1) (SurfaceCapabilities.maxImageCount scs)

    mkSwapExtent scs =
      if Extent2D.width (currentExtent scs) /= maxBound
      then pure $ currentExtent scs
      else do
        (w, h) <- GLFW.getFramebufferSize window
        pure $ VkExtent2D { width = fromIntegral w, height = fromIntegral h }

initImageViews :: MonadManaged m => VkDevice -> VkSwapchainKHR -> SwapchainConfig -> m [VkImageView]
initImageViews device swapchain scConfig = do
  images <- vkGetSwapchainImagesKHR device swapchain

  let
    imageViewCreateInfo img =
      VkImageViewCreateInfo
      { flags = []
      , image = img
      , viewType = TwoD
      , format = SurfaceFormat.format $ scFormat scConfig
      , components =
          VkComponentMapping
          { r = SwizzleIdentity
          , g = SwizzleIdentity
          , b = SwizzleIdentity
          , a = SwizzleIdentity
          }
      , subresourceRange =
          VkImageSubresourceRange
          { aspectMask = [Color]
          , baseMipLevel = 0
          , levelCount = 1
          , baseArrayLayer = 0
          , layerCount = 1
          }
      }

  traverse
    (\i -> vkCreateImageView device (imageViewCreateInfo i) Foreign.nullPtr)
    images

initPipelineLayout :: MonadManaged m => VkDevice -> m VkPipelineLayout
initPipelineLayout device = do
  let
    layoutInfo =
      VkPipelineLayoutCreateInfo
      { flags = []
      , pSetLayouts = []
      , pPushConstantRanges = []
      }

  vkCreatePipelineLayout device layoutInfo Foreign.nullPtr

initRenderPass :: MonadManaged m => VkDevice -> SwapchainConfig -> m VkRenderPass
initRenderPass device scConfig = do
  let
    renderPassInfo =
      VkRenderPassCreateInfo
      { flags = []
      , pAttachments =
        [ VkAttachmentDescription
          { flags = []
          , format = SurfaceFormat.format $ scFormat scConfig
          , samples = SC1
          , loadOp = LoadOp.Clear
          , storeOp = Store
          , stencilLoadOp = LoadOp.LoadOpDontCare
          , stencilStoreOp = StoreOpDontCare
          , initialLayout = ImageLayout.Undefined
          , finalLayout = ImageLayout.PresentSrcKHR
          }
        ]
      , pSubpasses =
        [ VkSubpassDescription
          { flags = []
          , pipelineBindPoint = BindPoint.Graphics
          , pInputAttachments = []
          , pColorAttachments =
            [ VkAttachmentReference
              { attachment = Just 0
              , layout = ImageLayout.ColorAttachmentOptimal
              }
            ]
          , pResolveAttachments = Nothing
          , pDepthStencilAttachment = Nothing
          , pPreserveAttachments = []
          }
        ]
      , pDependencies =
        [ VkSubpassDependency
          { srcSubpass = Nothing
          , dstSubpass = Just 0
          , srcStageMask = [PipelineStage.ColorAttachmentOutput]
          , dstStageMask = [PipelineStage.ColorAttachmentOutput]
          , srcAccessMask = []
          , dstAccessMask = [Access.ColorAttachmentRead, Access.ColorAttachmentWrite]
          , dependencyFlags = []
          }
        ]
      }

  vkCreateRenderPass device renderPassInfo Foreign.nullPtr

initPipeline ::
  MonadManaged m =>
  VkDevice ->
  VkShaderModule ->
  VkShaderModule ->
  SwapchainConfig ->
  VkPipelineLayout ->
  VkRenderPass ->
  m VkPipeline
initPipeline device vert frag scConfig pipelineLayout renderPass = do
  let
    vertShaderStageInfo :: VkPipelineShaderStageCreateInfo '[]
    vertShaderStageInfo =
      VkPipelineShaderStageCreateInfo
      { flags = []
      , stage = ShaderStage.Vertex
      , module_ = vert
      , pName = "main"
      , pSpecializationInfo = Nothing
      }

    fragShaderStageInfo :: VkPipelineShaderStageCreateInfo '[]
    fragShaderStageInfo =
      VkPipelineShaderStageCreateInfo
      { flags = []
      , stage = ShaderStage.Fragment
      , module_ = frag
      , pName = "main"
      , pSpecializationInfo = Nothing
      }

    vertexInputInfo =
      VkPipelineVertexInputStateCreateInfo
      { flags = []
      , pVertexBindingDescriptions =
        [ VkVertexInputBindingDescription
          { binding = 0
          , stride = vertexStride
          , inputRate = InputRate.Vertex
          }
        ]
      , pVertexAttributeDescriptions =
        [ VkVertexInputAttributeDescription
          { location = 0
          , binding = 0
          , format = R32G32_SFLOAT
          , offset = vPosOffset
          }
        , VkVertexInputAttributeDescription
          { location = 1
          , binding = 0
          , format = R32G32B32_SFLOAT
          , offset = vColorOffset
          }
        ]
      }

    inputAssemblyInfo =
      VkPipelineInputAssemblyStateCreateInfo
      { flags = []
      , topology = TriangleList
      , primitiveRestartEnable = False
      }

    viewportInfo =
      VkPipelineViewportStateCreateInfo
      { flags = []
      , pViewports =
        [ VkViewport
          { x = 0
          , y = 0
          , width = fromIntegral . Extent2D.width $ scExtent scConfig
          , height = fromIntegral . Extent2D.height $ scExtent scConfig
          , minDepth = 0
          , maxDepth = 1
          }
        ]
      , pScissors =
        [ VkRect2D
          { offset = VkOffset2D 0 0
          , extent = scExtent scConfig
          }
        ]
      }

    rasterizationInfo =
      VkPipelineRasterizationStateCreateInfo
      { flags = []
      , depthClampEnable = False
      , rasterizerDiscardEnable = False
      , polygonMode = Fill
      , cullMode = [Back]
      , frontFace = Clockwise
      , depthBiasEnable = False
      , depthBiasConstantFactor = 0
      , depthBiasClamp = 0
      , depthBiasSlopeFactor = 0
      , lineWidth = 1
      }

    multisampleInfo =
      VkPipelineMultisampleStateCreateInfo
      { flags = []
      , rasterizationSamples = SC1
      , sampleShadingEnable = False
      , minSampleShading = 0
      , pSampleMask = Nothing
      , alphaToCoverageEnable = False
      , alphaToOneEnable = False
      }

    colorBlendInfo =
      VkPipelineColorBlendStateCreateInfo
      { flags = []
      , logicOpEnable = False
      , logicOp = Copy
      , pAttachments =
        [ VkPipelineColorBlendAttachmentState
          { blendEnable = False
          , srcColorBlendFactor = One
          , dstColorBlendFactor = Zero
          , colorBlendOp = Add
          , srcAlphaBlendFactor = One
          , dstAlphaBlendFactor = Zero
          , alphaBlendOp = Add
          , colorWriteMask = [R, G, B, A]
          }
        ]
      , blendConstants = (0, 0, 0, 0)
      }

    pipelineInfo =
      VkGraphicsPipelineCreateInfo
      { flags = []
      , pStages =
        Stages.Cons vertShaderStageInfo $
        Stages.Cons fragShaderStageInfo $
        Stages.Nil
      , pVertexInputState = Just vertexInputInfo
      , pInputAssemblyState = Just inputAssemblyInfo
      , pTessellationState = Nothing
      , pViewportState = Just viewportInfo
      , pRasterizationState = Just rasterizationInfo
      , pMultisampleState = Just multisampleInfo
      , pDepthStencilState = Nothing
      , pColorBlendState = Just colorBlendInfo
      , pDynamicState = Nothing
      , layout = pipelineLayout
      , renderPass = renderPass
      , subpass = 0
      , basePipelineHandle = Nothing
      , basePipelineIndex = Nothing
      }

  fmap (\case; [] -> error "vulkan no graphics pipeline"; p:_ -> p) $
    vkCreateGraphicsPipelines device Nothing [Some pipelineInfo] Foreign.nullPtr

initFramebuffers ::
  MonadManaged m =>
  VkDevice ->
  [VkImageView] ->
  VkRenderPass ->
  SwapchainConfig ->
  m [VkFramebuffer]
initFramebuffers device imageViews renderPass scConfig =
  for imageViews $ \imageView -> do
    let
      framebufferInfo =
        VkFramebufferCreateInfo
        { flags = []
        , renderPass = renderPass
        , pAttachments = [imageView]
        , width = Extent2D.width $ scExtent scConfig
        , height = Extent2D.height $ scExtent scConfig
        , layers = 1
        }
    vkCreateFramebuffer device framebufferInfo Foreign.nullPtr

initCommandPool :: MonadManaged m => VkDevice -> Word32 -> m VkCommandPool
initCommandPool device ix = do
  let
    commandPoolInfo =
      VkCommandPoolCreateInfo
      { flags = []
      , queueFamilyIndex = ix
      }

  vkCreateCommandPool device commandPoolInfo Foreign.nullPtr

vertices :: [Vertex]
vertices =
  [ Vertex { vPos = V2 (-0.5) 0.5, vColor = V3 1 0 0 }
  , Vertex { vPos = V2 (-0.5) (-0.5), vColor = V3 0 1 0 }
  , Vertex { vPos = V2 0.5 (-0.5), vColor = V3 1 1 0 }
  , Vertex { vPos = V2 0.5 0.5, vColor = V3 0 0 1 }
  ]

indices :: [Word16]
indices = [ 1, 2, 3, 3, 0, 1 ]

initBuffers ::
  MonadManaged m =>
  VkPhysicalDevice ->
  VkDevice ->
  m (VkBuffer, SubresourceLoc Vertex, SubresourceLoc Word16)
initBuffers physDev dev = do
  let
    srInfo =
      AllocateSubresourcesInfo
      { flags = []
      , usage = [VertexBuffer, IndexBuffer]
      , subresources =
          SubCons (InitFull vertices) $
          SubCons (InitFull indices) $
          SubNil
      , memoryProperties = [HostVisible, HostCoherent]
      , sharingMode = Exclusive
      , pQueueFamilyIndices = []
      }
  (buf, srs) <- allocateSubresources physDev dev srInfo
  case srs of; SubCons vLoc (SubCons iLoc SubNil) -> pure (buf, vLoc, iLoc)

initCommandBuffers ::
  MonadManaged m =>
  VkDevice ->
  VkCommandPool ->
  [VkFramebuffer] ->
  VkRenderPass ->
  SwapchainConfig ->
  VkPipeline ->
  VkBuffer ->
  SubresourceLoc Vertex ->
  SubresourceLoc Word16 ->
  m [VkCommandBuffer]
initCommandBuffers device commandPool framebuffers renderPass scConfig pipeline vBuf vLoc iLoc = do
  let
    commandBufferInfo =
      VkCommandBufferAllocateInfo
      { commandPool = commandPool
      , level = Primary
      , commandBufferCount = fromIntegral $ length framebuffers
      }

  commandBuffers <- vkAllocateCommandBuffers device commandBufferInfo

  for_ (zip commandBuffers framebuffers) $ \(cmdBuf, fbuf) -> do
    let
      beginInfo =
        VkCommandBufferBeginInfo
        { flags = [SimultaneousUse]
        , pInheritanceInfo = Nothing
        }

      renderPassBeginInfo =
        VkRenderPassBeginInfo
        { renderPass = renderPass
        , framebuffer = fbuf
        , renderArea = VkRect2D (VkOffset2D 0 0) (scExtent scConfig)
        , pClearValues = [ClearValue.Color (ClearValue.Float32 1 1 1 1)]
        }

    withCommandBuffer cmdBuf beginInfo $
      withCmdRenderPass cmdBuf renderPassBeginInfo Subpass.Inline $ do
        vkCmdBindPipeline cmdBuf BindPoint.Graphics pipeline
        vkCmdBindVertexBuffers cmdBuf 0 [(vBuf, SubresourceLoc.offset vLoc)]
        vkCmdBindIndexBuffer
          cmdBuf
          vBuf
          (SubresourceLoc.offset iLoc)
          IndexUInt16
        vkCmdDrawIndexed cmdBuf (fromIntegral $ length indices) 1 0 0 0

  pure commandBuffers

initSyncObjects :: MonadManaged m => VkDevice -> m [(VkSemaphore, VkSemaphore, VkFence)]
initSyncObjects dev =
  replicateM framesInFlight $ do
    iaSem <- vkCreateSemaphore dev (VkSemaphoreCreateInfo []) Foreign.nullPtr
    rfSem <- vkCreateSemaphore dev (VkSemaphoreCreateInfo []) Foreign.nullPtr
    fence <- vkCreateFence dev (VkFenceCreateInfo [Signaled]) Foreign.nullPtr
    pure (iaSem, rfSem, fence)

recreateSwapchain ::
  MonadManaged m =>
  GLFW.Window ->
  VkPhysicalDevice ->
  VkDevice ->
  QueueFamilyIndices ->
  VkSurfaceKHR ->
  VkShaderModule ->
  VkShaderModule ->
  VkPipelineLayout ->
  VkCommandPool ->
  VkBuffer ->
  SubresourceLoc Vertex ->
  SubresourceLoc Word16 ->
  m (VkSwapchainKHR, [VkCommandBuffer])
recreateSwapchain window physDev dev qfIxs surf vert frag pipeLayout cmdPool vBuf vLoc iLoc = do
  (swapchain, scConfig) <- initSwapchain window physDev qfIxs surf dev
  imageViews <- initImageViews dev swapchain scConfig
  renderPass <- initRenderPass dev scConfig
  pipeline <- initPipeline dev vert frag scConfig pipeLayout renderPass
  framebuffers <- initFramebuffers dev imageViews renderPass scConfig
  commandBuffers <-
    initCommandBuffers dev cmdPool framebuffers renderPass scConfig pipeline vBuf vLoc iLoc

  pure (swapchain, commandBuffers)

main :: IO ()
main =
  vulkanGLFW . runManaged $ do
    window <- mkWindow hints 1280 960 "vulkan" Nothing Nothing

    instance_ <- initInstance
    messenger <- initDebugMessenger instance_
    surface <- glfwCreateWindowSurface instance_ window Foreign.nullPtr
    physicalDevice <- initPhysicalDevice instance_

    qfIxs <- initQueueFamilies surface physicalDevice

    device <- initDevice physicalDevice qfIxs

    qs <- initQueues device qfIxs

    vert <- shaderModuleFromFile "app/shaders/vert.spv" [] device Foreign.nullPtr
    frag <- shaderModuleFromFile "app/shaders/frag.spv" [] device Foreign.nullPtr
    pipelineLayout <- initPipelineLayout device
    commandPool <- initCommandPool device (graphicsQfIx qfIxs)
    syncObjects <- initSyncObjects device

    (buffer, vLoc, iLoc) <- initBuffers physicalDevice device

    mainLoop
      window
      (recreateSwapchain
         window
         physicalDevice
         device
         qfIxs
         surface
         vert
         frag
         pipelineLayout
         commandPool
         buffer
         vLoc
         iLoc)
      device
      qs
      syncObjects
  where
    hints =
      [ WindowHint'ClientAPI ClientAPI'NoAPI
      -- , WindowHint'Resizable False
      ]
