{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language PatternSynonyms, ViewPatterns #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
module Main where

import Control.Exception (Exception(..), bracket, throwIO)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Managed.Safe (MonadManaged, using, managed, runManaged)
import Data.Foldable (fold, for_, traverse_)
import Data.Int (Int32, Int64)
import Data.List (findIndex)
import Data.Maybe (fromMaybe)
import Data.Traversable (for)
import Data.Void (Void)
import Data.Word (Word32, Word64)
import Graphics.UI.GLFW (ClientAPI(..), WindowHint(..))

import qualified Data.Set as Set
import qualified Foreign.C.String as Foreign
import qualified Foreign.Marshal.Alloc as Foreign
import qualified Foreign.Marshal.Array as Foreign
import qualified Foreign.Ptr as Foreign
import qualified Foreign.Storable as Foreign
import qualified Graphics.UI.GLFW as GLFW

import Data.Some (Some(..))
import Graphics.Vulkan.ApplicationInfo (VkApplicationInfo(..))
import Graphics.Vulkan.CommandBuffer
  ( VkCommandBuffer
  , VkCommandBufferAllocateInfo(..), VkCommandBufferLevel(..), vkAllocateCommandBuffers
  , VkCommandBufferBeginInfo(..), VkCommandBufferUsageFlag(..), withCommandBuffer
  )
import Graphics.Vulkan.CommandPool (vkCreateCommandPool)
import Graphics.Vulkan.CommandPoolCreateInfo (VkCommandPoolCreateInfo(..))
import Graphics.Vulkan.Command.BindPipeline (vkCmdBindPipeline)
import Graphics.Vulkan.Command.Draw (vkCmdDraw)
import Graphics.Vulkan.Command.RenderPass (VkRenderPassBeginInfo(..), withCmdRenderPass)
import Graphics.Vulkan.Device (VkDevice, vkCreateDevice, vkGetDeviceQueue, vkDeviceWaitIdle)
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
  ( VkDebugUtilsMessengerCreateInfoEXT(..)
  , VkDebugUtilsMessageSeverity(..)
  , VkDebugUtilsMessageType(..)
  , mkDebugUtilsMessenger
  )
import Graphics.Vulkan.Ext.Surface
  ( VkSurfaceFormatKHR(..)
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
import Graphics.Vulkan.Framebuffer (vkCreateFramebuffer)
import Graphics.Vulkan.FramebufferCreateInfo (VkFramebufferCreateInfo(..))
import Graphics.Vulkan.GraphicsPipelineCreateInfo (VkGraphicsPipelineCreateInfo(..))
import Graphics.Vulkan.GraphicsPipeline (vkCreateGraphicsPipelines)
import Graphics.Vulkan.ImageCreateInfo (VkSharingMode(..), VkImageUsageFlag(..))
import Graphics.Vulkan.ImageViewCreateInfo
  ( VkImageViewCreateInfo(..), VkImageSubresourceRange(..), VkComponentMapping(..)
  , VkImageAspectFlag(..), VkComponentSwizzle(..), VkImageViewType(..)
  , vkCreateImageView
  )
import Graphics.Vulkan.Instance (mkInstance)
import Graphics.Vulkan.InstanceCreateInfo (VkInstanceCreateInfo(..))
import Graphics.Vulkan.Layer (VkLayer(..), vkLayer, unVkLayer)
import Graphics.Vulkan.Offset (VkOffset2D(..))
import Graphics.Vulkan.PhysicalDevice
  ( vkEnumeratePhysicalDevices
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
import Graphics.Vulkan.Pipeline.Layout (vkCreatePipelineLayout)
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
import Graphics.Vulkan.Pipeline.VertexInputStateCreateInfo (VkPipelineVertexInputStateCreateInfo(..))
import Graphics.Vulkan.Pipeline.ViewportStateCreateInfo (VkPipelineViewportStateCreateInfo(..))
import Graphics.Vulkan.Pipeline.InputAssemblyStateCreateInfo
  (VkPipelineInputAssemblyStateCreateInfo(..), VkPrimitiveTopology(..))
import Graphics.Vulkan.Queue (VkQueue, VkQueueFamilyProperties(..), VkSubmitInfo(..), vkQueueSubmit)
import Graphics.Vulkan.Rect (VkRect2D(..))
import Graphics.Vulkan.RenderPass (vkCreateRenderPass)
import Graphics.Vulkan.RenderPassCreateInfo
  ( VkRenderPassCreateInfo(..)
  , VkAttachmentDescription(..)
  , VkSubpassDescription(..)
  , VkSubpassDependency(..)
  , VkAttachmentReference(..)
  , VkAttachmentStoreOp(..)
  )
import Graphics.Vulkan.Result (vkResult)
import Graphics.Vulkan.SampleCount (VkSampleCount(..))
import Graphics.Vulkan.Semaphore (VkSemaphore, VkSemaphoreCreateInfo(..), vkCreateSemaphore)
import Graphics.Vulkan.ShaderModule (shaderModuleFromFile)
import Graphics.Vulkan.ShaderStage (VkShaderStageFlag(..))
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
import qualified Graphics.Vulkan.PipelineStage as PipelineStage (VkPipelineStageFlag(..))
import qualified Graphics.Vulkan.SubpassContents as Subpass (VkSubpassContents(..))
import qualified Graphics.Vulkan.Queue as QueueType (VkQueueType(..))

mainLoop ::
  MonadIO m =>
  GLFW.Window ->
  VkDevice ->
  VkSwapchainKHR ->
  VkQueue ->
  VkQueue ->
  [VkCommandBuffer] ->
  VkSemaphore ->
  VkSemaphore ->
  m ()
mainLoop window device swapchain graphicsQ presentQ commandBuffers imageAvailableSem renderFinishedSem = go
  where
    go = do
      close <- liftIO $ GLFW.windowShouldClose window
      if close
        then vkDeviceWaitIdle device
        else do
          liftIO GLFW.pollEvents
          ix <- vkAcquireNextImageKHR device swapchain maxBound (Just imageAvailableSem) Nothing
          let
            submitInfo =
              VkSubmitInfo
              { pWaitSemaphores = [imageAvailableSem]
              , pWaitDstStageMask = [[PipelineStage.ColorAttachmentOutput]]
              , pCommandBuffers = [commandBuffers !! fromIntegral ix]
              , pSignalSemaphores = [renderFinishedSem]
              }
          vkQueueSubmit graphicsQ [submitInfo] Nothing
          let
            presentInfo =
              VkPresentInfoKHR
              { pWaitSemaphores = [renderFinishedSem]
              , pSwapchains = [swapchain]
              , pImageIndices = [ix]
              }
          vkQueuePresentKHR presentQ presentInfo
          go

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

main :: IO ()
main =
  vulkanGLFW . runManaged $ do
    window <- mkWindow hints 1280 960 "vulkan" Nothing Nothing
    requiredExts <- glfwGetRequiredInstanceExtensions
    inst <- mkInstance (icInfo requiredExts) Foreign.nullPtr
    messenger <- mkDebugUtilsMessenger @() inst messengerCreateInfo Foreign.nullPtr
    surface <- glfwCreateWindowSurface inst window Foreign.nullPtr
    physicalDevice <-
      (\case; [] -> error "vulkan no devices"; d:_ -> d) <$>
      vkEnumeratePhysicalDevices inst
    dFeatures <- vkGetPhysicalDeviceFeatures physicalDevice
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
             surface)
        qfProps
    device <-
      vkCreateDevice
        physicalDevice
        (dcInfo graphicsQfIx presentQfIx requiredExts dFeatures)
        Foreign.nullPtr

    graphicsQ <- vkGetDeviceQueue device graphicsQfIx 0
    presentQ <- vkGetDeviceQueue device presentQfIx 0

    availableFormats <- vkGetPhysicalDeviceSurfaceFormatsKHR physicalDevice surface
    surfaceFormat <-
      case availableFormats of
        [] -> error "vulkan no surface formats"
        h:_ ->
          if
            SurfaceFormat.format h == UNDEFINED ||
            preferredFormat `elem` availableFormats
          then pure preferredFormat
          else error "vulkan couldn't find preferred format"

    availablePresentModes <- vkGetPhysicalDeviceSurfacePresentModesKHR physicalDevice surface
    surfaceCapabilities <- vkGetPhysicalDeviceSurfaceCapabilitiesKHR physicalDevice surface

    let
      presentMode = mkPresentMode availablePresentModes
      swapExtent = mkSwapExtent surfaceCapabilities
      imageCount = mkImageCount surfaceCapabilities

      swapchainCreateInfo =
        VkSwapchainCreateInfoKHR
        { flags = []
        , surface = surface
        , minImageCount = imageCount
        , imageFormat = SurfaceFormat.format surfaceFormat
        , imageColorSpace = colorSpace surfaceFormat
        , imageExtent = swapExtent
        , imageArrayLayers = 1
        , imageUsage = [ColorAttachment]
        , imageSharingMode =
            if graphicsQfIx /= presentQfIx then Concurrent else Exclusive
        , pQueueFamilyIndices =
            if graphicsQfIx /= presentQfIx then [graphicsQfIx, presentQfIx] else []
        , preTransform = currentTransform surfaceCapabilities
        , compositeAlpha = Opaque
        , presentMode = presentMode
        , clipped = True
        , oldSwapchain = Nothing
        }

    swapchain <- vkCreateSwapchainKHR device swapchainCreateInfo Foreign.nullPtr
    images <- vkGetSwapchainImagesKHR device swapchain

    let
      imageViewCreateInfo img =
        VkImageViewCreateInfo
        { flags = []
        , image = img
        , viewType = TwoD
        , format = SurfaceFormat.format surfaceFormat
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

    imageViews <-
      traverse
        (\i -> vkCreateImageView device (imageViewCreateInfo i) Foreign.nullPtr)
        images

    vert <- shaderModuleFromFile "app/shaders/vert.spv" [] device Foreign.nullPtr
    frag <- shaderModuleFromFile "app/shaders/frag.spv" [] device Foreign.nullPtr

    let
      layoutInfo =
        VkPipelineLayoutCreateInfo
        { flags = []
        , pSetLayouts = []
        , pPushConstantRanges = []
        }

    pipelineLayout <- vkCreatePipelineLayout device layoutInfo Foreign.nullPtr

    let
      renderPassInfo =
        VkRenderPassCreateInfo
        { flags = []
        , pAttachments =
          [ VkAttachmentDescription
            { flags = []
            , format = SurfaceFormat.format surfaceFormat
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

    renderPass <- vkCreateRenderPass device renderPassInfo Foreign.nullPtr

    let
      vertShaderStageInfo :: VkPipelineShaderStageCreateInfo '[]
      vertShaderStageInfo =
        VkPipelineShaderStageCreateInfo
        { flags = []
        , stage = Vertex
        , module_ = vert
        , pName = "main"
        , pSpecializationInfo = Nothing
        }

      fragShaderStageInfo :: VkPipelineShaderStageCreateInfo '[]
      fragShaderStageInfo =
        VkPipelineShaderStageCreateInfo
        { flags = []
        , stage = Fragment
        , module_ = frag
        , pName = "main"
        , pSpecializationInfo = Nothing
        }

      vertexInputInfo =
        VkPipelineVertexInputStateCreateInfo
        { flags = []
        , pVertexBindingDescriptions = []
        , pVertexAttributeDescriptions = []
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
            , width = fromIntegral $ Extent2D.width swapExtent
            , height = fromIntegral $ Extent2D.height swapExtent
            , minDepth = 0
            , maxDepth = 1
            }
          ]
        , pScissors =
          [ VkRect2D
            { offset = VkOffset2D 0 0
            , extent = swapExtent
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
        , minSampleShading = 1
        , pSampleMask = []
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

    pipeline <-
      fmap (\case; [] -> error "vulkan no graphics pipeline"; p:_ -> p) $
      vkCreateGraphicsPipelines device Nothing [Some pipelineInfo] Foreign.nullPtr

    framebuffers <- for imageViews $ \imageView -> do
      let
        framebufferInfo =
          VkFramebufferCreateInfo
          { flags = []
          , renderPass = renderPass
          , pAttachments = [imageView]
          , width = Extent2D.width swapExtent
          , height = Extent2D.height swapExtent
          , layers = 1
          }
      vkCreateFramebuffer device framebufferInfo Foreign.nullPtr

    let
      commandPoolInfo =
        VkCommandPoolCreateInfo
        { flags = []
        , queueFamilyIndex = graphicsQfIx
        }

    commandPool <- vkCreateCommandPool device commandPoolInfo Foreign.nullPtr

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
          , renderArea = VkRect2D (VkOffset2D 0 0) swapExtent
          , pClearValues = [ClearValue.Color (ClearValue.Float32 1 1 1 1)]
          }

      withCommandBuffer cmdBuf beginInfo $
        withCmdRenderPass cmdBuf renderPassBeginInfo Subpass.Inline $ do
          vkCmdBindPipeline cmdBuf BindPoint.Graphics pipeline
          vkCmdDraw cmdBuf 3 1 0 0

    imageAvailableSem <- vkCreateSemaphore device (VkSemaphoreCreateInfo []) Foreign.nullPtr
    renderFinishedSem <- vkCreateSemaphore device (VkSemaphoreCreateInfo []) Foreign.nullPtr

    mainLoop window device swapchain graphicsQ presentQ commandBuffers imageAvailableSem renderFinishedSem
  where
    hints =
      [ WindowHint'ClientAPI ClientAPI'NoAPI
      , WindowHint'Resizable False
      ]

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
      VkExtent2D
      { width =
          max
            (Extent2D.width $ minImageExtent scs)
            (min
               (Extent2D.width $ maxImageExtent scs)
               (Extent2D.width $ currentExtent scs))
      , height =
          max
            (Extent2D.height $ minImageExtent scs)
            (min
               (Extent2D.height $ maxImageExtent scs)
               (Extent2D.height $ currentExtent scs))
      }

    icInfo required =
      VkInstanceCreateInfo
      { pApplicationInfo =
        VkApplicationInfo
        { pApplicationName = "Demo"
        , applicationVersion = _VK_MAKE_VERSION 1 0 0
        , pEngineName = "No Engine"
        , engineVersion = _VK_MAKE_VERSION 1 0 0
        , apiVersion = _VK_MAKE_VERSION 1 0 82
        }
      , ppEnabledLayerNames = [LunargStandardValidation]
      , ppEnabledExtensionNames = DebugUtils : required
      }

    messengerCreateInfo =
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

    dcInfo gqix pqix required fts =
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
          (Set.fromList [gqix, pqix])
      , ppEnabledLayerNames = [LunargStandardValidation]
      , ppEnabledExtensionNames = [Swapchain]
      , pEnabledFeatures = fts
      }
