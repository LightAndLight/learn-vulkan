{-# language DataKinds #-}
{-# language MagicHash #-}
{-# language PatternSynonyms, ViewPatterns #-}
{-# language TypeApplications #-}
module Main where

import Control.Exception (Exception(..), bracket, throwIO)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Managed.Safe (MonadManaged, using, managed, runManaged)
import Data.Foldable (fold, traverse_)
import Data.Int (Int32, Int64)
import Data.Maybe (fromMaybe)
import Data.Traversable (for)
import Data.Void (Void)
import Data.Word (Word32, Word64)
import Graphics.UI.GLFW (ClientAPI(..), WindowHint(..))
import Graphics.Vulkan (_VK_MAKE_VERSION)
import Graphics.Vulkan.Core_1_0
  ( VkInstance, VkAllocationCallbacks
  , VkInstanceCreateInfo
  , VkBool32
  )

import qualified Foreign.C.String as Foreign
import qualified Foreign.Marshal.Alloc as Foreign
import qualified Foreign.Marshal.Array as Foreign
import qualified Foreign.Ptr as Foreign
import qualified Foreign.Storable as Foreign
import qualified Graphics.Vulkan.Constants as Vk
import qualified Graphics.Vulkan.Core_1_0 as Vk
import qualified Graphics.Vulkan.Ext.VK_EXT_debug_utils as Vk
import qualified Graphics.Vulkan.Marshal as Vk
import qualified Graphics.UI.GLFW as GLFW

import Graphics.Vulkan.ApplicationInfo (VkApplicationInfo(..))
import Graphics.Vulkan.Ext
  (VkExtension(..), vkExtension, unVkExtension, getRequiredInstanceExtensions)
import Graphics.Vulkan.Ext.DebugUtils
  ( VkDebugUtilsMessengerCreateInfoEXT(..)
  , VkDebugUtilsMessageSeverity(..)
  , VkDebugUtilsMessageType(..)
  , mkDebugUtilsMessenger
  )
import Graphics.Vulkan.Instance (mkInstance)
import Graphics.Vulkan.InstanceCreateInfo (VkInstanceCreateInfo(..))
import Graphics.Vulkan.Layer (VkLayer(..), vkLayer, unVkLayer)
import Graphics.Vulkan.Result (vkResult)

mainLoop :: MonadIO m => GLFW.Window -> m ()
mainLoop window = go
  where
    go = do
      close <- liftIO $ GLFW.windowShouldClose window
      unless close $ liftIO GLFW.pollEvents *> go

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

main :: IO ()
main =
  vulkanGLFW . runManaged $ do
    window <- mkWindow hints 1280 960 "vulkan" Nothing Nothing
    extsNames <- getRequiredInstanceExtensions
    inst <- mkInstance (icInfo extsNames) Foreign.nullPtr
    messenger <- mkDebugUtilsMessenger @() inst messengerCreateInfo Foreign.nullPtr
    mainLoop window
  where
    hints =
      [ WindowHint'ClientAPI ClientAPI'NoAPI
      , WindowHint'Resizable False
      ]

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
      { messageSeverity = [Verbose, Info, Warning, Error]
      , messageType = [General, Validation, Performance]
      , pfnUserCallback = \sev types cbData userData -> do
          putStrLn "debug callback:"
          print sev
          print types
          print cbData
          print userData
          pure False
      , pUserData = ()
      }
