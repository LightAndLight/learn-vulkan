{-# language DataKinds #-}
{-# language TypeApplications #-}
module Main where

import Control.Monad (unless)
import Data.Foldable (traverse_)
import Graphics.UI.GLFW (ClientAPI(..), WindowHint(..))
import Graphics.Vulkan (_VK_MAKE_VERSION)
import Graphics.Vulkan.Core_1_0 (VkApplicationInfo)

import qualified Foreign.C.String as C
import qualified Graphics.Vulkan.Core_1_0 as Vk
import qualified Graphics.Vulkan.Marshal as Vk
import qualified Graphics.UI.GLFW as GLFW

mainLoop :: GLFW.Window -> IO ()
mainLoop window = go
  where
    go = do
      close <- GLFW.windowShouldClose window
      unless close $ GLFW.pollEvents *> go

vulkanGLFW :: IO a -> IO a
vulkanGLFW m = do
  initSucceeded <- GLFW.init
  unless initSucceeded $ error "glfw init failed"
  vkSupported <- GLFW.vulkanSupported
  unless vkSupported $ error "glfw vulkan not supported"
  m <* GLFW.terminate

withWindow ::
  [GLFW.WindowHint] ->
  Int ->
  Int ->
  String ->
  Maybe GLFW.Monitor ->
  Maybe GLFW.Window ->
  (GLFW.Window -> IO a) ->
  IO a
withWindow hints w h name mMonitor mWindow f = do
  traverse_ GLFW.windowHint hints
  mWindow <- GLFW.createWindow w h name mMonitor mWindow
  case mWindow of
    Nothing -> error "glfw window create failed"
    Just window -> do
      res <- f window
      res <$ GLFW.destroyWindow window

type VkVersion = Vk.Word32

newAppInfo ::
  String -> -- ^ Application name
  String -> -- ^ Engine name
  VkVersion -> -- ^ Application version
  VkVersion -> -- ^ Engine version
  VkVersion -> -- ^ Api version
  IO VkApplicationInfo
newAppInfo appName engineName appVersion engineVersion apiVersion =
  C.withCString appName $ \appNamePtr ->
  C.withCString engineName $ \engineNamePtr ->
  Vk.newVkData @VkApplicationInfo $ \ptr -> do
    Vk.writeField @"sType" ptr Vk.VK_STRUCTURE_TYPE_APPLICATION_INFO
    Vk.writeField @"pApplicationName" ptr appNamePtr
    Vk.writeField @"applicationVersion" ptr appVersion
    Vk.writeField @"pEngineName" ptr engineNamePtr
    Vk.writeField @"engineVersion" ptr engineVersion
    Vk.writeField @"apiVersion" ptr apiVersion

main :: IO ()
main =
  vulkanGLFW $
  withWindow hints 1280 960 "vulkan" Nothing Nothing $ \window -> do
    appInfo <-
      newAppInfo
        "Demo"
        "No Engine"
        (_VK_MAKE_VERSION 1 0 0)
        (_VK_MAKE_VERSION 1 0 0)
        (_VK_MAKE_VERSION 1 1 82)
    mainLoop window
  where
    hints =
      [ WindowHint'ClientAPI ClientAPI'NoAPI
      , WindowHint'Resizable False
      ]
