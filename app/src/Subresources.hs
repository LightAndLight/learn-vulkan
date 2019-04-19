{-# language DataKinds, GADTs, ScopedTypeVariables, TypeOperators #-}
{-# language DuplicateRecordFields #-}
{-# language RankNTypes #-}
module Subresources where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Managed.Safe (MonadManaged)
import Control.Monad.State (evalStateT, modify, get)
import Data.Bits ((.&.), shiftL)
import Data.Foldable (for_)
import Data.Word (Word32)
import Data.Void (Void)
import Foreign (Ptr, Storable, sizeOf, pokeArray, nullPtr, plusPtr, pokeByteOff, castPtr)

import Data.Some (Some(..))
import Graphics.Vulkan.Buffer
  ( VkBuffer, VkBufferCreateInfo(..)
  , VkBufferUsageFlag(..)
  , VkBufferCreateFlag
  , vkCreateBuffer
  , vkGetBufferMemoryRequirements
  , vkBindBufferMemory
  )
import Graphics.Vulkan.Device
  ( VkDevice
  , VkMemoryAllocateInfo(..), vkAllocateMemory
  , vkMapMemory
  )
import Graphics.Vulkan.PhysicalDevice
  ( VkDeviceSize, VkPhysicalDevice
  , VkPhysicalDeviceProperties(..), VkPhysicalDeviceLimits(..)
  , VkMemoryPropertyFlag(..)
  , VkPhysicalDeviceMemoryProperties(..)
  , VkMemoryType(..)
  , vkGetPhysicalDeviceProperties
  , vkGetPhysicalDeviceMemoryProperties
  )
import Graphics.Vulkan.SharingMode (VkSharingMode(..))

import qualified Graphics.Vulkan.MemoryRequirements as MemoryRequirements (VkMemoryRequirements(..))

data Subresources f t where
  SubNil :: Subresources f '[]
  SubCons :: Storable t => f t -> Subresources f ts -> Subresources f (t ': ts)

traverseSubresources ::
  Applicative m =>
  (forall x. Storable x => f x -> m (g x)) ->
  Subresources f t ->
  m (Subresources g t)
traverseSubresources _ SubNil = pure SubNil
traverseSubresources f (SubCons a b) = SubCons <$> f a <*> traverseSubresources f b

forSubresources ::
  Applicative m =>
  Subresources f t ->
  (forall x. Storable x => f x -> m (g x)) ->
  m (Subresources g t)
forSubresources a f = traverseSubresources f a

data InitSubresource a
  = InitEmpty VkDeviceSize
  | InitFull [a]
  deriving (Eq, Ord, Show)

initSubresourceSize :: InitSubresource a -> VkDeviceSize
initSubresourceSize (InitEmpty a) = a
initSubresourceSize (InitFull a) = fromIntegral $ length a

subresourcesSize :: Subresources InitSubresource ts -> VkDeviceSize
subresourcesSize SubNil = 0
subresourcesSize (SubCons (ts :: InitSubresource t) rest) =
  fromIntegral (sizeOf (undefined :: t)) * initSubresourceSize ts +
  subresourcesSize rest

data SubresourceLoc a
  = SubresourceLoc
  { location :: Ptr a
  , offset :: VkDeviceSize
  , size :: VkDeviceSize
  } deriving (Eq, Ord, Show)

data AllocateSubresourcesInfo ts
  = AllocateSubresourcesInfo
  { flags :: [VkBufferCreateFlag]
  , usage :: [VkBufferUsageFlag]
  , memoryProperties :: [VkMemoryPropertyFlag]
  , subresources :: Subresources InitSubresource ts
  , sharingMode :: VkSharingMode
  , pQueueFamilyIndices :: [Word32]
  }

allocateSubresources ::
  forall m ts.
  MonadManaged m =>
  VkPhysicalDevice ->
  VkDevice ->
  AllocateSubresourcesInfo ts ->
  m (VkBuffer, Subresources SubresourceLoc ts)
allocateSubresources physDev dev info = do
  let
    bufferInfo =
      VkBufferCreateInfo
      { flags = flags (info :: AllocateSubresourcesInfo ts)
      , size = subresourcesSize $ subresources info
      , usage = usage (info :: AllocateSubresourcesInfo ts)
      , sharingMode = sharingMode (info :: AllocateSubresourcesInfo ts)
      , pQueueFamilyIndices = pQueueFamilyIndices (info :: AllocateSubresourcesInfo ts)
      }
  buffer <- vkCreateBuffer dev bufferInfo nullPtr
  memRequirements <- vkGetBufferMemoryRequirements dev buffer
  let
    reqTypeBits = MemoryRequirements.memoryTypeBits memRequirements
    reqPropFlags = memoryProperties info

  memProperties <- vkGetPhysicalDeviceMemoryProperties physDev

  let
    m_ix =
      foldr
        (\(ix, prop) rest ->
           let mask = (1 `shiftL` ix) in
           if
             reqTypeBits .&. mask == mask &&
             all (`elem` propertyFlags prop) reqPropFlags
           then Just ix
           else rest)
        Nothing
        (zip [0..] $ memoryTypes memProperties)

  case m_ix of
    Nothing -> error "No suitable memory type"
    Just ix -> do
      let
        allocInfo =
          VkMemoryAllocateInfo
          { allocationSize = MemoryRequirements.size memRequirements
          , memoryTypeIndex = fromIntegral ix
          }
      mem <- vkAllocateMemory dev allocInfo nullPtr
      vkBindBufferMemory dev buffer mem 0
      ptr :: Ptr Void <- vkMapMemory dev mem 0 Nothing []

      srs <-
        flip evalStateT 0 $
        forSubresources (subresources info) $ \(sr :: InitSubresource t) -> do
          off <- get
          let
            ptr' = ptr `plusPtr` fromIntegral off
            sz =
              case sr of
                InitEmpty sz -> sz
                InitFull stuff -> fromIntegral $ length stuff * sizeOf (undefined :: t)
          case sr of
            InitEmpty{} -> pure ()
            InitFull stuff -> liftIO $ pokeArray ptr' stuff

          SubresourceLoc ptr' off sz <$ modify (+sz)
      pure (buffer, srs)

perImageUniformBuffer ::
  forall m a.
  (MonadManaged m, Storable a) =>
  VkPhysicalDevice ->
  VkDevice ->
  Int ->
  a ->
  m (VkBuffer, SubresourceLoc a)
perImageUniformBuffer physDev dev count val = do
  props <- vkGetPhysicalDeviceProperties physDev
  let
    align = fromIntegral . minUniformBufferOffsetAlignment $ limits props
    sz = sizeOf (undefined :: a) - 1
    (q, r) = sz `quotRem` align
    elemSize = sz + align - r
    bufferSize = elemSize * count
    bufferInfo =
      VkBufferCreateInfo
      { flags = []
      , size = fromIntegral bufferSize
      , usage = [UniformBuffer]
      , sharingMode = Exclusive
      , pQueueFamilyIndices = []
      }
  buffer <- vkCreateBuffer dev bufferInfo nullPtr

  memRequirements <- vkGetBufferMemoryRequirements dev buffer
  let
    reqTypeBits = MemoryRequirements.memoryTypeBits memRequirements
    reqPropFlags = [HostVisible, HostCoherent]

  memProperties <- vkGetPhysicalDeviceMemoryProperties physDev

  let
    m_ix =
      foldr
        (\(ix, prop) rest ->
           let mask = (1 `shiftL` ix) in
           if
             reqTypeBits .&. mask == mask &&
             all (`elem` propertyFlags prop) reqPropFlags
           then Just ix
           else rest)
        Nothing
        (zip [0..] $ memoryTypes memProperties)

  case m_ix of
    Nothing -> error "No suitable memory type"
    Just ix -> do
      let
        allocInfo =
          VkMemoryAllocateInfo
          { allocationSize = MemoryRequirements.size memRequirements
          , memoryTypeIndex = fromIntegral ix
          }
      mem <- vkAllocateMemory dev allocInfo nullPtr
      vkBindBufferMemory dev buffer mem 0
      ptr :: Ptr Void <- vkMapMemory dev mem 0 Nothing []

      liftIO . for_ [0..count-1] $ \n ->
        pokeByteOff ptr (n * elemSize) val

      pure (buffer, SubresourceLoc (castPtr ptr) 0 $ fromIntegral bufferSize)
