module Graphics.Vulkan.SubpassContents where

import qualified Graphics.Vulkan.Core_1_0 as Vk

data VkSubpassContents
  = Inline
  | SecondaryCommandBuffers
  deriving (Eq, Ord, Show)

unVkSubpassContents :: VkSubpassContents -> Vk.VkSubpassContents
unVkSubpassContents a =
  case a of
    Inline -> Vk.VK_SUBPASS_CONTENTS_INLINE
    SecondaryCommandBuffers -> Vk.VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS
