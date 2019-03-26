{-# language MagicHash #-}
{-# language PatternSynonyms, ViewPatterns #-}
module Graphics.Vulkan.Layer.VK_LAYER_LUNARG_object_tracker where

import Foreign.C.String (CString)
import GHC.Ptr (Ptr(..))
import Graphics.Vulkan.Marshal (cmpCStrings)

{-# INLINE _VK_LAYER_LUNARG_object_tracker #-}
_VK_LAYER_LUNARG_object_tracker :: CString
_VK_LAYER_LUNARG_object_tracker = Ptr "VK_LAYER_LUNARG_object_tracker\NUL"#

{-# INLINE is_VK_LAYER_LUNARG_object_tracker #-}
is_VK_LAYER_LUNARG_object_tracker :: CString -> Bool
is_VK_LAYER_LUNARG_object_tracker
  = (EQ ==) . cmpCStrings _VK_LAYER_LUNARG_object_tracker

pattern VK_LAYER_LUNARG_object_tracker :: CString
pattern VK_LAYER_LUNARG_object_tracker <- (is_VK_LAYER_LUNARG_object_tracker -> True)
  where VK_LAYER_LUNARG_object_tracker = _VK_LAYER_LUNARG_object_tracker
