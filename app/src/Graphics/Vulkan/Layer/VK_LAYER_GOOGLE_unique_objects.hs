{-# language MagicHash #-}
{-# language PatternSynonyms, ViewPatterns #-}
module Graphics.Vulkan.Layer.VK_LAYER_GOOGLE_unique_objects where

import Foreign.C.String (CString)
import GHC.Ptr (Ptr(..))
import Graphics.Vulkan.Marshal (cmpCStrings)

{-# INLINE _VK_LAYER_GOOGLE_unique_objects #-}
_VK_LAYER_GOOGLE_unique_objects :: CString
_VK_LAYER_GOOGLE_unique_objects = Ptr "VK_LAYER_GOOGLE_unique_objects\NUL"#

{-# INLINE is_VK_LAYER_GOOGLE_unique_objects #-}
is_VK_LAYER_GOOGLE_unique_objects :: CString -> Bool
is_VK_LAYER_GOOGLE_unique_objects
  = (EQ ==) . cmpCStrings _VK_LAYER_GOOGLE_unique_objects

pattern VK_LAYER_GOOGLE_unique_objects :: CString
pattern VK_LAYER_GOOGLE_unique_objects <- (is_VK_LAYER_GOOGLE_unique_objects -> True)
  where VK_LAYER_GOOGLE_unique_objects = _VK_LAYER_GOOGLE_unique_objects
