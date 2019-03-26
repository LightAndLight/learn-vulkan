{-# language MagicHash #-}
{-# language PatternSynonyms, ViewPatterns #-}
module Graphics.Vulkan.Layer.VK_LAYER_GOOGLE_threading where

import Foreign.C.String (CString)
import GHC.Ptr (Ptr(..))
import Graphics.Vulkan.Marshal (cmpCStrings)

{-# INLINE _VK_LAYER_GOOGLE_threading #-}
_VK_LAYER_GOOGLE_threading :: CString
_VK_LAYER_GOOGLE_threading = Ptr "VK_LAYER_GOOGLE_threading\NUL"#

{-# INLINE is_VK_LAYER_GOOGLE_threading #-}
is_VK_LAYER_GOOGLE_threading :: CString -> Bool
is_VK_LAYER_GOOGLE_threading
  = (EQ ==) . cmpCStrings _VK_LAYER_GOOGLE_threading

pattern VK_LAYER_GOOGLE_threading :: CString
pattern VK_LAYER_GOOGLE_threading <- (is_VK_LAYER_GOOGLE_threading -> True)
  where VK_LAYER_GOOGLE_threading = _VK_LAYER_GOOGLE_threading
