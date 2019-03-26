{-# language MagicHash #-}
{-# language PatternSynonyms, ViewPatterns #-}
module Graphics.Vulkan.Layer.VK_LAYER_LUNARG_standard_validation where

import Foreign.C.String (CString)
import GHC.Ptr (Ptr(..))
import Graphics.Vulkan.Marshal (cmpCStrings)

{-# INLINE _VK_LAYER_LUNARG_standard_validation #-}
_VK_LAYER_LUNARG_standard_validation :: CString
_VK_LAYER_LUNARG_standard_validation = Ptr "VK_LAYER_LUNARG_standard_validation\NUL"#

{-# INLINE is_VK_LAYER_LUNARG_standard_validation #-}
is_VK_LAYER_LUNARG_standard_validation :: CString -> Bool
is_VK_LAYER_LUNARG_standard_validation
  = (EQ ==) . cmpCStrings _VK_LAYER_LUNARG_standard_validation

pattern VK_LAYER_LUNARG_standard_validation :: CString
pattern VK_LAYER_LUNARG_standard_validation <- (is_VK_LAYER_LUNARG_standard_validation -> True)
  where VK_LAYER_LUNARG_standard_validation = _VK_LAYER_LUNARG_standard_validation
