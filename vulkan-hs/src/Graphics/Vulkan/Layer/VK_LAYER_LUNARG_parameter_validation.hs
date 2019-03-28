{-# language MagicHash #-}
{-# language PatternSynonyms, ViewPatterns #-}
module Graphics.Vulkan.Layer.VK_LAYER_LUNARG_parameter_validation where

import Foreign.C.String (CString)
import GHC.Ptr (Ptr(..))
import Graphics.Vulkan.Marshal (cmpCStrings)

{-# INLINE _VK_LAYER_LUNARG_parameter_validation #-}
_VK_LAYER_LUNARG_parameter_validation :: CString
_VK_LAYER_LUNARG_parameter_validation = Ptr "VK_LAYER_LUNARG_parameter_validation\NUL"#

{-# INLINE is_VK_LAYER_LUNARG_parameter_validation #-}
is_VK_LAYER_LUNARG_parameter_validation :: CString -> Bool
is_VK_LAYER_LUNARG_parameter_validation
  = (EQ ==) . cmpCStrings _VK_LAYER_LUNARG_parameter_validation

pattern VK_LAYER_LUNARG_parameter_validation :: CString
pattern VK_LAYER_LUNARG_parameter_validation <- (is_VK_LAYER_LUNARG_parameter_validation -> True)
  where VK_LAYER_LUNARG_parameter_validation = _VK_LAYER_LUNARG_parameter_validation
