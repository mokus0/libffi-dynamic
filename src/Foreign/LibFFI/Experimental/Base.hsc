module Foreign.LibFFI.Experimental.Base where

import Foreign.C.Types

#include <ffi.h>

sizeOfClosure = (#size ffi_closure) :: CSize

newtype FFI_Status = FFI_Status CInt

