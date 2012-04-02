{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Foreign.LibFFI.Experimental.Closure where

import Foreign.C.Types
import Foreign.LibFFI.Experimental.Base
import Foreign.LibFFI.Experimental.CIF
import Foreign.Ptr
import Foreign.Storable

newtype Closure = Closure (Ptr Closure)
newtype Entry = Entry (FunPtr Entry) deriving (Eq, Ord, Show, Storable)

foreign import ccall ffi_closure_alloc :: CSize -> Ptr Entry -> IO Closure
foreign import ccall ffi_closure_free  :: Closure -> IO ()

type FFI_Impl t a = CIF t -> Ptr (SigReturn t) -> Ptr (Ptr ()) -> Ptr a -> IO ()

foreign import ccall "wrapper" wrap_FFI_Impl :: FFI_Impl t a -> IO (FunPtr (FFI_Impl t a))

foreign import ccall ffi_prep_closure_loc :: Closure -> CIF t -> FunPtr (FFI_Impl t a) -> Ptr a -> Entry -> IO FFI_Status
