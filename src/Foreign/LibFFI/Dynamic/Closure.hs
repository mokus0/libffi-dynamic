{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Foreign.LibFFI.Dynamic.Closure where

import Foreign.C.Types
import Foreign.LibFFI.Dynamic.Base
import Foreign.LibFFI.Dynamic.CIF
import Foreign.Ptr
import Foreign.Storable

newtype Closure = Closure (Ptr Closure)
newtype Entry = Entry (FunPtr Entry) deriving (Eq, Ord, Show, Storable)

foreign import ccall "hs_ffi_closure_alloc"
    ffi_closure_alloc :: CSize -> Ptr Entry -> IO Closure
foreign import ccall "hs_ffi_closure_free"
    ffi_closure_free :: Closure -> IO ()

type FFI_Impl t a = CIF t -> Ptr (SigReturn t) -> Ptr (Ptr ()) -> Ptr a -> IO ()

foreign import ccall "wrapper" wrap_FFI_Impl :: FFI_Impl t a -> IO (FunPtr (FFI_Impl t a))

foreign import ccall ffi_prep_closure_loc :: Closure -> CIF t -> FunPtr (FFI_Impl t a) -> Ptr a -> Entry -> IO FFI_Status
