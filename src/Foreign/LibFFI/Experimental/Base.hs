{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Foreign.LibFFI.Experimental.Base where

import Control.Applicative
import Foreign.Marshal
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe

newtype Type t = Type (Ptr (Type t)) deriving (Eq, Show, Storable)

newtype SomeType = SomeType (Type SomeType) deriving (Eq, Show, Storable)
toSomeType :: Type a -> SomeType
toSomeType (Type p) = SomeType (Type (castPtr p))

class Storable a => FFIType a where
    ffiType :: Type a

ffiTypeOf :: FFIType a => p a -> Type a
ffiTypeOf = const ffiType

ffiTypeOf_ :: FFIType a => p a -> SomeType 
ffiTypeOf_ = toSomeType . ffiTypeOf

class FFIType (Marshalled a) => ArgType a where
    type Marshalled a
    type Marshalled a = a

    withArg :: a -> (Ptr (Marshalled a) -> IO b) -> IO b
    default withArg :: (Storable a, a ~ Marshalled a) => a -> (Ptr a -> IO b) -> IO b
    withArg = with
class FFIType (Returned a) => RetType a where
    type Returned a
    type Returned a = a
    
    toReturned :: a -> Returned a
    default toReturned :: a ~ Returned a => a -> a
    toReturned = id
    
    fromReturned :: Returned a -> a
    default fromReturned :: a ~ Returned a => a -> a
    fromReturned = id

withRet_ :: RetType t => (Ptr (Returned t) -> IO b) -> IO t
withRet_ action = alloca $ \p -> do
    action p
    fromReturned <$> peek p

newtype ABI = ABI CInt
foreign import ccall getDefaultABI :: IO ABI
defaultABI = unsafePerformIO getDefaultABI

newtype CIF = CIF (Ptr CIF)
foreign import ccall getSizeOfCIF :: IO CInt
sizeOfCIF :: Int
sizeOfCIF = fromIntegral (unsafePerformIO getSizeOfCIF)

newtype FFI_Status = FFI_Status CInt

foreign import ccall ffi_prep_cif :: CIF -> ABI -> CInt -> SomeType -> Ptr SomeType -> IO FFI_Status

foreign import ccall ffi_call :: CIF -> FunPtr a -> Ptr r -> Ptr (Ptr ()) -> IO ()

