{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
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

class FFIType a where
    ffiType :: Type a
    
    -- some FFI types (small ints) are returned in larger
    -- buffers than they need.  On little-endian systems this
    -- would be OK, but for the sake of big-endian ones we 
    -- account for this.
    type Returned a
    type Returned a = a
    
    toReturned :: a -> Returned a
    default toReturned :: a ~ Returned a => a -> a
    toReturned = id
    
    fromReturned :: Returned a -> a
    default fromReturned :: a ~ Returned a => a -> a
    fromReturned = id

ffiTypeOf :: FFIType a => p a -> Type a
ffiTypeOf = const ffiType

ffiTypeOf_ :: FFIType a => p a -> SomeType 
ffiTypeOf_ = toSomeType . ffiTypeOf

newtype Arg a b = Arg
    { withArg :: forall t. a -> (Ptr b -> IO t) -> IO t }

class FFIType (ForeignArg a) => ArgType a where
    type ForeignArg a
    type ForeignArg a = a

    argMarshaller :: Arg a (ForeignArg a)
    default argMarshaller :: (Storable a, a ~ ForeignArg a) => Arg a a
    argMarshaller = Arg with

newtype Ret a b = Ret
    { withRet :: forall t. (Ptr b -> IO t) -> IO (a, t) }

newtype Ret_ a b = Ret_
    { withRet_ :: forall t. (Ptr b -> IO t) -> IO a }

class FFIType (ForeignRet a) => RetType a where
    type ForeignRet a
    type ForeignRet a = a
    
    retMarshaller_  :: Ret_ a (ForeignRet a)
    default retMarshaller_ :: (Storable (Returned a), a ~ ForeignRet a) => Ret_ a a
    retMarshaller_ = Ret_ $ \action ->
        alloca $ \p -> do
            action (castPtr p)
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

