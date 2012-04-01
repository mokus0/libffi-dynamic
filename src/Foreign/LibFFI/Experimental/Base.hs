{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Foreign.LibFFI.Experimental.Base where

import Control.Applicative
import Data.Functor.Contravariant
import Foreign.Marshal
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

newtype Type t = Type (Ptr (Type t)) deriving (Eq, Show, Storable)
castType :: Type a -> Type b
castType (Type t) = Type (castPtr t)

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

newtype InArg a b = InArg { withInArg :: forall t. Ptr a -> (b -> IO t) -> IO t }
instance Functor (InArg a) where
    fmap f (InArg g) = InArg (\p k -> g p (k . f))

newtype OutArg a b = OutArg { withOutArg :: forall t. b -> (Ptr a -> IO t) -> IO t }
instance Contravariant (OutArg a) where
    contramap f arg = OutArg (withOutArg arg . f)

composeInArgs :: InArg a (Ptr b) -> InArg b c -> InArg a c
composeInArgs f g = InArg $ \p -> withInArg f p . flip (withInArg g)

composeOutArgs :: OutArg b c -> OutArg a (Ptr b) -> OutArg a c
composeOutArgs f g = OutArg $ \x -> withOutArg f x . flip (withOutArg g)

class FFIType a => ArgType a where
    inArg :: InArg a a
    default inArg :: Storable a => InArg a a
    inArg = InArg $ \p k -> peek p >>= k
    
    outArg :: OutArg a a
    default outArg :: Storable a => OutArg a a
    outArg = OutArg with

newtype InRet a b = InRet
    { withInRet :: forall t. (Ptr a -> IO t) -> IO b }

instance Functor (InRet a) where
    fmap f ret = InRet (fmap f . withInRet ret)

newtype OutRet a b = OutRet { withOutRet :: IO b -> Ptr (Returned a) -> IO () }
instance Contravariant (OutRet a) where
    contramap f arg = OutRet (withOutRet arg . fmap f)

class FFIType a => RetType a where
    inRet  :: InRet a a
    default inRet :: Storable (Returned a) => InRet a a
    inRet = InRet $ \action ->
        alloca $ \p -> do
            action (castPtr p)
            fromReturned <$> peek p
    
    outRet :: OutRet a a
    default outRet :: Storable (Returned a) => OutRet a a
    outRet = OutRet $ \x p -> poke p . toReturned =<< x

newtype ABI = ABI CInt
foreign import ccall defaultABI :: ABI

newtype CIF = CIF (Ptr CIF)
foreign import ccall sizeOfCIF :: CSize

newtype FFI_Status = FFI_Status CInt

foreign import ccall ffi_prep_cif :: CIF -> ABI -> CInt -> SomeType -> Ptr SomeType -> IO FFI_Status

foreign import ccall ffi_call :: CIF -> FunPtr a -> Ptr r -> Ptr (Ptr ()) -> IO ()

newtype Closure = Closure (Ptr Closure)
newtype Entry = Entry (FunPtr Entry) deriving (Eq, Ord, Show, Storable)

foreign import ccall sizeOfClosure :: CSize

foreign import ccall ffi_closure_alloc :: CSize -> Ptr Entry -> IO Closure
foreign import ccall ffi_closure_free  :: Closure -> IO ()

type FFI_Impl a = CIF -> Ptr () -> Ptr (Ptr ()) -> Ptr a -> IO ()

foreign import ccall "wrapper" wrap_FFI_Impl :: FFI_Impl t -> IO (FunPtr (FFI_Impl t))

foreign import ccall ffi_prep_closure_loc :: Closure -> CIF -> FunPtr (FFI_Impl a) -> Ptr a -> Entry -> IO FFI_Status
