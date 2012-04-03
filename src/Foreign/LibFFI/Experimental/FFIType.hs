{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RankNTypes #-}
module Foreign.LibFFI.Experimental.FFIType where

import Data.Functor.Contravariant
import Data.Int
import Data.Word
import Foreign.C
import Foreign.LibFFI.Experimental.Type
import Foreign.Marshal hiding (void)
import Foreign.Ptr
import Foreign.Storable

class FFIType a where
    ffiType :: Type a

ffiTypeOf :: FFIType a => p a -> Type a
ffiTypeOf = const ffiType

ffiTypeOf_ :: FFIType a => p a -> SomeType 
ffiTypeOf_ = toSomeType . ffiTypeOf

newtype InArg a b = InArg { withInArg :: forall t. Ptr a -> (b -> IO t) -> IO t }
instance Functor (InArg a) where
    fmap f (InArg g) = InArg (\p k -> g p (k . f))

castInArg :: InArg a c -> InArg b c
castInArg (InArg f) = InArg (f . castPtr)

newtype OutArg a b = OutArg { withOutArg :: forall t. b -> (Ptr a -> IO t) -> IO t }
instance Contravariant (OutArg a) where
    contramap f arg = OutArg (withOutArg arg . f)

castOutArg :: OutArg a c -> OutArg b c
castOutArg (OutArg f) = OutArg (\x k -> f x (k . castPtr))

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

castInRet :: InRet a c -> InRet b c
castInRet (InRet f) = InRet (\k -> f (k . castPtr))

newtype OutRet a b = OutRet { withOutRet :: IO b -> Ptr a -> IO () }
instance Contravariant (OutRet a) where
    contramap f arg = OutRet (withOutRet arg . fmap f)

castOutRet :: OutRet a c -> OutRet b c
castOutRet (OutRet f) = OutRet (\x -> f x . castPtr)

class FFIType a => RetType a where
    inRet  :: InRet a a
    default inRet :: Storable a => InRet a a
    inRet = InRet $ \action ->
        alloca $ \p -> do
            action p
            peek p
    
    outRet :: OutRet a a
    default outRet :: Storable a => OutRet a a
    outRet = OutRet $ \x p -> poke p =<< x

instance FFIType () where ffiType = void
instance RetType () where
    inRet = InRet $ \action -> do
        action nullPtr
        return ()
    outRet = OutRet const

instance FFIType (Ptr a) where ffiType = pointer
instance ArgType (Ptr a)
instance RetType (Ptr a)

instance FFIType (FunPtr a) where ffiType = castType pointer
instance ArgType (FunPtr a)
instance RetType (FunPtr a)

instance FFIType Float where ffiType = floating
instance ArgType Float
instance RetType Float

instance FFIType Double where ffiType = floating
instance ArgType Double
instance RetType Double

instance FFIType Int where ffiType = sint
instance ArgType Int
instance RetType Int

inRetViaInt :: Integral a => InRet a a
inRetViaInt = castInRet (fmap (fromIntegral :: Integral a => Int -> a ) inRet)
inRetViaWord :: Integral a => InRet a a
inRetViaWord = castInRet (fmap (fromIntegral :: Integral a => Word -> a ) inRet)

outRetViaInt :: Integral a => OutRet a a
outRetViaInt = castOutRet (contramap (fromIntegral :: Integral a => a -> Int) outRet)
outRetViaWord :: Integral a => OutRet a a
outRetViaWord = castOutRet (contramap (fromIntegral :: Integral a => a -> Word) outRet)

instance FFIType Int8 where ffiType = sint8
instance ArgType Int8
instance RetType Int8 where
    inRet  = inRetViaInt
    outRet = outRetViaInt

instance FFIType Int16 where ffiType = sint16
instance ArgType Int16
instance RetType Int16 where
    inRet  = inRetViaInt
    outRet = outRetViaInt

instance FFIType Int32 where ffiType = sint32
instance ArgType Int32
instance RetType Int32 where
    inRet  = inRetViaInt
    outRet = outRetViaInt

instance FFIType Int64 where ffiType = sint64
instance ArgType Int64
instance RetType Int64

-- TODO: check target word size
instance FFIType Word where ffiType = uint
instance ArgType Word
instance RetType Word

instance FFIType Word8 where ffiType = uint8
instance ArgType Word8
instance RetType Word8 where
    inRet  = inRetViaWord
    outRet = outRetViaWord

instance FFIType Word16 where ffiType = uint16
instance ArgType Word16
instance RetType Word16 where
    inRet  = inRetViaWord
    outRet = outRetViaWord

instance FFIType Word32 where ffiType = uint32
instance ArgType Word32
instance RetType Word32 where
    inRet  = inRetViaWord
    outRet = outRetViaWord

instance FFIType Word64 where ffiType = uint64
instance ArgType Word64
instance RetType Word64

instance FFIType CChar where ffiType = sint
instance ArgType CChar
instance RetType CChar where
    inRet  = inRetViaInt
    outRet = outRetViaInt

instance FFIType CSChar where ffiType = sint
instance ArgType CSChar
instance RetType CSChar where
    inRet  = inRetViaInt
    outRet = outRetViaInt

instance FFIType CUChar where ffiType = uint
instance ArgType CUChar
instance RetType CUChar where
    inRet  = inRetViaWord
    outRet = outRetViaWord

instance FFIType CShort where ffiType = sint
instance ArgType CShort
instance RetType CShort where
    inRet  = inRetViaInt
    outRet = outRetViaInt

instance FFIType CUShort where ffiType = uint
instance ArgType CUShort
instance RetType CUShort where
    inRet  = inRetViaWord
    outRet = outRetViaWord

instance FFIType CInt where ffiType = sint
instance ArgType CInt
instance RetType CInt where
    inRet  = inRetViaInt
    outRet = outRetViaInt

instance FFIType CUInt where ffiType = uint
instance ArgType CUInt
instance RetType CUInt where
    inRet  = inRetViaWord
    outRet = outRetViaWord

instance FFIType CLong where ffiType = sint
instance ArgType CLong
instance RetType CLong

instance FFIType CULong where ffiType = uint
instance ArgType CULong
instance RetType CULong

instance FFIType CPtrdiff where ffiType = uint
instance ArgType CPtrdiff
instance RetType CPtrdiff

instance FFIType CSize where ffiType = uint
instance ArgType CSize
instance RetType CSize

instance FFIType CWchar where ffiType = sint
instance ArgType CWchar
instance RetType CWchar where
    inRet  = inRetViaInt
    outRet = outRetViaInt

instance FFIType CSigAtomic where ffiType = sint
instance ArgType CSigAtomic
instance RetType CSigAtomic

instance FFIType CLLong where ffiType = sint
instance ArgType CLLong
instance RetType CLLong

instance FFIType CULLong where ffiType = uint
instance ArgType CULLong
instance RetType CULLong

instance FFIType CIntPtr where ffiType = sint
instance ArgType CIntPtr
instance RetType CIntPtr

instance FFIType CUIntPtr where ffiType = uint
instance ArgType CUIntPtr
instance RetType CUIntPtr

instance FFIType CIntMax where ffiType = sint
instance ArgType CIntMax
instance RetType CIntMax

instance FFIType CUIntMax where ffiType = uint
instance ArgType CUIntMax
instance RetType CUIntMax

instance FFIType CClock where ffiType = sint
instance ArgType CClock
instance RetType CClock

instance FFIType CTime where ffiType = sint
instance ArgType CTime
instance RetType CTime

instance FFIType CUSeconds where ffiType = uint
instance ArgType CUSeconds
instance RetType CUSeconds

instance FFIType CSUSeconds where ffiType = sint
instance ArgType CSUSeconds
instance RetType CSUSeconds

instance FFIType CFloat where ffiType = floating
instance ArgType CFloat
instance RetType CFloat

instance FFIType CDouble where ffiType = floating
instance ArgType CDouble
instance RetType CDouble

outByRef :: OutArg a b -> OutArg (Ptr a) b
outByRef arg = composeOutArgs arg outArg

stringArg :: OutArg CString String
stringArg = outByRef (OutArg withCString)

instance (FFIType a, FFIType b)
        => FFIType (a, b) where
    ffiType = t
        where
            t = Type $ struct
                [ ffiTypeOf_ ((castType :: Type (a,b) -> Type a) t)
                , ffiTypeOf_ ((castType :: Type (a,b) -> Type b) t)
                ]

instance (FFIType a, FFIType b, FFIType c) 
        => FFIType (a, b, c) where
    ffiType = t
        where
            t = Type $ struct
                [ ffiTypeOf_ ((castType :: Type (a,b,c) -> Type a) t)
                , ffiTypeOf_ ((castType :: Type (a,b,c) -> Type b) t)
                , ffiTypeOf_ ((castType :: Type (a,b,c) -> Type c) t)
                ]

instance (FFIType a, FFIType b, FFIType c, FFIType d) 
        => FFIType (a, b, c, d) where
    ffiType = t
        where
            t = Type $ struct
                [ ffiTypeOf_ ((castType :: Type (a,b,c,d) -> Type a) t)
                , ffiTypeOf_ ((castType :: Type (a,b,c,d) -> Type b) t)
                , ffiTypeOf_ ((castType :: Type (a,b,c,d) -> Type c) t)
                , ffiTypeOf_ ((castType :: Type (a,b,c,d) -> Type d) t)
                ]

instance (FFIType a, FFIType b, FFIType c, FFIType d, FFIType e)
        => FFIType (a, b, c, d, e) where
    ffiType = t
        where
            t = Type $ struct
                [ ffiTypeOf_ ((castType :: Type (a,b,c,d,e) -> Type a) t)
                , ffiTypeOf_ ((castType :: Type (a,b,c,d,e) -> Type b) t)
                , ffiTypeOf_ ((castType :: Type (a,b,c,d,e) -> Type c) t)
                , ffiTypeOf_ ((castType :: Type (a,b,c,d,e) -> Type d) t)
                , ffiTypeOf_ ((castType :: Type (a,b,c,d,e) -> Type e) t)
                ]
