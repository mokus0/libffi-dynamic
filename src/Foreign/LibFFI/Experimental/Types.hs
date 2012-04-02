{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}
module Foreign.LibFFI.Experimental.Types
    ( module Foreign.LibFFI.Experimental.Struct
    , module Foreign.LibFFI.Experimental.Types
    ) where

import Data.Int
import Data.Proxy
import Data.Word
import Foreign.C
import Foreign.LibFFI.Experimental.Base
import Foreign.LibFFI.Experimental.Struct (struct)
import Foreign.Ptr

foreign import ccall "&ffi_type_void"    ffi_type_void    :: Type ()
foreign import ccall "&ffi_type_pointer" ffi_type_pointer :: Type (Ptr a)

foreign import ccall "&ffi_type_float"  ffi_type_float  :: Type Float
foreign import ccall "&ffi_type_double" ffi_type_double :: Type Double

foreign import ccall "&ffi_type_uint8"  ffi_type_uint8  :: Type Word8
foreign import ccall "&ffi_type_uint16" ffi_type_uint16 :: Type Word16
foreign import ccall "&ffi_type_uint32" ffi_type_uint32 :: Type Word32
foreign import ccall "&ffi_type_uint64" ffi_type_uint64 :: Type Word64

foreign import ccall "&ffi_type_sint8"  ffi_type_sint8  :: Type Int8
foreign import ccall "&ffi_type_sint16" ffi_type_sint16 :: Type Int16
foreign import ccall "&ffi_type_sint32" ffi_type_sint32 :: Type Int32
foreign import ccall "&ffi_type_sint64" ffi_type_sint64 :: Type Int64


instance FFIType () where
    ffiType = ffi_type_void
instance RetType () where
    inRet = InRet $ \action -> do
        action nullPtr
        return ()
    outRet = OutRet const

instance FFIType (Ptr a) where
    ffiType = ffi_type_pointer
instance ArgType (Ptr a)
instance RetType (Ptr a)

instance FFIType (FunPtr a) where
    ffiType = castType ffi_type_pointer
instance ArgType (FunPtr a)
instance RetType (FunPtr a)

instance FFIType Float where
    ffiType = ffi_type_float
instance ArgType Float
instance RetType Float

instance FFIType Double where
    ffiType = ffi_type_double
instance ArgType Double
instance RetType Double

-- TODO: detect int/word size
-- TODO: Foreign.C.Types
instance FFIType Int where
    ffiType = castType ffi_type_sint64
instance ArgType Int
instance RetType Int

instance FFIType Int8 where
    ffiType = ffi_type_sint8
    type Returned Int8 = Int
    toReturned   = fromIntegral
    fromReturned = fromIntegral
instance ArgType Int8
instance RetType Int8

instance FFIType Int16 where
    ffiType = ffi_type_sint16
    type Returned Int16 = Int
    toReturned   = fromIntegral
    fromReturned = fromIntegral
instance ArgType Int16
instance RetType Int16

instance FFIType Int32 where
    ffiType = ffi_type_sint32
    type Returned Int32 = Int
    toReturned   = fromIntegral
    fromReturned = fromIntegral
instance ArgType Int32
instance RetType Int32

instance FFIType Int64 where
    ffiType = ffi_type_sint64
instance ArgType Int64
instance RetType Int64

-- TODO: check target word size
instance FFIType Word where
    ffiType = castType ffi_type_uint64
instance ArgType Word
instance RetType Word

instance FFIType Word8 where
    ffiType = ffi_type_uint8
    type Returned Word8 = Word
    toReturned   = fromIntegral
    fromReturned = fromIntegral
instance ArgType Word8
instance RetType Word8

instance FFIType Word16 where
    ffiType = ffi_type_uint16
    type Returned Word16 = Word
    toReturned   = fromIntegral
    fromReturned = fromIntegral
instance ArgType Word16
instance RetType Word16

instance FFIType Word32 where
    ffiType = ffi_type_uint32
    type Returned Word32 = Word
    toReturned   = fromIntegral
    fromReturned = fromIntegral
instance ArgType Word32
instance RetType Word32

instance FFIType Word64 where
    ffiType = ffi_type_uint64
instance ArgType Word64
instance RetType Word64

outByRef :: OutArg a b -> OutArg (Ptr a) b
outByRef arg = composeOutArgs arg outArg

stringArg :: OutArg CString String
stringArg = outByRef (OutArg withCString)

instance (FFIType a, FFIType b)
        => FFIType (a, b) where
    ffiType = t
        where
            t = Type $ struct
                [ ffiTypeOf_ ((const Proxy :: Type (a,b) -> Proxy a) t)
                , ffiTypeOf_ ((const Proxy :: Type (a,b) -> Proxy b) t)
                ]

instance (FFIType a, FFIType b, FFIType c) 
        => FFIType (a, b, c) where
    ffiType = t
        where
            t = Type $ struct
                [ ffiTypeOf_ ((const Proxy :: Type (a,b,c) -> Proxy a) t)
                , ffiTypeOf_ ((const Proxy :: Type (a,b,c) -> Proxy b) t)
                , ffiTypeOf_ ((const Proxy :: Type (a,b,c) -> Proxy c) t)
                ]

instance (FFIType a, FFIType b, FFIType c, FFIType d) 
        => FFIType (a, b, c, d) where
    ffiType = t
        where
            t = Type $ struct
                [ ffiTypeOf_ ((const Proxy :: Type (a,b,c,d) -> Proxy a) t)
                , ffiTypeOf_ ((const Proxy :: Type (a,b,c,d) -> Proxy b) t)
                , ffiTypeOf_ ((const Proxy :: Type (a,b,c,d) -> Proxy c) t)
                , ffiTypeOf_ ((const Proxy :: Type (a,b,c,d) -> Proxy d) t)
                ]

instance (FFIType a, FFIType b, FFIType c, FFIType d, FFIType e)
        => FFIType (a, b, c, d, e) where
    ffiType = t
        where
            t = Type $ struct
                [ ffiTypeOf_ ((const Proxy :: Type (a,b,c,d,e) -> Proxy a) t)
                , ffiTypeOf_ ((const Proxy :: Type (a,b,c,d,e) -> Proxy b) t)
                , ffiTypeOf_ ((const Proxy :: Type (a,b,c,d,e) -> Proxy c) t)
                , ffiTypeOf_ ((const Proxy :: Type (a,b,c,d,e) -> Proxy d) t)
                , ffiTypeOf_ ((const Proxy :: Type (a,b,c,d,e) -> Proxy e) t)
                ]
