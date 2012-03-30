{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}
module Foreign.LibFFI.Experimental.Types ({- instances -}) where

import Data.Int
import Data.Word
import Foreign.C
import Foreign.LibFFI.Experimental.Base
import Foreign.Marshal
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
    retMarshaller_ = Ret_ $ \action -> do
        action nullPtr
        return ()

instance FFIType (Ptr a) where
    ffiType = ffi_type_pointer

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
instance ArgType Int where
    type ForeignArg Int = Int64
    argMarshaller = Arg (withArg int64arg . fromIntegral)
        where int64arg = argMarshaller :: Arg Int64 Int64
instance RetType Int where
    type ForeignRet Int = Int64
    retMarshaller_ = Ret_ $ fmap fromIntegral . withRet_ int64ret
        where int64ret = retMarshaller_ :: Ret_ Int64 Int64

instance FFIType Int8 where
    ffiType = ffi_type_sint8
    type Returned Int8 = Int64
    toReturned   = fromIntegral
    fromReturned = fromIntegral
instance ArgType Int8
instance RetType Int8

instance FFIType Int16 where
    ffiType = ffi_type_sint16
    type Returned Int16 = Int64
    toReturned   = fromIntegral
    fromReturned = fromIntegral
instance ArgType Int16
instance RetType Int16

instance FFIType Int32 where
    ffiType = ffi_type_sint32
    type Returned Int32 = Int64
    toReturned   = fromIntegral
    fromReturned = fromIntegral
instance ArgType Int32
instance RetType Int32

instance FFIType Int64 where
    ffiType = ffi_type_sint64
instance ArgType Int64
instance RetType Int64

instance ArgType Word where
    type ForeignArg Word = Word64
    argMarshaller = Arg (withArg word64arg . fromIntegral)
        where word64arg = argMarshaller :: Arg Word64 Word64
instance RetType Word where
    type ForeignRet Word = Word64
    retMarshaller_ = Ret_ $ fmap fromIntegral . withRet_ word64ret
        where word64ret = retMarshaller_ :: Ret_ Word64 Word64

instance FFIType Word8 where
    ffiType = ffi_type_uint8
    type Returned Word8 = Word64
    toReturned   = fromIntegral
    fromReturned = fromIntegral
instance ArgType Word8
instance RetType Word8

instance FFIType Word16 where
    ffiType = ffi_type_uint16
    type Returned Word16 = Word64
    toReturned   = fromIntegral
    fromReturned = fromIntegral
instance ArgType Word16
instance RetType Word16

instance FFIType Word32 where
    ffiType = ffi_type_uint32
    type Returned Word32 = Word64
    toReturned   = fromIntegral
    fromReturned = fromIntegral
instance ArgType Word32
instance RetType Word32

instance FFIType Word64 where
    ffiType = ffi_type_uint64
instance ArgType Word64
instance RetType Word64

-- hmm... this is questionable, I think...
instance ArgType [Char] where
    type ForeignArg String = CString
    argMarshaller = Arg $ \str -> withCString str . flip with
        

