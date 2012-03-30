{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}
module Foreign.LibFFI.Experimental.Types
    ( CVoid
    ) where

import Data.Int
import Data.Word
import Foreign.C
import Foreign.LibFFI.Experimental.Base
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable

foreign import ccall "&ffi_type_void"    ffi_type_void    :: Type CVoid
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


-- silly type to avoid orphan instance Storable ()
data CVoid = CVoid deriving (Eq, Ord, Read, Show, Enum, Bounded)
instance Storable CVoid where
    sizeOf    _  = 0
    alignment _  = 0
    peek _       = return CVoid
    poke _ CVoid = return ()
instance FFIType CVoid where
    ffiType = ffi_type_void
instance RetType () where
    type Returned () = CVoid
    toReturned () = CVoid
    fromReturned CVoid = ()

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
    type Marshalled Int = Int64
    withArg = withArg . (fromIntegral :: Int -> Int64)
instance RetType Int where
    type Returned Int = Int64
    toReturned   = fromIntegral
    fromReturned = fromIntegral

instance FFIType Int8 where
    ffiType = ffi_type_sint8
instance ArgType Int8
instance RetType Int8 where
    type Returned Int8 = Int64
    toReturned   = fromIntegral
    fromReturned = fromIntegral

instance FFIType Int16 where
    ffiType = ffi_type_sint16
instance ArgType Int16
instance RetType Int16 where
    type Returned Int16 = Int64
    toReturned   = fromIntegral
    fromReturned = fromIntegral

instance FFIType Int32 where
    ffiType = ffi_type_sint32
instance ArgType Int32
instance RetType Int32 where
    type Returned Int32 = Int64
    toReturned   = fromIntegral
    fromReturned = fromIntegral

instance FFIType Int64 where
    ffiType = ffi_type_sint64
instance ArgType Int64
instance RetType Int64

instance ArgType Word where
    type Marshalled Word = Word64
    withArg = withArg . (fromIntegral :: Word -> Word64)
instance RetType Word where
    type Returned Word = Word64
    toReturned   = fromIntegral
    fromReturned = fromIntegral

instance FFIType Word8 where
    ffiType = ffi_type_uint8
instance ArgType Word8
instance RetType Word8 where
    type Returned Word8 = Word64
    toReturned   = fromIntegral
    fromReturned = fromIntegral

instance FFIType Word16 where
    ffiType = ffi_type_uint16
instance ArgType Word16
instance RetType Word16 where
    type Returned Word16 = Word64
    toReturned   = fromIntegral
    fromReturned = fromIntegral

instance FFIType Word32 where
    ffiType = ffi_type_uint32
instance ArgType Word32
instance RetType Word32 where
    type Returned Word32 = Word64
    toReturned   = fromIntegral
    fromReturned = fromIntegral

instance FFIType Word64 where
    ffiType = ffi_type_uint64
instance ArgType Word64
instance RetType Word64

-- hmm... this is questionable, I think...
instance ArgType [Char] where
    type Marshalled String = CString
    withArg str action = withCString str $ \cStr -> with cStr action

