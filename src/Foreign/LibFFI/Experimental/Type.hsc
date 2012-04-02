{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Foreign.LibFFI.Experimental.Type
    ( SomeType(..), Type(..)
    , toSomeType, castType
    
    , void
    , pointer
    , float, double
    , sint8, sint16, sint32, sint64
    , uint8, uint16, uint32, uint64
    , struct
    , structElements
    ) where

import Data.Hashable
import Data.Int
import Data.Interned
import Data.List
import Data.Word
import Foreign.C.Types
import Foreign.Marshal hiding (void)
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe

newtype SomeType = SomeType (Ptr SomeType) deriving (Eq, Ord, Show, Storable)
instance Hashable SomeType where
    hashWithSalt salt (SomeType p) =
        hashWithSalt salt (fromIntegral (ptrToIntPtr p) :: Int)

newtype Type t = Type SomeType deriving (Eq, Ord, Show, Storable)

toSomeType :: Type a -> SomeType
toSomeType (Type t) = t

castType :: Type a -> Type b
castType (Type t) = Type t

#include <ffi.h>

foreign import ccall "&ffi_type_void"    void    :: Type ()
foreign import ccall "&ffi_type_pointer" pointer :: Type (Ptr a)

foreign import ccall "&ffi_type_float"  float  :: Type Float
foreign import ccall "&ffi_type_double" double :: Type Double

foreign import ccall "&ffi_type_sint8"  sint8  :: Type Int8
foreign import ccall "&ffi_type_sint16" sint16 :: Type Int16
foreign import ccall "&ffi_type_sint32" sint32 :: Type Int32
foreign import ccall "&ffi_type_sint64" sint64 :: Type Int64

foreign import ccall "&ffi_type_uint8"  uint8  :: Type Word8
foreign import ccall "&ffi_type_uint16" uint16 :: Type Word16
foreign import ccall "&ffi_type_uint32" uint32 :: Type Word32
foreign import ccall "&ffi_type_uint64" uint64 :: Type Word64

typeIsStruct :: SomeType -> Bool
typeIsStruct (SomeType p) = unsafePerformIO $ do
    t <- (#peek ffi_type, type) p :: IO CShort
    return $! t == #const FFI_TYPE_STRUCT

structElements :: SomeType -> [SomeType]
structElements st@(SomeType t)
    | typeIsStruct st = unsafePerformIO
        ((#peek ffi_type, elements) t >>= loop)
    | otherwise = []
        where
            nextPtr p = plusPtr p (sizeOf p)
            loop elems = do
                e <- peek elems
                return $! if e == SomeType nullPtr
                    then []
                    else e : unsafePerformIO (loop (nextPtr elems))

mkStruct :: [SomeType] -> IO SomeType
mkStruct ts = do
    t <- mallocBytes (#size ffi_type)
    
    (#poke ffi_type, size)      t (0 :: CSize)
    (#poke ffi_type, alignment) t (0 :: CShort)
    (#poke ffi_type, type)      t ((#const FFI_TYPE_STRUCT) :: CShort)
    (#poke ffi_type, elements)  t =<< newArray0 (SomeType nullPtr) ts
    
    return (SomeType t)

newtype Struct = Struct {structType :: SomeType}
    deriving (Eq, Ord, Show)

instance Interned Struct where
    data Description Struct = StructElems [SomeType]
        deriving (Eq, Ord, Show)
    
    type Uninterned Struct = [SomeType]
    
    describe = StructElems
    identify _ = Struct . unsafePerformIO . mkStruct
    
    cache = structCache

instance Uninternable Struct where
    unintern (Struct t) = structElements t

{-# NOINLINE structCache #-}
structCache :: Cache Struct
structCache = mkCache

instance Hashable (Description Struct) where
    hashWithSalt salt (StructElems ts) = foldl' (\s -> hashWithSalt s . f) salt ts
        where f (SomeType t) = fromIntegral (ptrToIntPtr t) :: Int

struct :: [SomeType] -> SomeType
struct = structType . intern
