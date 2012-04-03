{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Foreign.LibFFI.Experimental.Type
    ( SomeType(..), Type(..)
    , toSomeType, castType
    
    , void
    , pointer
    , float, double, longdouble, floating
    , sint8, sint16, sint32, sint64, sint
    , uint8, uint16, uint32, uint64, uint
    , struct
    
    , typeIsStruct
    , structElements
    
    , TypeDescription(..)
    , describeType
    , getType
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

#include <ffi.h>

newtype SomeType = SomeType (Ptr SomeType) deriving (Eq, Ord, Show, Storable)
instance Hashable SomeType where
    hashWithSalt salt (SomeType p) =
        hashWithSalt salt (fromIntegral (ptrToIntPtr p) :: Int)

newtype Type t = Type SomeType deriving (Eq, Ord, Show, Storable)

toSomeType :: Type a -> SomeType
toSomeType (Type t) = t

castType :: Type a -> Type b
castType (Type t) = Type t

sizeOf1 :: Storable a => p a -> Int
sizeOf1 = sizeOf . (undefined :: p a -> a)

foreign import ccall "&ffi_type_void"    void    :: Type ()
foreign import ccall "&ffi_type_pointer" pointer :: Type (Ptr a)

foreign import ccall "&ffi_type_float"      float  :: Type Float
foreign import ccall "&ffi_type_double"     double :: Type Double
foreign import ccall "&ffi_type_longdouble" longdouble :: Type Double

floating :: Storable a => Type a
floating = t
    where
        t = case sizeOf1 t of
            4 -> castType float
            8 -> castType double
            (#const sizeof(long double)) -> castType longdouble
            _ -> error "floating: invalid size for floating point type"

foreign import ccall "&ffi_type_sint8"  sint8  :: Type Int8
foreign import ccall "&ffi_type_sint16" sint16 :: Type Int16
foreign import ccall "&ffi_type_sint32" sint32 :: Type Int32
foreign import ccall "&ffi_type_sint64" sint64 :: Type Int64

sint :: Storable a => Type a
sint = t
    where
        t = case sizeOf1 t of
            1 -> castType sint8
            2 -> castType sint16
            4 -> castType sint32
            8 -> castType sint64
            _ -> error "sint: invalid size for signed int type"

foreign import ccall "&ffi_type_uint8"  uint8  :: Type Word8
foreign import ccall "&ffi_type_uint16" uint16 :: Type Word16
foreign import ccall "&ffi_type_uint32" uint32 :: Type Word32
foreign import ccall "&ffi_type_uint64" uint64 :: Type Word64

uint :: Storable a => Type a
uint = t
    where
        t = case sizeOf1 t of
            1 -> castType uint8
            2 -> castType uint16
            4 -> castType uint32
            8 -> castType uint64
            _ -> error "uint: invalid size for unsigned int type"

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

newtype StructType = StructType {structType :: SomeType}
    deriving (Eq, Ord, Show)

instance Interned StructType where
    data Description StructType = StructElems [SomeType]
        deriving (Eq, Ord, Show)
    
    type Uninterned StructType = [SomeType]
    
    describe = StructElems
    identify _ = StructType . unsafePerformIO . mkStruct
    
    cache = structTypeCache

instance Uninternable StructType where
    unintern (StructType t) = structElements t

{-# NOINLINE structTypeCache #-}
structTypeCache :: Cache StructType
structTypeCache = mkCache

instance Hashable (Description StructType) where
    hashWithSalt salt (StructElems ts) = foldl' (\s -> hashWithSalt s . f) salt ts
        where f (SomeType t) = fromIntegral (ptrToIntPtr t) :: Int

struct :: [SomeType] -> SomeType
struct = structType . intern

data TypeDescription
    = Prim SomeType
    | Struct [TypeDescription]
    deriving (Eq, Ord, Show)

describeType :: SomeType -> TypeDescription
describeType t
    | typeIsStruct t    = Struct (map describeType (structElements t))
    | otherwise         = Prim t

getType :: TypeDescription -> SomeType
getType (Prim t)    = t
getType (Struct ts) = struct (map getType ts)