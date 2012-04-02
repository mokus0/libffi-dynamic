{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}
module Foreign.LibFFI.Experimental.Struct
    ( struct
    , structElements
    ) where

import Data.Hashable
import Data.Interned
import Data.List
import Foreign.C.Types
import Foreign.LibFFI.Experimental.Base
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe

#include <ffi.h>

foreign import ccall typeIsStruct :: SomeType -> Bool

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
    
    cache = typeCache

instance Uninternable Struct where
    unintern (Struct t) = structElements t

{-# NOINLINE typeCache #-}
typeCache :: Cache Struct
typeCache = mkCache

instance Hashable (Description Struct) where
    hashWithSalt salt (StructElems ts) = foldl' (\s -> hashWithSalt s . f) salt ts
        where f (SomeType t) = fromIntegral (ptrToIntPtr t) :: Int

struct :: [SomeType] -> SomeType
struct = structType . intern
