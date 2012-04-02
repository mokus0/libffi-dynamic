{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}
module Foreign.LibFFI.Experimental.Struct
    ( struct
    , someStruct
    , structElements
    
    , Description(Prim, Struct)
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
structElements st@(SomeType (Type t))
    | typeIsStruct st = unsafePerformIO
        ((#peek ffi_type, elements) t >>= loop)
    | otherwise = []
        where
            nextPtr p = plusPtr p (sizeOf p)
            loop elems = do
                e <- peek elems
                if e == nullPtr
                    then return []
                    else do
                        es <- unsafeInterleaveIO (loop (nextPtr elems))
                        return (SomeType (Type e):es)

mkStruct ts = do
    t     <- mallocBytes (#size ffi_type)
    elems <- newArray0 (SomeType (Type nullPtr)) ts
    
    (#poke ffi_type, size)      t (0 :: CSize)
    (#poke ffi_type, alignment) t (0 :: CShort)
    (#poke ffi_type, type)      t ((#const FFI_TYPE_STRUCT) :: CShort)
    (#poke ffi_type, elements)  t elems
    
    return (SomeType (Type t))


instance Interned SomeType where
    data Description SomeType 
        = Prim SomeType
        | Struct [Description SomeType]
        deriving (Eq, Ord, Show)
    
    type Uninterned SomeType = Description SomeType
    
    describe (Prim   t)  = unintern t
    describe (Struct ts) = Struct (map describe ts)
    
    identity (SomeType (Type t)) = fromIntegral (ptrToIntPtr t)
    
    identify _ (Prim    t) = t
    identify _ (Struct ts) = unsafePerformIO (mkStruct (map intern ts))
    
    cache = typeCache

instance Uninternable SomeType where
    unintern t
        | typeIsStruct t = Struct (map unintern (structElements t))
        | otherwise      = Prim t

{-# NOINLINE typeCache #-}
typeCache :: Cache SomeType
typeCache = mkCache

instance Hashable (Description SomeType) where
    hashWithSalt salt (Prim t) = salt `combine` identity t
    hashWithSalt salt (Struct ts) = foldl' hashWithSalt salt ts

someStruct :: [SomeType] -> SomeType
someStruct = intern . Struct . map Prim

struct :: [SomeType] -> Type a
struct ts = case someStruct ts of
    SomeType t -> castType t
