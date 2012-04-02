{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Foreign.LibFFI.Experimental.CIF
    ( ABI(..)
    , defaultABI
    
    , CIF(..)
    , getCIF
    
    , ffi_call
    ) where

import Control.Applicative
import Data.Hashable
import Data.Interned
import Data.List
import Foreign.LibFFI.Experimental.Base
import Foreign.LibFFI.Experimental.Type
import Foreign.C.Types
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe

#include <ffi.h>

newtype ABI = ABI CInt
    deriving (Eq, Ord, Show, Storable)

instance Hashable ABI where
    hashWithSalt salt (ABI x) = 
        hashWithSalt salt (fromIntegral x :: Int)

defaultABI = ABI (#const FFI_DEFAULT_ABI)

newtype CIF = CIF (Ptr CIF)

instance Interned CIF where
    data Description CIF = Sig ABI SomeType [SomeType]
        deriving (Eq, Show)
    type Uninterned CIF = Description CIF
    
    describe = id
    identify _ (Sig abi ret args) = unsafePerformIO $ do
        -- these should not be freed as long as the returned @a@ is reachable
        cif <- CIF <$> mallocBytes (#size ffi_cif)
        
        let nArgs = fromIntegral (length args)
        argTypes <- newArray args
        
        -- TODO: check return code
        ffi_prep_cif cif abi nArgs ret argTypes 
        
        return cif
    
    cache = cifCache

{-# NOINLINE cifCache #-}
cifCache :: Cache CIF
cifCache = mkCache

instance Hashable (Description CIF) where
    hashWithSalt salt (Sig abi ret args) =
        foldl' hashWithSalt (hashWithSalt salt abi) (ret : args)

foreign import ccall ffi_prep_cif :: CIF -> ABI -> CInt -> SomeType -> Ptr SomeType -> IO FFI_Status
foreign import ccall ffi_call :: CIF -> FunPtr a -> Ptr r -> Ptr (Ptr ()) -> IO ()

getCIF :: ABI -> SomeType -> [SomeType] -> CIF
getCIF abi retType argTypes = intern (Sig abi retType argTypes)