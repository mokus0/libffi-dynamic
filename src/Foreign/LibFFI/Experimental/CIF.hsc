{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Foreign.LibFFI.Experimental.CIF
    ( ABI(..)
    , defaultABI
    
    , SomeCIF(..)
    , getCIF
    
    , CIF(..)
    , toSomeCIF
    , cif
    , cifWithABI
    
    , abi
    , retType
    , argTypes, nArgs
    
    , SigType, SigReturn
    , retTypeOf, argTypesOf
    , ffi_call, call, callWithABI
    ) where

import Control.Applicative
import Data.Hashable
import Data.Interned
import Data.List
import Foreign.LibFFI.Experimental.Base
import Foreign.LibFFI.Experimental.FFIType
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

newtype SomeCIF = SomeCIF (Ptr SomeCIF)
    deriving (Eq, Ord, Show)

instance Interned SomeCIF where
    data Description SomeCIF = Sig ABI SomeType [SomeType]
        deriving (Eq, Show)
    type Uninterned SomeCIF = Description SomeCIF
    
    describe = id
    identify _ (Sig abi ret args) = unsafePerformIO $ do
        -- these should not be freed as long as the returned @a@ is reachable
        cif <- SomeCIF <$> mallocBytes (#size ffi_cif)
        
        let nArgs = fromIntegral (length args)
        argTypes <- newArray args
        
        -- TODO: check return code
        ffi_prep_cif cif abi nArgs ret argTypes 
        
        return cif
    
    cache = cifCache

{-# NOINLINE cifCache #-}
cifCache :: Cache SomeCIF
cifCache = mkCache

instance Hashable (Description SomeCIF) where
    hashWithSalt salt (Sig abi ret args) =
        foldl' hashWithSalt (hashWithSalt salt abi) (ret : args)

foreign import ccall ffi_prep_cif :: SomeCIF -> ABI -> CInt -> SomeType -> Ptr SomeType -> IO FFI_Status

getCIF :: ABI -> SomeType -> [SomeType] -> SomeCIF
getCIF abi retType argTypes = intern (Sig abi retType argTypes)

class SigType t where
    type SigReturn t
    
    retTypeOf'  :: p t ->  SomeType
    argTypesOf' :: p t -> [SomeType]

retTypeOf :: SigType t => p t -> SomeType
retTypeOf = retTypeOf'

argTypesOf :: SigType t => p t -> [SomeType]
argTypesOf = argTypesOf'

instance RetType t => SigType (IO t) where
    type SigReturn (IO t) = t
    
    retTypeOf' = ffiTypeOf_ . (const Nothing :: p (IO b) -> Maybe b)
    argTypesOf' _ = []

instance (ArgType a, SigType b) => SigType (a -> b) where
    type SigReturn (a -> b) = SigReturn b
    
    retTypeOf' = retTypeOf . (const Nothing :: p (a -> b) -> Maybe b)
    argTypesOf' p
        = ffiTypeOf_ ((const Nothing :: p (a -> b) -> Maybe a) p)
        : argTypesOf ((const Nothing :: p (a -> b) -> Maybe b) p)

newtype CIF a = CIF SomeCIF
    deriving (Eq, Ord, Show)

toSomeCIF :: CIF a -> SomeCIF
toSomeCIF (CIF c) = c

cif :: SigType t => CIF t
cif = cifWithABI defaultABI

cifWithABI :: SigType t => ABI -> CIF t
cifWithABI abi = theCIF
    where
        theCIF = CIF (getCIF abi (retTypeOf theCIF) (argTypesOf theCIF))

foreign import ccall ffi_call :: CIF a -> FunPtr a -> Ptr (SigReturn a) -> Ptr (Ptr ()) -> IO ()

call :: SigType t => FunPtr t -> Ptr (SigReturn t) -> Ptr (Ptr ()) -> IO ()
call = ffi_call theCIF
    where
        {-# NOINLINE theCIF #-}
        theCIF = cif

callWithABI :: SigType t => ABI -> FunPtr t -> Ptr (SigReturn t) -> Ptr (Ptr ()) -> IO ()
callWithABI abi = ffi_call theCIF
    where 
        {-# NOINLINE theCIF #-}
        theCIF = cifWithABI abi

abi :: CIF a -> ABI
abi (CIF (SomeCIF p)) = unsafePerformIO $ do
    (#peek ffi_cif, abi) p

retType :: CIF a -> SomeType
retType (CIF (SomeCIF p)) = unsafePerformIO $ do
    (#peek ffi_cif, rtype) p

argTypes :: CIF a -> [SomeType]
argTypes cif@(CIF (SomeCIF p)) = unsafePerformIO $ do
    ts <- (#peek ffi_cif, arg_types) p :: IO (Ptr SomeType)
    peekArray (nArgs cif) ts

nArgs :: CIF a -> Int
nArgs (CIF (SomeCIF p)) = unsafePerformIO $ do
    n  <- (#peek ffi_cif, nargs) p :: IO CUInt
    return $! fromIntegral n

