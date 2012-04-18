{-# LANGUAGE BangPatterns #-}
module Foreign.Wrapper
    ( Wrap, mkWrap, consWrap
    , exportWrap
    , exportWrapWithABI
    , exportWrapWithCIF
    
    , Wrapper, wrap, wrapper
    ) where

import Foreign.LibFFI.Experimental.Base
import Foreign.LibFFI.Experimental.CIF
import Foreign.LibFFI.Experimental.Closure
import Foreign.LibFFI.Experimental.FFIType
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable

newtype Wrap a b = Wrap
    { prepWrapper :: a -> Ptr (Ptr ()) -> Ptr (SigReturn b) -> IO ()
    }

mkWrap :: OutRet a b -> Wrap (IO b) (IO a)
mkWrap ret = Wrap
    { prepWrapper = \fun _ p -> fun >>= pokeRet ret p
    }

infixr 5 `consWrap`

consWrap :: InArg a b -> Wrap c d -> Wrap (b -> c) (a -> d)
consWrap arg wrap = wrap
    { prepWrapper =
        \fun args ret -> do
            arg0 <- peek args
            withInArg arg (castPtr arg0)
                (\arg -> prepWrapper wrap
                    (fun arg)
                    (plusPtr args (sizeOf args)) 
                     ret)
    }

fromEntry :: Entry -> FunPtr a
fromEntry (Entry p) = castFunPtr p

exportWrap :: SigType b => Wrap a b -> a -> IO (FunPtr b)
exportWrap = exportWrapWithABI defaultABI

exportWrapWithABI :: SigType b => ABI -> Wrap a b -> a -> IO (FunPtr b)
exportWrapWithABI = exportWrapWithCIF . cifWithABI

exportWrapWithCIF :: CIF b -> Wrap a b -> a -> IO (FunPtr b)
exportWrapWithCIF !cif !wrap !fun = do
    impl <- wrap_FFI_Impl $ \_ ret args _ -> prepWrapper wrap fun args ret
    
    alloca $ \entryPtr -> do
        closure <- ffi_closure_alloc sizeOfClosure entryPtr
        entry <- peek entryPtr
        
        ffi_prep_closure_loc closure cif impl nullPtr entry
        
        return (fromEntry entry)

wrapper :: Wrapper a => a -> IO (FunPtr a)
wrapper = exportWrap stdWrap

class SigType a => Wrapper a where
    stdWrap :: Wrap a a

wrap :: Wrapper a => Wrap a a
wrap = stdWrap

instance RetType a => Wrapper (IO a) where
    stdWrap = mkWrap outRet

instance (ArgType a, Wrapper b) => Wrapper (a -> b) where
    stdWrap = consWrap inArg stdWrap