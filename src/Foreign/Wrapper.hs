{-# LANGUAGE RankNTypes #-}
module Foreign.Wrapper
    ( Wrap, mkWrap, consWrap
    , exportWrap
    , exportWrapWithABI
    
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
    { prepWrapper :: IO (a -> Ptr (Ptr ()) -> Ptr (SigReturn b) -> IO ())
    }

mkWrap :: OutRet a b -> Wrap (IO b) (IO a)
mkWrap ret = Wrap
    { prepWrapper = return $ \fun _ p -> fun >>= pokeRet ret (castPtr p)
    }

consWrap :: InArg a b -> Wrap c d -> Wrap (b -> c) (a -> d)
consWrap p wrap = wrap
    { prepWrapper = do
        wrap <- prepWrapper wrap
        return $ \fun args ret -> do
            withInArg p (castPtr args)
                (\arg -> wrap (fun arg)
                              (plusPtr args (sizeOf args)) 
                               ret)
    }

fromEntry :: Entry -> FunPtr a
fromEntry (Entry p) = castFunPtr p

exportWrap :: SigType b => Wrap a b -> a -> IO (FunPtr b)
exportWrap = exportWrapWithABI defaultABI

exportWrapWithABI :: SigType b => ABI -> Wrap a b -> a -> IO (FunPtr b)
exportWrapWithABI abi wrap fun = do
    let asWrappedTypeOf :: CIF b -> Wrap a b -> CIF b
        asWrappedTypeOf = const 
        
        cif = cifWithABI abi `asWrappedTypeOf` wrap
    
    wrap <- prepWrapper wrap
    impl <- wrap_FFI_Impl $ \_ ret args _ -> wrap fun args ret
    
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