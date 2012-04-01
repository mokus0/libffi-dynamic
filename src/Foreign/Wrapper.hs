{-# LANGUAGE RankNTypes #-}
module Foreign.Wrapper where

import Control.Applicative
import Data.Proxy
import Foreign.LibFFI.Experimental.Base
import Foreign.LibFFI.Experimental.Types ({- instances -})
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable

data Wrap a b = Wrap
    { abi      :: ABI
    , argTypes :: [SomeType]
    , retType  ::  SomeType
    
    , prepWrapper :: IO (a -> Ptr (Ptr  ()) -> Ptr () -> IO ())
    }

fstProxy :: p a b -> Proxy a
fstProxy _ = Proxy

mkWrap :: FFIType a => ABI -> OutRet a b -> Wrap (IO b) (IO a)
mkWrap abi ret = Wrap
    { abi = abi
    , argTypes = []
    , retType = ffiTypeOf_ (fstProxy ret)
    
    , prepWrapper = return $ \fun -> const (withOutRet ret fun . castPtr)
    }

consWrap :: FFIType a => InArg a b -> Wrap c d -> Wrap (b -> c) (a -> d)
consWrap p wrap = wrap
    { argTypes = ffiTypeOf_ (fstProxy p) : argTypes wrap
    
    , prepWrapper = do
        wrap <- prepWrapper wrap
        return $ \fun args ret -> do
            withInArg p (castPtr args)
                (\arg -> wrap (fun arg)
                              (plusPtr args (sizeOf args)) 
                               ret)
    }

fromEntry :: Entry -> FunPtr a
fromEntry (Entry p) = castFunPtr p

exportWrap :: Wrap a b -> a -> IO (FunPtr b)
exportWrap wrap fun = do
    let nArgs  = length (argTypes wrap)
    
    -- these should not be freed as long as the returned @FunPtr@ is reachable
    cif <- CIF <$> mallocBytes (fromIntegral sizeOfCIF)
    argTypes <- newArray (argTypes wrap)
    
    -- TODO: check return code
    ffi_prep_cif cif (abi wrap) (fromIntegral nArgs) (retType wrap) argTypes 
    
    wrap <- prepWrapper wrap
    impl <- wrap_FFI_Impl $ \_ ret args _ -> wrap fun args ret
        
    alloca $ \entryPtr -> do
        closure <- ffi_closure_alloc sizeOfClosure entryPtr
        entry <- peek entryPtr
        
        ffi_prep_closure_loc closure cif impl nullPtr entry
        
        return (fromEntry entry)

wrapper :: Wrapper a => a -> IO (FunPtr a)
wrapper = exportWrap stdWrap

class Wrapper a where
    stdWrap :: Wrap a a

instance RetType a => Wrapper (IO a) where
    stdWrap = mkWrap defaultABI outRet

instance (ArgType a, Wrapper b) => Wrapper (a -> b) where
    stdWrap = consWrap inArg stdWrap