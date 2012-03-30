module Foreign.Dynamic (Dynamic, dynamic) where

import Control.Applicative
import Control.Exception
import Data.Proxy
import Foreign.LibFFI.Experimental.Base
import Foreign.LibFFI.Experimental.Types ({- instances -})
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable

type WithArgs = (Ptr (Ptr ()) -> IO ()) -> IO ()

class Dynamic a where
    argTypesByProxy :: Proxy a -> [SomeType]
    retTypeByProxy  :: Proxy a -> SomeType
    
    dynamic' :: CIF -> FunPtr a -> Int -> IO (WithArgs -> a)

proxyOf :: p a -> Proxy a
proxyOf = const Proxy

argTypesOf :: Dynamic a => p a -> [SomeType]
argTypesOf = argTypesByProxy . proxyOf

retTypeOf  :: Dynamic a => p a -> SomeType
retTypeOf = retTypeByProxy . proxyOf

dynamic :: Dynamic a => FunPtr a -> IO a
dynamic fun = do
    let rType  = retTypeOf  fun
        aTypes = argTypesOf fun
        nArgs  = length aTypes

    -- these should not be freed as long as the returned @a@ is reachable
    cif <- CIF <$> mallocBytes sizeOfCIF
    aTypes <- newArray aTypes

    -- TODO: check return code
    ffi_prep_cif cif defaultABI (fromIntegral nArgs) rType aTypes 

    dyn <- dynamic' cif fun 0
    let withArgs = bracket (mallocArray nArgs) free

    return $! dyn withArgs

ioRetProxy :: Proxy (IO a) -> Proxy (Returned a)
ioRetProxy = reproxy

instance RetType a => Dynamic (IO a) where
    argTypesByProxy _ = []
    retTypeByProxy = ffiTypeOf_ . ioRetProxy

    dynamic' cif fun _ = return $ \withArgs ->
        withRet_ (withArgs . ffi_call cif fun)

argProxy :: Proxy (a -> b) -> Proxy (Marshalled a)
argProxy = reproxy

retProxy :: Proxy (a -> b) -> Proxy b
retProxy = reproxy

instance (Show a, ArgType a, Dynamic b) => Dynamic (a -> b) where
    argTypesByProxy = liftA2 (:) (ffiTypeOf_ . argProxy) argTypesByProxy
    retTypeByProxy = retTypeByProxy . retProxy

    dynamic' cif fun i = do
        let castFn :: FunPtr (a -> b) -> FunPtr b
            castFn = castFunPtr
        rest <- dynamic' cif (castFn fun) (i+1)
        return $ \withArgs arg ->
            let withArgs' action = withArgs $ \args -> 
                    withArg arg $ \arg -> do
                        pokeElemOff args i (castPtr arg)
                        action args
             in rest withArgs'

