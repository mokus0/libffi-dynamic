{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Foreign.Dynamic
    ( Dyn, mkDyn, consDyn
    , importDyn
    
    , Dynamic, dynamic, dyn
    ) where

import Control.Applicative
import Control.Exception
import Data.Proxy
import Foreign.LibFFI.Experimental.Base
import Foreign.LibFFI.Experimental.Types ({- instances -})
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable

type Call = Ptr () -> Ptr (Ptr ()) -> IO ()
type WithArgs = (Ptr (Ptr ()) -> IO ()) -> IO ()

data Dyn a b = Dyn
    { abi      :: ABI
    , argTypes :: [SomeType]
    , retType  ::  SomeType
    
    , prepDynamic :: Call -> Int -> IO (WithArgs -> b)
    }

fstProxy :: p a b -> Proxy a
fstProxy _ = Proxy

mkDyn :: ABI -> FFIType a => InRet a b -> Dyn (IO a) (IO b)
mkDyn abi ret = Dyn
    { abi = defaultABI
    , argTypes = []
    , retType  = ffiTypeOf_ (fstProxy ret)
    , prepDynamic = \call _ -> return $ \withArgs ->
        withInRet ret (withArgs . call . castPtr)
    }

consDyn :: FFIType a => OutArg a b -> Dyn c d -> Dyn (a -> c) (b -> d)
consDyn arg dyn = dyn
    { argTypes = ffiTypeOf_ (fstProxy arg) : argTypes dyn
    , prepDynamic = \call i -> do
        dyn <- prepDynamic dyn call (i+1)
        return $ \withArgs x ->
            dyn $ \action ->
                withOutArg arg x $ \p ->
                    withArgs $ \args -> do
                        pokeElemOff args i (castPtr p)
                        action args
    }

importDyn :: Dyn a b -> FunPtr a -> IO b
importDyn dyn fun = do
    let nArgs  = length (argTypes dyn)

    -- these should not be freed as long as the returned @a@ is reachable
    cif <- CIF <$> mallocBytes (fromIntegral sizeOfCIF)
    argTypes <- newArray (argTypes dyn)

    -- TODO: check return code
    ffi_prep_cif cif (abi dyn) (fromIntegral nArgs) (retType dyn) argTypes 

    dyn <- prepDynamic dyn (ffi_call cif fun) 0
    let withArgs = bracket (mallocArray nArgs) free

    return $! dyn withArgs

dynamic :: Dynamic a => FunPtr a -> IO a
dynamic = importDyn stdDyn

dyn :: Dynamic a => Dyn a a
dyn = stdDyn

class Dynamic a where
    stdDyn :: Dyn a a

instance RetType a => Dynamic (IO a) where
    stdDyn = mkDyn defaultABI inRet

instance (ArgType a, Dynamic b) => Dynamic (a -> b) where
    stdDyn = consDyn outArg stdDyn
