{-# LANGUAGE TypeFamilies #-}
module Foreign.Dynamic
    ( Dyn, dyn, consDyn
    , importDynamic
    
    , Dynamic, dynamic
    , CVoid
    ) where

import Control.Applicative
import Control.Exception
import Foreign.LibFFI.Experimental.Base
import Foreign.LibFFI.Experimental.Types
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable

type Call = Ptr () -> Ptr (Ptr ()) -> IO ()
type WithArgs = (Ptr (Ptr ()) -> IO ()) -> IO ()

data Dyn a b = Dyn
    { argTypes :: [SomeType]
    , retType  ::  SomeType
    
    , prepDynamic :: Call -> Int -> IO (WithArgs -> a)
    }

dyn :: FFIType b => Ret_ a b -> Dyn (IO a) (IO b)
dyn ret = Dyn
    { argTypes = []
    , retType  = ffiTypeOf_ ret
    , prepDynamic = \call _ -> return $ \withArgs ->
        withRet_ ret (withArgs . call . castPtr)
    }

consDyn :: FFIType b => Arg a b -> Dyn c d -> Dyn (a -> c) (b -> d)
consDyn arg dyn = dyn
    { argTypes = ffiTypeOf_ arg : argTypes dyn
    , prepDynamic = \call i -> do
        dyn <- prepDynamic dyn call (i+1)
        return $ \withArgs x ->
            dyn $ \action ->
                withArg arg x $ \p ->
                    withArgs $ \args -> do
                        pokeElemOff args i (castPtr p)
                        action args
    }

importDynamic :: Dyn a b -> FunPtr b -> IO a
importDynamic dyn fun = do
    let nArgs  = length (argTypes dyn)

    -- these should not be freed as long as the returned @a@ is reachable
    cif <- CIF <$> mallocBytes sizeOfCIF
    argTypes <- newArray (argTypes dyn)

    -- TODO: check return code
    ffi_prep_cif cif defaultABI (fromIntegral nArgs) (retType dyn) argTypes 

    dyn <- prepDynamic dyn (ffi_call cif fun) 0
    let withArgs = bracket (mallocArray nArgs) free

    return $! dyn withArgs

dynamic :: Dynamic a => FunPtr (ForeignDyn a) -> IO a
dynamic = importDynamic stdDyn

class Dynamic a where
    type ForeignDyn a
    stdDyn :: Dyn a (ForeignDyn a)

instance RetType a => Dynamic (IO a) where
    type ForeignDyn (IO a) = IO (ForeignRet a)
    stdDyn = dyn retMarshaller_

instance (ArgType a, Dynamic b) => Dynamic (a -> b) where
    type ForeignDyn (a -> b) = ForeignArg a -> ForeignDyn b
    stdDyn = consDyn argMarshaller stdDyn
