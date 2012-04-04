{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Foreign.Dynamic
    ( Dyn, mkDyn, consDyn
    , importDyn
    , importDynWithABI
    
    , Dynamic, dynamic, dyn
    ) where

import Control.Exception
import Foreign.LibFFI.Experimental.CIF
import Foreign.LibFFI.Experimental.FFIType
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable

type Call t = Ptr (SigReturn t) -> Ptr (Ptr ()) -> IO ()
type WithArgs = (Ptr (Ptr ()) -> IO ()) -> IO ()

newtype Dyn a b = Dyn
    { prepDynamic :: Call a -> Int -> IO (WithArgs -> b)
    }

mkDyn :: InRet a b -> Dyn (IO a) (IO b)
mkDyn ret = Dyn 
    { prepDynamic = \call _ -> return $ \withArgs ->
        withInRet ret (withArgs . call)
    }

consDyn :: OutArg a b -> Dyn c d -> Dyn (a -> c) (b -> d)
consDyn arg dyn = dyn
    { prepDynamic = \call i -> do
        dyn <- prepDynamic dyn call (i+1)
        return $ \withArgs x ->
            dyn $ \action ->
                withOutArg arg x $ \p ->
                    withArgs $ \args -> do
                        pokeElemOff args i (castPtr p)
                        action args
    }

importDyn :: SigType a => Dyn a b -> FunPtr a -> IO b
importDyn = importDynWithABI defaultABI

importDynWithABI :: SigType a => ABI -> Dyn a b -> FunPtr a -> IO b
importDynWithABI abi dyn fun = do
    let theCIF = cifWithABI abi
        n = nArgs theCIF
    
    dyn <- prepDynamic dyn (ffi_call theCIF fun) 0
    let withArgs = bracket (mallocArray n) free

    return $! dyn withArgs

dynamic :: Dynamic a => FunPtr a -> IO a
dynamic = importDyn stdDyn

dyn :: Dynamic a => Dyn a a
dyn = stdDyn

class SigType a => Dynamic a where
    stdDyn :: Dyn a a

instance RetType a => Dynamic (IO a) where
    stdDyn = mkDyn inRet

instance (ArgType a, Dynamic b) => Dynamic (a -> b) where
    stdDyn = consDyn outArg stdDyn
