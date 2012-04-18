{-# LANGUAGE BangPatterns #-}
module Foreign.Dynamic
    ( Dyn, mkDyn, consDyn
    , importDyn
    , importDynWithABI
    , importDynWithCIF
    , importDynWithCall
    
    , Dynamic, dynamic, dyn
    ) where

import Control.Exception
import Foreign.LibFFI.Experimental.CIF
import Foreign.LibFFI.Experimental.FFIType
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable

type Call t = Ptr (SigReturn t) -> Ptr (Ptr ()) -> IO ()
type WithArgs = Int -> (Ptr (Ptr ()) -> IO ()) -> IO ()

newtype Dyn a b = Dyn
    { prepDynamic :: Int -> WithArgs -> Call a -> b
    }

mkDyn :: InRet a b -> Dyn (IO a) (IO b)
mkDyn ret = Dyn 
    { prepDynamic = \n withArgs call ->
        withInRet ret (withArgs n . call)
    }

infixr 5 `consDyn`

consDyn :: OutArg a b -> Dyn c d -> Dyn (a -> c) (b -> d)
consDyn arg dyn = dyn
    { prepDynamic = \i withArgs call x ->
        let withMoreArgs n action = 
                withOutArg arg x $ \p ->
                    withArgs n $ \args -> do
                        pokeElemOff args i (castPtr p)
                        action args
         in prepDynamic dyn (i+1) withMoreArgs call
    }

importDyn :: SigType a => Dyn a b -> FunPtr a -> b
importDyn = importDynWithCIF cif

importDynWithABI :: SigType a => ABI -> Dyn a b -> FunPtr a -> b
importDynWithABI = importDynWithCIF . cifWithABI

importDynWithCIF :: CIF a -> Dyn a b -> FunPtr a -> b
importDynWithCIF = importDynWithCall . callWithCIF

importDynWithCall :: (FunPtr a -> Ptr (SigReturn a) -> Ptr (Ptr ()) -> IO ()) -> Dyn a b -> FunPtr a -> b
importDynWithCall !call !dyn =
    prepDynamic dyn 0 withArgs . call
        where
            withArgs n = bracket (mallocArray n) free

dynamic :: Dynamic a => FunPtr a -> a
dynamic = importDyn stdDyn

dyn :: Dynamic a => Dyn a a
dyn = stdDyn

class SigType a => Dynamic a where
    stdDyn :: Dyn a a

instance RetType a => Dynamic (IO a) where
    stdDyn = mkDyn inRet

instance (ArgType a, Dynamic b) => Dynamic (a -> b) where
    stdDyn = consDyn outArg stdDyn
