module Foreign.Dynamic (Dynamic, dynamic) where

import Control.Applicative
import Control.Exception
import Data.Tagged
import Foreign.LibFFI.Experimental.Base
import Foreign.LibFFI.Experimental.Types ({- instances -})
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable

type WithArgs = (Ptr (Ptr ()) -> IO ()) -> IO ()

class Dynamic a where
    argTypes :: Tagged a [SomeType]
    retType  :: Tagged a SomeType
    dynamic' :: CIF -> FunPtr a -> Int -> IO (WithArgs -> a)

argTypesOf :: Dynamic a => FunPtr a -> [SomeType]
argTypesOf = (const . untag :: Tagged a t -> FunPtr a -> t) argTypes
retTypeOf  :: Dynamic a => FunPtr a -> SomeType
retTypeOf = (const . untag :: Tagged a t -> FunPtr a -> t) retType

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

instance RetType a => Dynamic (IO a) where
    argTypes = Tagged []
    retType = wrap ffiType
        where
            wrap :: Type (Returned t) -> Tagged (IO t) SomeType
            wrap t = Tagged (toSomeType t)

    dynamic' cif fun _ = return $ \withArgs ->
        withRet_ (withArgs . ffi_call cif fun)

instance (Show a, ArgType a, Dynamic b) => Dynamic (a -> b) where
    argTypes = cons ffiType argTypes
        where
            cons :: Type (Marshalled a) -> Tagged b [SomeType] -> Tagged (a -> b) [SomeType]
            cons t (Tagged ts) = Tagged (toSomeType t : ts)
    
    retType = tl retType
        where
            tl :: Tagged b SomeType -> Tagged (a -> b) SomeType
            tl (Tagged t) = Tagged t

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

