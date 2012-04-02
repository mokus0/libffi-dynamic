{-# LANGUAGE ForeignFunctionInterface #-}
module Test where

import Data.Int
import Data.Word
import Data.IORef
import Foreign.C
import Foreign.Dynamic
import Foreign.Wrapper
import Foreign.LibFFI.Experimental.Base
import Foreign.LibFFI.Experimental.Types
import Foreign.Ptr
import Foreign.Storable

foreign import ccall "&printf" p_printf :: FunPtr (CString -> Int32 -> Int64 -> Double -> IO ())
foreign import ccall "&replicateM_" p_replicateM_ :: FunPtr (Word8 -> FunPtr (IO ()) -> IO Word8)
foreign import ccall "&mkPair" p_mkPair :: FunPtr (Word16 -> Float -> IO Pair)

data Pair = Pair Word16 Float deriving Show
instance Storable Pair where
    sizeOf    _ = 8
    alignment _ = 8
    peek p = do
        x <- peek        (castPtr p)
        y <- peekByteOff (castPtr p) 4
        return (Pair x y)
    poke p (Pair x y) = do
        poke        (castPtr p) x
        pokeByteOff (castPtr p) 4 y
instance FFIType Pair where
    ffiType = castType (ffiType :: Type (Word16, Float))
instance ArgType Pair
instance RetType Pair

main = do
    printf <- importDyn (stringArg `consDyn` dyn) p_printf

    printf "foo: %8d 0x%016lx %g\n"   42     0x0123456789abcdef  pi
    printf "bar: %-8d 0x%lx %.10a\n" (6 * 9) 0xfedcba9876543210 (exp pi - (20 + pi))
    printf "qux: %d/%d ~ %g\n" 7 22 pi
    
    replicateM_ <- dynamic p_replicateM_
    n <- newIORef 0
    action <- wrapper $ do
        i <- readIORef n
        writeIORef n $! (i + 1)
        putStrLn (replicate i ' ' ++ "Hi!")
    
    ct <- replicateM_ 10 action
    printf' <- importDyn (stringArg `consDyn` dyn)
        (castFunPtr p_printf :: FunPtr (CString -> Word8 -> IO ()))
    printf' "replicateM_ returned: %hhu\n" ct
    
    mkPair <- dynamic p_mkPair
    mkPair 12345 6.78e9 >>= print

