{-# LANGUAGE ForeignFunctionInterface #-}
-- To run this example:
--  gcc -c test-c.c
--  ghc -O Test -main-is Test test-c.o
--  ./Test
module Test where

import Data.Int
import Data.Word
import Data.IORef
import Foreign.C
import Foreign.LibFFI.Dynamic
import Foreign.Ptr
import Foreign.Storable

foreign import ccall "&printf" p_printf :: FunPtr (CString -> Int32 -> Int64 -> Double -> IO ())
foreign import ccall "&replicateM_" p_replicateM_ :: FunPtr (Word8 -> FunPtr (CInt -> IO CInt) -> IO Word8)
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

printf = importDyn (stringArg `consDyn` dyn) p_printf
printf' = importDyn (stringArg `consDyn` dyn)
    (castFunPtr p_printf :: FunPtr (CString -> Word8 -> IO ()))
replicateM_ = dynamic p_replicateM_
mkPair = dynamic p_mkPair

main = do
    printf "foo: %8d 0x%016lx %g\n"   42     0x0123456789abcdef  pi
    printf "bar: %-8d 0x%lx %.10a\n" (6 * 9) 0xfedcba9876543210 (exp pi - (20 + pi))
    printf "qux: %d/%d ~ %g\n" 7 22 pi
    
    action <- wrapper $ \i -> do
        putStrLn (show i ++ ": " ++ "Hi!")
        return (i * 2 + 1)
    
    ct <- replicateM_ 10 action
    printf' "replicateM_ returned: %hhu\n" ct
    
    mkPair 12345 6.78e9 >>= print

