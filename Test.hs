{-# LANGUAGE ForeignFunctionInterface #-}
module Test where

import Data.Int
import Foreign.C
import Foreign.Dynamic
import Foreign.Ptr

foreign import ccall "&printf" p_printf :: FunPtr (CString -> Int32 -> Int64 -> Double -> IO ())

main = do
    printf <- dynamic p_printf :: IO (String -> Int32 -> Int -> Double -> IO ())
    printf' <- dynamic p_printf :: IO (String -> Int32 -> Int64 -> Double -> IO ())

    printf "foo: %8d 0x%016lx %g\n"   42     0x0123456789abcdef  pi
    printf "bar: %-8d 0x%lx %.10a\n" (6 * 9) 0xfedcba9876543210 (exp pi - (20 + pi))
    
    printf' "qux: %d/%d ~ %g\n" 7 22 pi
