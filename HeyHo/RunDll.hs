module Main where

import Foreign.Ptr
import Foreign.C.Types 
import Data.Int

foreign import ccall unsafe "fnTestDll" c_someFn :: CInt
mySomeFn = c_someFn

main :: IO ()
main = do let z = mySomeFn
          print z
          
          