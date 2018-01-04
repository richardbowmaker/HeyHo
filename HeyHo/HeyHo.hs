module Main where
import Graphics.UI.WX
import Graphics.UI.WXCore


import Foreign
import Foreign.C.Types
import Foreign.C.String
import System.Win32.DLL
import System.Win32.Types
import Graphics.Win32.Window
import Graphics.Win32.GDI.Types
import Graphics.Win32.Message

foreign import ccall "libc.h exp" c_exp :: Double -> Double
foreign import ccall "math.h sin" c_sin :: CDouble -> CDouble
foreign import ccall unsafe "fnTheDll" c_fnTheDll :: HWND -> IO (CInt)

main = start mainGUI

data Drag = Drag { downAt :: Point, at :: Point }

mainGUI :: IO ()
mainGUI = do 


    f <- frameFixed [] 
    
    set f [ text := "HeyHo", 
            bgcolor     := white, 
            layout      := space 400 400
          ]
           
    test <- button f [text := "Test "]
    
    set test [on command := cmdTest f test]
    
    return ()
    
{-
loadLibrary' :: String -> IO Int
loadLibrary' lib = do 
    cstr <- newCString lib
    hinstance <- c_loadLibrary' cstr
    return (fromIntegral hinstance)

    
marshallTypes :: String -> Int -> Double -> IO ()
marshallTypes s n d = do 
    cstr <- newCString s 
    c_marshallTypes cstr n d
    return ()
    
-}
    
cmdTest :: Frame () -> Button () -> IO ()
cmdTest f b = do
    set b [ text := "Clicked! "]
    hwnd <- windowGetHandle f 
    c_fnTheDll hwnd   
    return ()
    
    
    