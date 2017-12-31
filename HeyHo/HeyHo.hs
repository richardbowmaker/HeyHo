module Main where
import Graphics.UI.WX
import Graphics.UI.WXCore
import System.Random
import Data.List
import Debug.Trace
import System.IO
import Data.Char

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
-- foreign import ccall unsafe "ProxyLoadLibrary" c_loadLibrary' :: CString -> IO (CInt)
-- foreign import ccall unsafe "MarshallTypes" c_marshallTypes :: CString -> Int -> Double -> IO ()

main = start mainGUI

data Drag = Drag { downAt :: Point, at :: Point }

mainGUI :: IO ()
mainGUI = do 


    f <- frameFixed [] 
    
    set f [ text := "HeyHo", 
            bgcolor     := white, 
            layout      := space 400 400
          ]
          
    test <- button f [text := "Test"]
    
    set test [on command := cmdTest test]

--    hinstance <- loadLibrary' "D:\\_Rick's\\haskell\\ProxyDLL\\ProxyDLL.dll"

{-
    hinstance <- loadLibrary "MSIMG32.dll"
    hinstance <- loadLibrary "MSVCP140.dll"
    hinstance <- loadLibrary "VCRUNTIME140.dll"
    hinstance <- loadLibrary "thumbcache.dll"
    hinstance <- loadLibrary "WindowsCodecs.dll"
-}
    
--    hinstance <- loadLibrary "D:\\_Rick's\\haskell\\HeyHo\\SciLexer.dll"
--    hinstance <- loadLibraryEx "D:\\_Rick's\\haskell\\HeyHo\\SciLexer.dll" (castUINTPtrToPtr 0) 0
--   set test [text := "return: " ++ show(hinstance)]

    
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
    
cmdTest :: Button () -> IO ()
cmdTest b = do
--    n <- loadLibrary' "D:\\_Rick's\\haskell\\HeyHo\\SciLexer64.dll"
--    n <- loadLibrary' "MSIMG32.dll"
    hinstance <- loadLibrary "D:\\_Rick's\\haskell\\HeyHo\\SciLexer64.dll"
    set b [ text := "Clicked! " ++ show hinstance]
    createWindow (mkClassName "Scintilla") "Source" (wS_VSCROLL + wS_HSCROLL + wS_CLIPCHILDREN)
        Nothing Nothing Nothing Nothing Nothing Nothing hinstance winClose
--    marshallTypes "My Text" 45 6.789
    return ()
    
    
winClose :: HWND ->  WindowMessage -> WPARAM -> LPARAM -> IO LRESULT
winClose _ _ _ _ = do 
    return (0)
    
    