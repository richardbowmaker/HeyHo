{-# LANGUAGE ForeignFunctionInterface #-}

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
import Numeric


foreign import ccall "libc.h exp" c_exp :: Double -> Double
foreign import ccall "math.h sin" c_sin :: CDouble -> CDouble
foreign import ccall unsafe "fnTheDll" c_fnTheDll :: HWND -> IO (CInt)
foreign import ccall unsafe "SetWindowPos" c_SetWindowPos :: HWND -> HWND -> Int -> Int -> Int -> Int -> Word32 -> IO (Int)

foreign import ccall "GetFnPtr" c_GetFnPtr :: Ptr (Int -> Int)
foreign import ccall "dynamic" doublePtr :: FunPtr (Int -> Int) -> (Int -> Int)

myDouble :: Int -> Int
myDouble = doublePtr (castPtrToFunPtr c_GetFnPtr)



main = start mainGUI

data Drag = Drag { downAt :: Point, at :: Point }

mainGUI :: IO ()
mainGUI = do 

    f <- frame [] 
    p <- panel  f []
    
    set f [ text:= "HeyHo", bgcolor := white]
           
    b  <- button f [text := "Test "]   
    -- set test [on command := cmdTest f test]

    -- panel fills frame client area
    set p [ bgcolor := red]


    -- 
--    set f [ layout := container p $ minsize (sz 200 200) $ fill $ label "x:"]
--    set f [ layout := minsize (sz 400 400) $ fill $ widget p]
--    windowSetSize f (Rect 0 0 500 500) wxSIZE_USE_EXISTING 
    set f [ size := (Size 500 500)]
    
    
  
    hwnd <- windowGetHandle p 
--    n <- c_fnTheDll hwnd

    set b [ text := "return " ++ show (myDouble 12)]

{-
    hwnd <- windowGetHandle p   
    hinstance <- loadLibrary "D:\\_Rick's\\haskell\\HeyHo\\SciLexer64.dll"
    hwnd1 <- createWindow (mkClassName "Scintilla") "Source" (wS_CHILD + wS_VSCROLL + wS_HSCROLL + wS_CLIPCHILDREN) 
        (Just 0)  (Just 0)  (Just 500)  (Just 500) (Just hwnd) Nothing hinstance winClose                    
    showWindow hwnd1 sW_SHOW
        
   
    set p [on resize := panelResize p hwnd1]
-}
    
    return ()
    
panelResize :: Panel() -> HWND -> IO ()
panelResize p hwnd = do
    (Size x y) <- get p size
    c_SetWindowPos hwnd hWND_TOP 0 0 x y sWP_NOMOVE
    return ()

    

       
{-   
cmdTest :: Panel () -> Button () -> IO ()
cmdTest p b = do
    set b [ text := "Clicked! "]
    hwnd <- windowGetHandle p 
    c_fnTheDll hwnd
    return ()
-}
    
winClose :: HWND ->  WindowMessage -> WPARAM -> LPARAM -> IO LRESULT
winClose _ _ _ _ = do 
    return (0)
        
    
    
    