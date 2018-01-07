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


main = start mainGUI

data Drag = Drag { downAt :: Point, at :: Point }

mainGUI :: IO ()
mainGUI = do 


    f <- frame []   
    p <- panel  f []

    set f [ text    := "HeyHo", 
            bgcolor := white, 
            layout  := space 500 500
          ]

    -- panel fills frame client area
--    windowSetLayout p (expand $ widget f)          
          
    {-
       
    hwnd <- windowGetHandle p   
    hinstance <- loadLibrary "D:\\_Rick's\\haskell\\HeyHo\\SciLexer64.dll"
    hwnd1 <- createWindow (mkClassName "Scintilla") "Source" (wS_VSCROLL + wS_HSCROLL + wS_CLIPCHILDREN) 
        (Just 100)  (Just 100)  (Just 500)  (Just 500) (Just hwnd) Nothing hinstance winClose                    
    showWindow hwnd1 sW_SHOW
    
   -}
   
    hmod <- getModuleHandle Nothing
    test <- button f [text := "Test " ++ (show hmod)]
   
    set test [on command := cmdTest f test]

     
    return ()
    
    
cmdTest :: Frame () -> Button () -> IO ()
cmdTest f b = do
    set b [ text := "Clicked! "]
    return ()
    
Func123 :: Int -> Int
Func123 x = 2*x  
    
winClose :: HWND ->  WindowMessage -> WPARAM -> LPARAM -> IO LRESULT
winClose _ _ _ _ = do 
    return (0)


    
    