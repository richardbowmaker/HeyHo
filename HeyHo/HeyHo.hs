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
import Control.Applicative ((<$>), (<*>))

import Scintilla




foreign import ccall "libc.h exp" c_exp :: Double -> Double
foreign import ccall "math.h sin" c_sin :: CDouble -> CDouble
foreign import ccall unsafe "SetWindowPos" c_SetWindowPos :: HWND -> HWND -> Int -> Int -> Int -> Int -> Word32 -> IO (Int)


-- timesThree export
foreign export ccall timesThree :: Int -> Int
foreign import ccall safe "wrapper" createTimesThreePtr :: (Int -> Int) -> IO (FunPtr (Int -> Int))
timesThree :: Int -> Int
timesThree x = 3*x
 
-- Some IO callback export
foreign export ccall someIO :: Int -> IO (Int)
foreign import ccall safe "wrapper" createSomeIO :: (Int -> IO (Int)) -> IO (FunPtr (Int -> IO (Int)))
someIO :: Int -> IO (Int)
someIO x = do
    return (3*x)
  
someIO' :: Panel() -> Int -> IO (Int)
someIO' p x = do
    infoDialog p "mess1" "mess2"
    return (3*x)

  
    
main = start mainGUI

data Drag = Drag { downAt :: Point, at :: Point }

mainGUI :: IO ()
mainGUI = do 


    f <- frame [] 
    p1 <- panel  f []
    p2 <- panel  f []
    
    set f [ text:= "HeyHo", bgcolor := white]
           
    b  <- button f [text := "Test xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"]   
    set b [on command := cmdTest b]

    t1 <- textEntry f [text := "..."]
    t2 <- textEntry f [text := "..."]
 
 
--     someIOPtr <- createSomeIO (someIO' p)
--    c_SetFnPtr someIOPtr
  
    
--    timesThreePtr <- createTimesThreePtr timesThree
--    c_SetFnPtr timesThreePtr
--    n <- c_UseFnPtr 3
--    set txt [text := "answer = " ++ show n]
   

--  set f [ layout := minsize (sz 400 400) $ fill $ widget p]
    set f [ layout := column 10 [ (minsize (sz 200 200) $ fill $ widget p1), (minsize (sz 200 200) $ fill $ widget p2), (widget t1), (widget t2) ] ]
    set f [ size := (Size 500 500)]
    
--    scn <- peek c_GetFnPtr
--    set b [ text := "return " ++ showHex (scnNotifyGetWParam scn) "" ]
   
    hwnd1 <- windowGetHandle p1
    scn1 <- scnCreateEditor hwnd1
    scn1 <- scnEnableEvents scn1 $ eventHandler t1

    hwnd2 <- windowGetHandle p2
    scn2 <- scnCreateEditor hwnd2
    scn2 <- scnEnableEvents scn2 $ eventHandler t2

     


   
    --set p [on resize := scnSetSizeFromParent scn]

--    (Size x y) <- get p size
--    c_SetWindowPos scnHwnd hWND_TOP 0 0 x y sWP_NOMOVE

--    freeHaskellFunPtr timesThreePtr
    
    return ()
    
eventHandler :: TextCtrl () -> SCNotification -> IO ()
eventHandler t n = do
    set t [text := "event!! " ++ show (scnNotifyGetCode n) ++ "   "]
    repaint t
    return ()    
    
panelResize' :: Panel() -> HWND -> IO ()
panelResize' p hwnd = do
    (Size x y) <- get p size
    c_SetWindowPos hwnd hWND_TOP 0 0 x y sWP_NOMOVE
    return ()
  
panelResize :: ScnEditor -> IO ()
panelResize scn = do
--    scnSetSizeFromParent scn
    return ()
    
cmdTest :: Button () -> IO ()
cmdTest b = do
    set b [ text := "Clicked! "]
--    c_UnhookWindow
    return ()

    
winClose :: HWND ->  WindowMessage -> WPARAM -> LPARAM -> IO LRESULT
winClose _ _ _ _ = do 
    return (0)
   

    