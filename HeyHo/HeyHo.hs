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

foreign import ccall unsafe "fnTheDll" c_fnTheDll :: HWND -> IO (CInt)
foreign import ccall "GetFnPtr" c_GetFnPtr :: Ptr SCNotification
foreign import ccall "dynamic" doublePtr :: FunPtr (Int -> Int) -> (Int -> Int)

foreign import ccall unsafe "SetFnPtr" c_SetFnPtr :: FunPtr (Int -> Int) -> IO ()
foreign import ccall unsafe "UseFnPtr" c_UseFnPtr :: Int -> IO (Int)

foreign import ccall unsafe "HookWindow" c_HookWindow :: IO (CInt)
foreign import ccall unsafe "UnhookWindow" c_UnhookWindow :: IO ()


myDouble :: Int -> Int
myDouble = doublePtr (castPtrToFunPtr c_GetFnPtr)

-- timesThree export
foreign export ccall timesThree :: Int -> Int
foreign import ccall safe "wrapper" createTimesThreePtr :: (Int -> Int) -> IO (FunPtr (Int -> Int))
timesThree :: Int -> Int
timesThree x = 3*x
 

            

main = start mainGUI

data Drag = Drag { downAt :: Point, at :: Point }

mainGUI :: IO ()
mainGUI = do 


    f <- frame [] 
    p <- panel  f []
    
    set f [ text:= "HeyHo", bgcolor := white]
           
    b  <- button f [text := "Test xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"]   
    set b [on command := cmdTest b]

    txt <- textEntry f [text := "..."]
 
 

--    timesThreePtr <- createTimesThreePtr timesThree
--    c_SetFnPtr timesThreePtr
--    n <- c_UseFnPtr 3
   

--  set f [ layout := minsize (sz 400 400) $ fill $ widget p]
    set f [ layout := column 10 [ (minsize (sz 400 400) $ fill $ widget p), (widget txt) ] ]
    set f [ size := (Size 500 500)]
    
--    scn <- peek c_GetFnPtr
--    set b [ text := "return " ++ showHex (scnNotifyGetWParam scn) "" ]
   
    hwnd <- windowGetHandle p
    scn <- scnCreateEditor hwnd

     

--    c_HookWindow




       

   
    set p [on resize := scnSetSizeFromParent scn]

    (Size x y) <- get p size
--    c_SetWindowPos scnHwnd hWND_TOP 0 0 x y sWP_NOMOVE

--    freeHaskellFunPtr timesThreePtr
    
    return ()
    
    
panelResize' :: Panel() -> HWND -> IO ()
panelResize' p hwnd = do
    (Size x y) <- get p size
    c_SetWindowPos hwnd hWND_TOP 0 0 x y sWP_NOMOVE
    return ()

   
panelResize :: ScnEditor -> IO ()
panelResize scn = do
    scnSetSizeFromParent scn
    return ()
    
cmdTest :: Button () -> IO ()
cmdTest b = do
    set b [ text := "Clicked! "]
--    c_UnhookWindow
    return ()

    
winClose :: HWND ->  WindowMessage -> WPARAM -> LPARAM -> IO LRESULT
winClose _ _ _ _ = do 
    return (0)
   

    