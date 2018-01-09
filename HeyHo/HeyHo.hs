{-# LANGUAGE ForeignFunctionInterface #-}

module Main where
import Graphics.UI.WX
import Graphics.UI.WXCore


import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Storable

import System.Win32.DLL
import System.Win32.Types
import Graphics.Win32.Window
import Graphics.Win32.GDI.Types
import Graphics.Win32.Message

import Numeric
import Control.Applicative ((<$>), (<*>))

foreign import ccall "libc.h exp" c_exp :: Double -> Double
foreign import ccall "math.h sin" c_sin :: CDouble -> CDouble
foreign import ccall unsafe "fnTheDll" c_fnTheDll :: HWND -> IO (CInt)
foreign import ccall unsafe "SetWindowPos" c_SetWindowPos :: HWND -> HWND -> Int -> Int -> Int -> Int -> Word32 -> IO (Int)

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
 
data MyStruct = MyStruct
            { d :: Double
            , c :: Word8
            , i :: Int32
            } 

instance Storable MyStruct where
        alignment _ = 8
        sizeOf _    = 16
        peek ptr    = MyStruct
            <$> peekByteOff ptr 0
            <*> peekByteOff ptr 8
            <*> peekByteOff ptr 12 -- skip padding bytes after "c"
        poke ptr (MyStruct d c i) = do
            pokeByteOff ptr 0 d
            pokeByteOff ptr 8 c
            pokeByteOff ptr 12 i 
            
       
-- Structure for Scintilla Notification (64 bit version)
-- See Scintilla.h SCNotification for original       
data  SCNotification = SCNotification
            {
                ptrHwndFrom     :: Word64,
                idFrom          :: Word64,
                code            :: Word32,
                
                position        :: Int64,
                -- SCN_STYLENEEDED, SCN_DOUBLECLICK, SCN_MODIFIED, SCN_MARGINCLICK, 
                -- SCN_NEEDSHOWN, SCN_DWELLSTART, SCN_DWELLEND, SCN_CALLTIPCLICK, 
                -- SCN_HOTSPOTCLICK, SCN_HOTSPOTDOUBLECLICK, SCN_HOTSPOTRELEASECLICK, 
                -- SCN_INDICATORCLICK, SCN_INDICATORRELEASE, 
                -- SCN_USERLISTSELECTION, SCN_AUTOCSELECTION            
                ch              :: Int32,
                -- SCN_CHARADDED, SCN_KEY, SCN_AUTOCCOMPLETED, SCN_AUTOCSELECTION, 
                -- SCN_USERLISTSELECTION 
                modifiers       :: Int32,
                -- SCN_KEY, SCN_DOUBLECLICK, SCN_HOTSPOTCLICK, SCN_HOTSPOTDOUBLECLICK, 
                -- SCN_HOTSPOTRELEASECLICK, SCN_INDICATORCLICK, SCN_INDICATORRELEASE, 

                modificationType :: Int32, -- SCN_MODIFIED 
                ptrText         :: Word64,
                -- SCN_MODIFIED, SCN_USERLISTSELECTION, SCN_AUTOCSELECTION, SCN_URIDROPPED 

                length          :: Int64, 
                linesAdded      :: Int64,  -- SCN_MODIFIED 
                message         :: Int32,  -- SCN_MACRORECORD 
                wParam          :: Word64, -- SCN_MACRORECORD
                lParam          :: Int64,  -- SCN_MACRORECORD
                line            :: Int64,  -- SCN_MODIFIED
                foldLevelNow    :: Int32,  -- SCN_MODIFIED
                foldLevelPrev   :: Int32,  -- SCN_MODIFIED 
                margin          :: Int32,  -- SCN_MARGINCLICK 
                listType        :: Int32,  -- SCN_USERLISTSELECTION 
                x               :: Int32,  -- SCN_DWELLSTART, SCN_DWELLEND 
                y               :: Int32,  -- SCN_DWELLSTART, SCN_DWELLEND 
                token           :: Int32,  -- SCN_MODIFIED with SC_MOD_CONTAINER 
                annotationLinesAdded :: Int64, -- SCN_MODIFIED with SC_MOD_CHANGEANNOTATION 
                updated         :: Int32,  -- SCN_UPDATEUI 
                listCompletionMethod :: Int32
                -- SCN_AUTOCSELECTION, SCN_AUTOCCOMPLETED, SCN_USERLISTSELECTION,    
            }

-- 0 8 16 24 32 36 40 48 56 64 72 80 88 96 104 108 112 116 120 124 128 136 144 148             
instance Storable SCNotification where
        alignment _ = 8
        sizeOf _    = 152
        peek ptr    = SCNotification
            <$> peekByteOff ptr 0
            <*> peekByteOff ptr 8
            <*> peekByteOff ptr 16 
            <*> peekByteOff ptr 24
            <*> peekByteOff ptr 32
            <*> peekByteOff ptr 36
            <*> peekByteOff ptr 40  
            <*> peekByteOff ptr 48
            <*> peekByteOff ptr 56
            <*> peekByteOff ptr 64
            <*> peekByteOff ptr 72 
            <*> peekByteOff ptr 80
            <*> peekByteOff ptr 88
            <*> peekByteOff ptr 96
            <*> peekByteOff ptr 104 
            <*> peekByteOff ptr 108
            <*> peekByteOff ptr 112 
            <*> peekByteOff ptr 116
            <*> peekByteOff ptr 120
            <*> peekByteOff ptr 124
            <*> peekByteOff ptr 128
            <*> peekByteOff ptr 136
            <*> peekByteOff ptr 144
            <*> peekByteOff ptr 148
        poke ptr (SCNotification 
                ptrHwndFrom
                idFrom
                code                
                position           
                ch
                modifiers
                modificationType
                ptrText
                length
                linesAdded
                message
                wParam
                lParam
                line
                foldLevelNow
                foldLevelPrev
                margin
                listType
                x
                y
                token
                annotationLinesAdded 
                updated
                listCompletionMethod) = do             
-- 0 8 16 24 32 36 40 48 56 64 72 80 88 96 104 108 112 116 120 124 128 136 144 148            
            pokeByteOff ptr 0     ptrHwndFrom
            pokeByteOff ptr 8     idFrom
            pokeByteOff ptr 16    code                
            pokeByteOff ptr 24    position           
            pokeByteOff ptr 32    ch
            pokeByteOff ptr 36    modifiers
            pokeByteOff ptr 40    modificationType
            pokeByteOff ptr 48    ptrText
            pokeByteOff ptr 56    length
            pokeByteOff ptr 64    linesAdded
            pokeByteOff ptr 72    message
            pokeByteOff ptr 80    wParam
            pokeByteOff ptr 88    lParam
            pokeByteOff ptr 96    line
            pokeByteOff ptr 104   foldLevelNow
            pokeByteOff ptr 108   foldLevelPrev
            pokeByteOff ptr 112   margin
            pokeByteOff ptr 116   listType
            pokeByteOff ptr 120   x
            pokeByteOff ptr 124   y
            pokeByteOff ptr 128   token
            pokeByteOff ptr 136   annotationLinesAdded 
            pokeByteOff ptr 144   updated
            pokeByteOff ptr 148   listCompletionMethod                 
           
scnNotifyGetPosition :: SCNotification -> Int64
scnNotifyGetPosition (SCNotification _ _ _ x _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = x           

scnNotifyGetWParam :: SCNotification -> Word64
scnNotifyGetWParam (SCNotification _ _ _ _ _ _ _ _ _ _ _ x _ _ _ _ _ _ _ _ _ _ _ _) = x           

scnNotifyGetListCompletionMethod :: SCNotification -> Int32
scnNotifyGetListCompletionMethod (SCNotification _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ x) = x           
            
main = start mainGUI

data Drag = Drag { downAt :: Point, at :: Point }

mainGUI :: IO ()
mainGUI = do 

    f <- frame [] 
    p <- panel  f []
    
    set f [ text:= "HeyHo", bgcolor := white]
           
    b  <- button f [text := "Test xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"]   
    set b [on command := cmdTest b]

    -- panel fills frame client area
    set p [ bgcolor := red]

--    timesThreePtr <- createTimesThreePtr timesThree
--    c_SetFnPtr timesThreePtr
--    n <- c_UseFnPtr 3
   
--    set f [ layout := minsize (sz 400 400) $ fill $ widget p]
--    windowSetSize f (Rect 0 0 500 500) wxSIZE_USE_EXISTING 
    set f [ size := (Size 500 500)]
    
    scn <- peek c_GetFnPtr
   
     
--    hwnd <- windowGetHandle p 
--    n <- c_fnTheDll hwnd

--    c_HookWindow

    set b [ text := "return " ++ showHex (scnNotifyGetWParam scn) "" ]
    repaint b

{-
    hwnd <- windowGetHandle p   
    hinstance <- loadLibrary "D:\\_Rick's\\haskell\\HeyHo\\SciLexer64.dll"
    hwnd1 <- createWindow (mkClassName "Scintilla") "Source" (wS_CHILD + wS_VSCROLL + wS_HSCROLL + wS_CLIPCHILDREN) 
        (Just 0)  (Just 0)  (Just 500)  (Just 500) (Just hwnd) Nothing hinstance winClose                    
    showWindow hwnd1 sW_SHOW
        
   
    set p [on resize := panelResize p hwnd1]
-}

--    freeHaskellFunPtr timesThreePtr
    
    return ()
    
    
    
panelResize :: Panel() -> HWND -> IO ()
panelResize p hwnd = do
    (Size x y) <- get p size
    c_SetWindowPos hwnd hWND_TOP 0 0 x y sWP_NOMOVE
    return ()
    
cmdTest :: Button () -> IO ()
cmdTest b = do
    set b [ text := "Clicked! "]
--    c_UnhookWindow
    return ()

    
winClose :: HWND ->  WindowMessage -> WPARAM -> LPARAM -> IO LRESULT
winClose _ _ _ _ = do 
    return (0)
        
    
    
    