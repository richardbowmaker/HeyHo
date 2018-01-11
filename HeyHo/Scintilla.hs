module Scintilla
(   SCNotification,
    ScnEditor,
    scnCreateEditor,
    scnNotifyGetPosition,
    scnNotifyGetWParam,
    scnNotifyGetListCompletionMethod,
    scnSetSizeFromParent,
    scnGetHwnd,
    scnEnableEvents
) where 
    
import Numeric
import Control.Applicative ((<$>), (<*>))
import Foreign.Storable
import Data.Word
import Data.Int

import System.Win32.DLL
import System.Win32.Types
import Graphics.Win32.Window
import Graphics.Win32.GDI.Types
import Graphics.Win32.Message
import Graphics.Win32.Misc

import Graphics.UI.WXCore
import Foreign.Marshal.Alloc
import Foreign.Ptr

-----------------------
-- Windows API calls --
-----------------------

type HHOOK = Word64

-- HHOOK WINAPI SetWindowsHookEx(
--   _In_ int       idHook,
--   _In_ HOOKPROC  lpfn,
--   _In_ HINSTANCE hMod,
--   _In_ DWORD     dwThreadId
-- );
foreign import ccall unsafe "SetWindowsHookExW"
    c_SetWindowsHookEx :: Int32 -> FunPtr (Int32 -> WPARAM -> Ptr (CWPSTRUCT) -> IO (LRESULT)) -> HINSTANCE -> DWORD -> IO (HHOOK)        
           
-- LRESULT WINAPI CallNextHookEx(
--   _In_opt_ HHOOK  hhk,
--   _In_     int    nCode,
--   _In_     WPARAM wParam,
--   _In_     LPARAM lParam
-- );
foreign import ccall unsafe "CallNextHookEx"
    c_CallNextHookEx :: HHOOK -> Int32 -> WPARAM -> LPARAM -> IO (LRESULT)
    
-- BOOL WINAPI UnhookWindowsHookEx(
--   _In_ HHOOK hhk
-- );  
foreign import ccall unsafe "UnhookWindowsHookEx"
    c_UnhookWindowsHookEx :: HHOOK-> IO (BOOL)
  

-- LRESULT CALLBACK CallWndProc(
--   _In_ int    nCode,
--   _In_ WPARAM wParam,
--   _In_ LPARAM lParam
-- );
--foreign export ccall scnCallWndProc :: Int32 -> WPARAM -> LPARAM -> IO (LRESULT)
foreign import ccall safe "wrapper" scnCreateCallWndProc :: 
    (Int32 -> WPARAM -> Ptr (CWPSTRUCT) -> IO(LRESULT)) -> IO (FunPtr (Int32 -> WPARAM -> Ptr (CWPSTRUCT) -> IO(LRESULT)))
 
-- BOOL WINAPI SetWindowPos(
--   _In_     HWND hWnd,
--   _In_opt_ HWND hWndInsertAfter,
--   _In_     int  X,
--   _In_     int  Y,
--   _In_     int  cx,
--   _In_     int  cy,
--   _In_     UINT uFlags
-- );
foreign import ccall unsafe "SetWindowPos"
    c_SetWindowPos :: HWND -> HWND -> Int32 -> Int32 -> Int32 -> Int32 -> Word32 -> IO (Word32)
                  
-- BOOL WINAPI GetWindowRect(
--   _In_  HWND   hWnd,
--   _Out_ LPRECT lpRect
-- );    
foreign import ccall unsafe "GetWindowRect"
    c_GetWindowRec :: HWND -> Ptr (ScnRect) -> IO (Word32) 

-- DWORD WINAPI GetCurrentThreadId(void)
foreign import ccall unsafe "GetCurrentThreadId"
    c_GetCurrentThreadId :: IO (Word32) 
     
--------------------------------------------------------------
-- data types
--------------------------------------------------------------

data CWPSTRUCT = CWPSTRUCT
    {
        cwpLParam   :: Word64,
        cwpWParam   :: Word64,
        cwpMessage  :: Word32,
        cwpHWnd     :: Word64
    }
                   
instance Storable CWPSTRUCT where
    alignment _ = 8
    sizeOf _    = 32
    peek ptr    = CWPSTRUCT
        <$> peekByteOff ptr 0
        <*> peekByteOff ptr 8
        <*> peekByteOff ptr 16 
        <*> peekByteOff ptr 24
    poke ptr (CWPSTRUCT cwpLParam cwpWParam cwpMessage cwpHWnd) = do
        pokeByteOff ptr 0     cwpLParam
        pokeByteOff ptr 8     cwpWParam
        pokeByteOff ptr 16    cwpMessage                
        pokeByteOff ptr 24    cwpHWnd 
       
data ScnRect = ScnRect
    {
        recLeft   :: Int32,
        recTop    :: Int32,
        recRight  :: Int32,
        recBottom :: Int32
    }
                   
instance Storable ScnRect where
    alignment _ = 8
    sizeOf _    = 16
    peek ptr    = ScnRect
        <$> peekByteOff ptr 0
        <*> peekByteOff ptr 4
        <*> peekByteOff ptr 8 
        <*> peekByteOff ptr 12
    poke ptr (ScnRect recLeft recTop recRight recBottom) = do
        pokeByteOff ptr 0     recLeft
        pokeByteOff ptr 4     recTop
        pokeByteOff ptr 18    recRight                
        pokeByteOff ptr 12    recBottom 
        
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
           

data ScnEditor = ScnEditor 
    { 
        hParent     :: HWND,
        hScnWnd     :: HWND, 
        hDll        :: HINSTANCE, 
        hHook       :: HHOOK,
        events      :: Maybe (SCNotification -> IO ())
    }  
 
-----------------------------------------------------------

-- Create the Scintilla editor window
-- parent = HWND of parent window
scnCreateEditor :: HWND -> IO (ScnEditor)
scnCreateEditor parent = do
    hDll <- loadLibrary "SciLexer64.dll"
    scnHwnd <- createWindow (mkClassName "Scintilla") "Source" (wS_CHILD + wS_VSCROLL + wS_HSCROLL + wS_CLIPCHILDREN) 
        (Just 0)  (Just 0)  (Just 500)  (Just 500) (Just parent) Nothing hDll scnWinClose                    
    showWindow scnHwnd sW_SHOW     
    v <- varCreate (ScnEditor parent scnHwnd hDll 0 Nothing)
    scn <- varGet v
    scnSetSizeFromParent scn
    return (scn)
  
scnWinClose :: HWND ->  WindowMessage -> WPARAM -> LPARAM -> IO LRESULT
scnWinClose _ _ _ _ = do 
    return (0)
 
scnGetHwnd :: ScnEditor -> HWND
scnGetHwnd (ScnEditor _ scnHwnd _ _ _) = scnHwnd
  
scnSetSizeFromParent :: ScnEditor -> IO ()
scnSetSizeFromParent scn = do
    alloca (scnSetSize scn)
    return ()

scnSetSize :: ScnEditor -> Ptr (ScnRect) -> IO ()
scnSetSize (ScnEditor p c _ _ _) rect = do
    c_GetWindowRec p rect
    (ScnRect l t r b) <- peek rect
    c_SetWindowPos c hWND_TOP 0 0 (r-l) (b-t) sWP_NOMOVE
    return ()

scnNotifyGetPosition :: SCNotification -> Int64
scnNotifyGetPosition (SCNotification _ _ _ x _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = x           

scnNotifyGetWParam :: SCNotification -> Word64
scnNotifyGetWParam (SCNotification _ _ _ _ _ _ _ _ _ _ _ x _ _ _ _ _ _ _ _ _ _ _ _) = x           

scnNotifyGetListCompletionMethod :: SCNotification -> Int32
scnNotifyGetListCompletionMethod (SCNotification _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ x) = x           

scnEnableEvents :: ScnEditor -> Maybe (SCNotification -> IO ()) -> IO (ScnEditor)
scnEnableEvents scn@(ScnEditor p c lib hk f) f' = do
    callback <- scnCreateCallWndProc (scnCallWndProc scn)
    tid <- c_GetCurrentThreadId
    c_SetWindowsHookEx 4 callback nullPtr tid
    return (ScnEditor p c lib hk f')
   
-- Windows Hook callback
scnCallWndProc :: ScnEditor -> Int32 -> WPARAM -> Ptr (CWPSTRUCT) -> IO (LRESULT) 
scnCallWndProc scn code _ lparam = do
    if code < 0
    then
        return (0)
    else
        do
        (CWPSTRUCT lp wp m h) <- peek lparam
        if m = wM_NOTIFY
        then
            
        else        
        return (0)

   
--    f' scn


 

 
