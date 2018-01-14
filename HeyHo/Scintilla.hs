module Scintilla
(   SCNotification,
    ScnEditor,
    scnCreateEditor,
    scnNotifyGetCode,
    scnNotifyGetPosition,
    scnNotifyGetWParam,
    scnNotifyGetListCompletionMethod,
    scnGetHwnd,
    scnEnableEvents,
    scnDisableEvents,
    scnSetText,
    scnGetAllText,
    scnGetTextLen,
) where 
    
import Control.Applicative ((<$>), (<*>))
import Foreign.Storable
import Data.Word
import Data.Int

import System.Win32.Types
import Graphics.Win32.GDI.Types

import Graphics.UI.WXCore
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.String (CString)
import qualified Data.ByteString as BS
import Data.ByteString.Internal
import Data.ByteString.Unsafe (unsafeUseAsCString)
 
-----------------------
-- Windows API calls --
-----------------------

type HHOOK = Word64

-- imports from ScintillaProxy.dll
foreign import ccall safe "ScnNewEditor"     c_ScnNewEditor :: HWND -> IO (HWND)      
foreign import ccall safe "ScnDestroyEditor" c_ScnDestroyEditor :: HWND -> IO ()      
foreign import ccall safe "ScnEnableEvents"  c_ScnEnableEvents :: HWND -> FunPtr (Ptr (SCNotification) -> IO ()) -> IO (Int32)
foreign import ccall safe "ScnDisableEvents" c_ScnDisableEvents :: HWND -> IO ()      

-- direct call to Scintilla, different aliases simplify conversion to WPARAM and LPARAM types 
foreign import ccall safe "ScnSendEditor"    c_ScnSendEditorII :: HWND -> Word32 -> Word64 -> Int64 -> IO (Int64)
foreign import ccall safe "ScnSendEditor"    c_ScnSendEditorIS :: HWND -> Word32 -> Word64 -> CString -> IO (Int64)

-- callback wrapper
foreign import ccall safe "wrapper" createCallback ::
    (Ptr (SCNotification) -> IO ()) -> IO (FunPtr (Ptr (SCNotification) -> IO ()))

--------------------------------------------------------------
-- data types
--------------------------------------------------------------
      
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
        events      :: Maybe (SCNotification -> IO ())
    }  
 
-----------------------------------------------------------

-- Create the Scintilla editor window
-- parent = HWND of parent window
scnCreateEditor :: HWND -> IO (ScnEditor)
scnCreateEditor parent = do
    hwnd <- c_ScnNewEditor parent
    v <- varCreate (ScnEditor parent hwnd Nothing)
    scn <- varGet v
    return (scn)
    
scnEnableEvents :: ScnEditor -> (SCNotification -> IO ()) -> IO (ScnEditor)
scnEnableEvents (ScnEditor p c _) f = do
    let s = (ScnEditor p c (Just f))
    cb <- createCallback $ scnCallback s
    c_ScnEnableEvents c cb    
    return (s)

scnDisableEvents :: ScnEditor -> IO (ScnEditor)
scnDisableEvents (ScnEditor p c _) = do
    let s = (ScnEditor p c Nothing)
    c_ScnDisableEvents c    
    return (s)

-- the callback from ScintillaProxy dll    
scnCallback :: ScnEditor -> Ptr (SCNotification) -> IO ()
scnCallback (ScnEditor _ _ Nothing) _ = return ()
scnCallback (ScnEditor _ _ (Just f)) p = do
    n <- peek p
    f n
    return () 

scnGetHwnd :: ScnEditor -> HWND
scnGetHwnd (ScnEditor _ h _) = h

scnNotifyGetCode :: SCNotification -> Word32
scnNotifyGetCode (SCNotification _ _ x _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = x           

scnNotifyGetPosition :: SCNotification -> Int64
scnNotifyGetPosition (SCNotification _ _ _ x _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = x           

scnNotifyGetWParam :: SCNotification -> Word64
scnNotifyGetWParam (SCNotification _ _ _ _ _ _ _ _ _ _ _ x _ _ _ _ _ _ _ _ _ _ _ _) = x           

scnNotifyGetListCompletionMethod :: SCNotification -> Int32
scnNotifyGetListCompletionMethod (SCNotification _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ x) = x           

------------------------------------------------------------    
-- Scintilla commands
------------------------------------------------------------    

scnSetText :: ScnEditor -> ByteString -> IO ()
scnSetText e bs = do
    unsafeUseAsCString bs (\cs -> do c_ScnSendEditorIS (scnGetHwnd e) 2181 0 cs)
    return ()

scnGetAllText :: ScnEditor -> IO (ByteString)
scnGetAllText e = do
    let h = scnGetHwnd e                -- scintilla editor hwnd
    len <- c_ScnSendEditorII h 2006 0 0 -- get length
    let bs = (BS.replicate (fromIntegral (len+1) :: Int) 0)   -- allocate buffer including terminating null
    unsafeUseAsCString bs (\cs -> do c_ScnSendEditorIS h 2182 (fromIntegral (len+1) :: Word64) cs) -- get text
    return (bs)
    
scnGetTextLen :: ScnEditor -> IO (Int64)
scnGetTextLen e = c_ScnSendEditorII (scnGetHwnd e) 2006 0 0

