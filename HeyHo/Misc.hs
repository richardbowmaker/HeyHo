
module Misc
(
    ptrToString,
    frameToString,
    windowToString,
    panelToString
) where

import Foreign.Ptr (FunPtr, Ptr, minusPtr, nullPtr)
import Numeric (showHex)
import Graphics.UI.WXCore

ptrToString :: Ptr a -> String
ptrToString p = "0x0" ++ (showHex (minusPtr p nullPtr) "")
              
panelToString :: Panel () -> IO String
panelToString p = do
    h <- windowGetHandle p
    return ("Window HWND: " ++ (showHex (minusPtr h nullPtr) ""))

frameToString :: Frame () -> IO String
frameToString f = do
    h <- windowGetHandle f
    t <- frameGetTitle f
    return ("Frame HWND: " ++ (showHex (minusPtr h nullPtr) "") ++ " Title: " ++ t)

windowToString :: Window () -> IO String
windowToString w = do
    h <- windowGetHandle w
    return ("Window HWND: " ++ (showHex (minusPtr h nullPtr) ""))
    
