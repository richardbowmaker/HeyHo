
module Debug
(
    debugOut
) where

import Foreign.Ptr (FunPtr, Ptr, minusPtr, nullPtr)
import Numeric (showHex)
import Graphics.UI.WXCore
import Data.Word (Word64)

import Scintilla
import Session

debugOut :: Session -> String -> IO ()
debugOut ss s = do
    let e = sessionGetDebug ss
    scnSetReadOnly e False
    scnAppendLine e s
    scnSetReadOnly e True
    return ()
