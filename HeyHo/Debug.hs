
module Debug
(
    debugOut
) where

import Foreign.Ptr (FunPtr, Ptr, minusPtr, nullPtr)
import Numeric (showHex)
import Graphics.UI.WXCore
import Data.Word (Word64)

import Scintilla

debugOut :: ScnEditor -> String -> IO ()
debugOut e s = do
    scnSetReadOnly e False
    scnAppendLine e s
    scnSetReadOnly e True
