
module Main where
import Graphics.UI.WX
import Graphics.UI.WXCore
import Control.Concurrent.MVar
import Data.Maybe (isNothing)
import Data.List (find)
import Numeric (showHex)
import Data.String.Combinators (punctuate)
import Data.Char (toLower)



import qualified Data.ByteString.Char8 as BS (pack, readFile, writeFile, ByteString)

data SCNotification = SCNotification Int
data HWND = HWND Int
data AuiManager' a = AuiManager' a
data AuiNotebook' a = AuiNotebook' a

data ScnEditor = ScnEditor 
    { 
        hParent     :: HWND,
        hScnWnd     :: HWND,         
        events      :: Maybe (SCNotification -> IO ())
    } 

data Panel' a = Panel' a
data Frame' a = Frame' a

-- Session data
data SourceFile = SourceFile {  edPanel     :: Panel' (),       -- The panel added to the AuiNotebookManager
                                editor      :: ScnEditor,       -- The Scintilla editor, child window of panel
                                filePath    :: Maybe String,    -- Source file path, Nothing = file name not set yet
                                isClean     :: Bool }           -- False = file needs saving
                                                        
data Session = Session {    mainFrame   :: Frame' (),           -- Main window
                            auiMgr      :: AuiManager' (),      -- Application level AUI manager
                            editorNB    :: AuiNotebook' (),     -- Notebook of source file editors
                            openFiles   :: [SourceFile] }       -- List of open source files
                

type MSession = MVar Session

instance Show ScnEditor where
    show (ScnEditor (HWND p) (HWND e) me) = "{ScnEditor} Parent HWND: " ++ (showHex p "h") ++ ", Editor HWND: " ++ (showHex e "h") ++ ", Event Handler: ??" 
 
instance Show SourceFile where
    show (SourceFile p e mfp ic) = "{SourceFile} Panel: ??, " ++ show (e) ++ "), File: " ++ show (mfp) ++ ", Clean: " ++ show (ic)
    
instance Show Session where
    show (Session mf am nb []) = "{Session} Main: ??, AuiMgr: ??, Notebook ??, No files"
    show (Session mf am nb fs) = "{Session} Main: ??, AuiMgr: ??, Notebook ??\n    " ++ (concat $ punctuate "\n    " $ map show fs) ++ "\n"
    
 
fs = [(SourceFile (Panel' ()) (ScnEditor (HWND 0) (HWND 0) Nothing) Nothing True)]
ns = (Session (Frame' ()) (AuiManager' ()) (AuiNotebook' ()) fs)    
    
 
 
main = start mainGUI

mainGUI :: IO ()
mainGUI = do 
  
    -- main window
    f <- frame []    
    set f [ text := "Tryout", size := (Size 1300 800)]
    
    p <- panel f []
     
    mv <- newEmptyMVar
    
    putMVar mv createSession
    
    
        
    return ()
 
 
createSession :: Frame () -> Panel () -> Session
createSession f p = (Session (Frame' ()) (AuiManager' ()) (AuiNotebook' ()) [(SourceFile (Panel' ()) scn Nothing True)])
    where scn = (ScnEditor (HWND 0) (HWND 0) Nothing)

addSourceFile ::  Session -> String -> Session
addSourceFile (Session f am nb fns) fp = (Session f am nb fns2)
    
    where    -- assign file to opening empty edit window, if not already used
            (fns1, c) = findAndUpdate 
                (\sf -> sourceFilePathIs sf Nothing) 
                (\(SourceFile p s _ _) -> (SourceFile p s (Just fp) True)) 
                fns

            -- if file is not in the list add it
            fns2 = case find (\sf -> sourceFilePathIs sf (Just fp)) fns1 of
                Just _ -> fns1
                Nothing -> (newSourceFile nb fp) : fns1 

sourceFilePathIs :: SourceFile -> Maybe String -> Bool
sourceFilePathIs (SourceFile _ _ mfp1 _) mfp2 = fmap (map toLower) mfp1 == fmap (map toLower) mfp2
                
newSourceFile :: AuiNotebook' () -> String -> SourceFile
newSourceFile nb fn = (SourceFile (Panel' ()) (ScnEditor (HWND 0) (HWND 0) Nothing) (Just fn) True)
            
-- finds elements in a list using a predicate and updates them using a function
-- (a -> Bool)  = predicate
-- (a -> a)     = update function
-- [a]          = the list
-- ([a], Int)   = (updated list, no. of items updated)
findAndUpdate :: (a -> Bool) -> (a -> a) -> [a] -> ([a], Int)
findAndUpdate _ _ [] = ([], 0)
findAndUpdate fp fu (x:xs) = 
    if fp x then ((fu x):xs', c+1) else (x:xs', c) 
    where (xs', c) = findAndUpdate fp fu xs  

findAndReplace :: (a -> Bool) -> [a] -> a -> ([a], Int)
findAndReplace fp xs x = findAndUpdate fp (\_ -> x) xs

        


    