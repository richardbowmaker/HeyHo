
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
    

sf = (SourceFile (Panel' ()) (ScnEditor (HWND 0) (HWND 0) Nothing) Nothing True)    
fs = [sf]
ns = (Session (Frame' ()) (AuiManager' ()) (AuiNotebook' ()) fs)    
  

 
main = start mainGUI

mainGUI :: IO ()
mainGUI = do 
  
    -- main window
    f <- frame []    
    
    p <- panel f []
     
    mv <- newEmptyMVar
    
    createSession mv f p
    addSourceFile mv "some file 1.xxx"
   
    addSourceFile mv "some file 2.xxx"
    addSourceFile mv "some file 1.xxx"
    addSourceFile mv "some file 3.xxx"
    addSourceFile mv "some file 2.xxx"
    addSourceFile mv "some file 4.xxx"

     
    ss <- takeMVar mv
    
    putStr (show ss ++ "\n")
    
    set f [ text := "Tryout", size := (Size 1300 800)]

    return ()
 

createSession :: MSession -> Frame () -> Panel () -> IO ()
createSession mv f p = do
    let ss = (Session (Frame' ()) (AuiManager' ()) (AuiNotebook' ()) [(SourceFile (Panel' ()) scn Nothing True)])
    putMVar mv ss
    return ()
    where scn = (ScnEditor (HWND 0) (HWND 0) Nothing)

addSourceFile :: MSession -> String -> IO ()
addSourceFile mv fp = do

    ss@(Session mf am nb fns) <- takeMVar mv
 
    case fns of 
        -- assign file to initial editor window that is opened by the app
        -- provided it is not dirty
        [sf@(SourceFile _ _ Nothing True)] ->  do 
        
            -- set 1st slot
            let ss = (Session mf am nb [sourceSetFileName sf fp])
            putMVar mv ss
            setSourceFileFocus mv fp
            return ()          
                                              
        otherwise -> do 
            if (isSourceFileInList fp fns) then do
                
                -- if already in file list then just switch focus to editor
                setSourceFileFocus mv fp
                putMVar mv ss
                return ()
                
             else do
             
                -- new file so add to list, create window and set focus
                sf' <- openSourceFileEditor mv fp
                let ss = (Session mf am nb (sf':fns))
                putMVar mv ss
                setSourceFileFocus mv fp                        
                return ()
    
sourceSetFileName :: SourceFile -> String -> SourceFile
sourceSetFileName (SourceFile p ed _ ic) fp = (SourceFile p ed (Just fp) ic)
  
isSourceFileInList :: String -> [SourceFile] -> Bool
isSourceFileInList fp fs = 
    case find (\sf -> sourceFilePathIs sf (Just fp)) fs of
        Just _ -> True
        Nothing -> False
 
sourceFilePathIs :: SourceFile -> Maybe String -> Bool
sourceFilePathIs (SourceFile _ _ mfp1 _) mfp2 = fmap (map toLower) mfp1 == fmap (map toLower) mfp2

setSourceFileFocus :: MSession -> String -> IO ()
setSourceFileFocus mv fp = return ()

openSourceFileEditor :: MSession -> String -> IO (SourceFile)
openSourceFileEditor mv fp = return (SourceFile (Panel' ()) (ScnEditor (HWND 0) (HWND 0) Nothing) (Just fp) True) 

