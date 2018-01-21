
module Main where
import Graphics.UI.WX
import Graphics.UI.WXCore
import Control.Concurrent.MVar
import Data.Maybe (isNothing)
import Data.List (find)
import Numeric (showHex)
import Data.String.Combinators (punctuate)
import Data.Char (toLower)
import System.IO
import Control.Concurrent.STM



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
                            project     :: TProject }           -- List of open source files
                

type MSession = MVar Session


type TProject = TVar Project

data Project = Project { files :: [SourceFile] }



instance Show ScnEditor where
    show (ScnEditor (HWND p) (HWND e) me) = "{ScnEditor} Parent HWND: " ++ (showHex p "h") ++ ", Editor HWND: " ++ (showHex e "h") ++ ", Event Handler: ??" 
 
instance Show SourceFile where
    show (SourceFile p e mfp ic) = "{SourceFile} Panel: ??, " ++ show (e) ++ "), File: " ++ show (mfp) ++ ", Clean: " ++ show (ic)
    
sessionToString :: Session -> IO String
sessionToString (Session mf am nb tpr) = do
    let s1 = "{Session} Main: ??, AuiMgr: ??, Notebook ??, "
    s2 <- projectToString tpr
    return (s1 ++ s2)
    
projectToString :: TProject -> IO String
projectToString tpr = do
    (Project fs) <- atomically $ readTVar tpr
    let s = concat $ punctuate ", " $ map show fs
    return ("Files : " ++ s)
 
sf = (SourceFile (Panel' ()) (ScnEditor (HWND 0) (HWND 0) Nothing) Nothing True)    
fs = [sf]
pr = (Project [sf])
  
 
main = start mainGUI

mainGUI :: IO ()
mainGUI = do 
  
    -- main window
    f <- frame []    
    
    p <- panel f []
     

    
    ss <- newSession 
    addSourceFile ss "some file 1.xxx"
   
    addSourceFile ss "some file 2.xxx"
    addSourceFile ss "some file 1.xxx"
    addSourceFile ss "some file 3.xxx"
    addSourceFile ss "some file 2.xxx"
    addSourceFile ss "some file 4.xxx"

     
    s <- sessionToString ss
    
    putStr (show s ++ "\n")
    
    set f [ text := "Tryout", size := (Size 1300 800)]

    return ()
 

 
updateProject :: TProject -> (Project -> Project) -> IO (Project)
updateProject tpr f = atomically (do 
                        pr <- readTVar tpr
                        let pr' = f pr
                        writeTVar tpr pr'
                        return (pr))

readProject :: TProject -> IO Project
readProject = atomically . readTVar 
                        
newSession :: IO Session
newSession = do
    tpr <- atomically $ newTVar pr
    return (Session (Frame' ()) (AuiManager' ()) (AuiNotebook' ()) tpr)

addSourceFile :: Session -> String -> IO ()
addSourceFile ss@(Session mf am nb tpr) fp = do

    (Project fns) <- readProject tpr
 
    case fns of 
        -- assign file to initial editor window that is opened by the app
        -- provided it is not dirty
        [sf@(SourceFile _ _ Nothing True)] ->  do 
        
            -- set 1st slot
            updateProject tpr (\_ -> (Project [sourceSetFileName sf fp]))
            setSourceFileFocus ss fp
            return ()          
                                              
        otherwise -> do 
            if (isSourceFileInList fp fns) then do
                
                -- if already in file list then just switch focus to editor
                setSourceFileFocus ss fp
                return ()
                
             else do
             
                -- new file so add to list, create window and set focus
                sf' <- openSourceFileEditor ss fp
                updateProject tpr (\(Project fns') -> (Project (sf':fns')))
                setSourceFileFocus ss fp                        
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

setSourceFileFocus :: Session -> String -> IO ()
setSourceFileFocus ss fp = return ()

openSourceFileEditor :: Session -> String -> IO (SourceFile)
openSourceFileEditor ss fp = return (SourceFile (Panel' ()) (ScnEditor (HWND 0) (HWND 0) Nothing) (Just fp) True) 

findAndUpdate1 :: (a -> Bool) -> [a] -> a -> [a]
findAndUpdate1 _ [] _ = []
findAndUpdate1 f (x:xs) x' = (if f x then x' else x) : findAndUpdate1 f xs x'

findAndUpdate2 :: (a -> Maybe a) -> [a] -> [a]
findAndUpdate2 _ [] = []
findAndUpdate2 f (x:xs) = 
    (case f x of
        Just x' -> x' 
        Nothing -> x) : findAndUpdate2 f xs
        
ifFoundThenIO :: (a -> Bool) -> [a] -> (IO ()) -> IO ()
ifFoundThenIO _ [] _ =  return ()
ifFoundThenIO p (x:xs) io = do if p x then io else ifFoundThenIO p xs io

ifFoundThenElseIO :: (a -> Bool) -> [a] -> (IO ()) -> (IO ()) -> IO ()
ifFoundThenElseIO _ [] _ io2 =  io2
ifFoundThenElseIO p (x:xs) io1 io2 = do if p x then io1 else ifFoundThenElseIO p xs io1 io2



  
