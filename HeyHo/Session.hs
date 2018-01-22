module Session 
(
    Session,
    Project,
    SourceFile,
    Menus,
    sourceFileToString,
    sessionToString,
    projectToString,
    updateProject,
    readProject,
    createSession,
    sessionGetMainFrame,
    sessionGetAuiManager, 
    sessionGetNotebook,
    sessionGetProject,
    sessionGetDebug,
    createMenus,
    sessionGetMenus,
    sessionGetFileOpen,
    sessionGetFileSave,
    sessionGetFileSaveAs,
    sessionGetFileSaveAll,
    createSourceFile,
    sourceFileGetPanel,
    sourceFileGetPanelHwnd,
    sourceFileGetEditor,
    sourceFileGetHwndEditor,
    sourceFileGetFilePath,
    sourceFileGetIsClean,
    sourceFileSetFilePath,
    projectGetFiles,
    projectSetFiles,
    createProject,
    sourceFileSetIsClean,
    sessionReadSourceFiles,
    sessionIsOpeningState,
    anyDirtyFiles,
    isSourceFileInList,
    sourceFilePathIs
) where


import Graphics.UI.WXCore
import Control.Concurrent.STM
import Data.String.Combinators (punctuate)
import Data.List (find)
import Data.Char (toLower)
import Data.Word (Word64)
import Numeric (showHex)



import Scintilla
import Misc

----------------------------------------------------------
-- Session data and ToString functions
----------------------------------------------------------

data Session = Session {    mainFrame   :: Frame (),            -- Main window
                            auiMgr      :: AuiManager (),       -- Application level AUI manager
                            editorNB    :: AuiNotebook (),      -- Notebook of source file editors
                            project     :: TProject,            -- Project data (mutable)
                            menus       :: Menus,
                            debug       :: ScnEditor}
                                                        
 -- project data is mutable
type TProject = TVar Project

data Project = Project { files :: [SourceFile] }

-- Session data
data SourceFile = SourceFile {  edPanel     :: Panel (),        -- The panel added to the AuiNotebookManager
                                hPanel      :: Word64,          -- HWND of panel
                                editor      :: ScnEditor,       -- The Scintilla editor, child window of panel
                                filePath    :: Maybe String,    -- Source file path, Nothing = file name not set yet
                                isClean     :: Bool }           -- False = file needs saving

data Menus = Menus {    menuFileOpen    :: MenuItem (), 
                        menuFileSave    :: MenuItem (),
                        menuFileSaveAs  :: MenuItem (),
                        menuFileSaveAll :: MenuItem () }
                        
sessionGetMainFrame :: Session -> Frame ()
sessionGetMainFrame (Session x _ _ _ _ _) = x
 
sessionGetAuiManager :: Session -> AuiManager ()
sessionGetAuiManager (Session _ x _ _ _ _) = x
 
sessionGetNotebook :: Session -> AuiNotebook ()
sessionGetNotebook (Session _ _ x _ _ _) = x

sessionGetProject :: Session -> TProject
sessionGetProject (Session _ _ _ x _ _) = x

sessionGetMenus :: Session -> Menus
sessionGetMenus (Session _ _ _ _ x _) = x

sessionGetDebug :: Session -> ScnEditor
sessionGetDebug (Session _ _ _ _ _ x) = x

createSession :: Frame () -> AuiManager () -> AuiNotebook () -> Project -> Menus -> ScnEditor -> IO (Session)
createSession mf am nb pr ms db = do 
    tpr <- atomically $ newTVar (createProject [])
    return (Session mf am nb tpr ms db)

sessionGetFileOpen :: Session -> MenuItem ()                        
sessionGetFileOpen (Session _ _ _ _ (Menus x _ _ _) _) = x                        

sessionGetFileSave :: Session -> MenuItem ()                        
sessionGetFileSave (Session _ _ _ _ (Menus _ x _ _) _) = x                        

sessionGetFileSaveAs :: Session -> MenuItem ()                        
sessionGetFileSaveAs (Session _ _ _ _ (Menus _ _ x _) _) = x                        

sessionGetFileSaveAll :: Session -> MenuItem ()                        
sessionGetFileSaveAll (Session _ _ _ _ (Menus _ _ _ x) _) = x                        

sourceFileGetPanel :: SourceFile -> Panel()                        
sourceFileGetPanel (SourceFile x _ _ _ _ ) = x                        
                        
sourceFileGetPanelHwnd :: SourceFile -> Word64                        
sourceFileGetPanelHwnd (SourceFile _ x _ _ _ ) = x                        

sourceFileGetEditor :: SourceFile -> ScnEditor                        
sourceFileGetEditor (SourceFile _ _ x _ _) = x                        

sourceFileGetHwndEditor :: SourceFile -> Word64                        
sourceFileGetHwndEditor (SourceFile _ _ e _ _) = ptrToWord64 $ scnGetHwnd e                        

sourceFileGetFilePath :: SourceFile -> Maybe String                        
sourceFileGetFilePath (SourceFile _ _ _ x _) = x                       

sourceFileGetIsClean :: SourceFile -> Bool                        
sourceFileGetIsClean (SourceFile _ _ _ _ x) = x                        

sourceFileSetIsClean :: SourceFile -> Bool -> SourceFile
sourceFileSetIsClean (SourceFile p hp e mfp _) ic = (SourceFile p hp e mfp ic)

sourceFileSetFilePath :: SourceFile -> String -> SourceFile
sourceFileSetFilePath (SourceFile p hp e _ ic) fp = (SourceFile p hp e (Just fp) ic)

createSourceFile :: Panel() -> ScnEditor -> Maybe String -> Bool -> IO SourceFile
createSourceFile p e mfp ic = do
    hp <- windowGetHandle p
    return (SourceFile p (ptrToWord64 hp) e mfp ic)
 
isSourceFileInList :: String -> [SourceFile] -> Bool
isSourceFileInList fp fs = 
    case find (\sf -> sourceFilePathIs sf (Just fp)) fs of
        Just _ -> True
        Nothing -> False
 
sourceFilePathIs :: SourceFile -> Maybe String -> Bool
sourceFilePathIs (SourceFile _ _ _ mfp1 _) mfp2 = fmap (map toLower) mfp1 == fmap (map toLower) mfp2

projectGetFiles :: Project -> [SourceFile]
projectGetFiles (Project x) = x

projectSetFiles :: Project -> [SourceFile] -> Project
projectSetFiles (Project _) x = (Project x)

sessionIsOpeningState :: [SourceFile] -> Bool
sessionIsOpeningState [sf@(SourceFile _ _ _ Nothing True)] = True
sessionIsOpeningState _ = False

sessionReadSourceFiles :: Session -> IO [SourceFile]
sessionReadSourceFiles ss = do 
    pr <- readProject ss 
    return (projectGetFiles pr)

createProject :: [SourceFile] -> Project
createProject sfs = (Project sfs)

anyDirtyFiles :: [SourceFile] -> Bool
anyDirtyFiles fs = any (\(SourceFile _ _ _ _ ic) -> not ic) fs

createMenus :: MenuItem () -> MenuItem () -> MenuItem () -> MenuItem () -> Menus
createMenus menuFileOpen menuFileSave menuFileSaveAs menuFileSaveAll = 
    (Menus menuFileOpen menuFileSave menuFileSaveAs menuFileSaveAll) 
 
sourceFileToString :: SourceFile -> String
sourceFileToString (SourceFile _ hp e mfp ic) = 
        "{SourceFile} Panel: 0x" ++ (showHex hp "" ) ++
        ", (" ++ show (e) ++ "), " ++ 
        ", File: " ++ show (mfp) ++ 
        ", Clean: " ++ show (ic)
        
sessionToString :: Session -> IO String
sessionToString ss = do
    fs <- frameToString $ sessionGetMainFrame ss
    prs <- projectToString $ sessionGetProject ss
    return ("{Session} Main: " ++ fs ++ prs)

projectToString :: TProject -> IO String
projectToString tpr = do
    (Project fs) <- atomically $ readTVar tpr
    let ss = map sourceFileToString fs
    let s = concat $ punctuate ", " ss
    return ("Files : " ++ s)
    
updateProject :: Session -> (Project -> Project) -> IO (Project)
updateProject ss f = atomically (do
                        let tpr = sessionGetProject ss
                        pr <- readTVar $ tpr
                        let pr' = f pr
                        writeTVar tpr pr'
                        return (pr))

readProject :: Session -> IO Project
readProject ss = atomically $ readTVar $ sessionGetProject ss





