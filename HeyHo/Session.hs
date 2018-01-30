module Session 
(
    Session,
    Project,
    SourceFile,
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
    sessionGetStatus,
    sessionGetDebug,
    SessionNameMenuPair,
    SessionMenuList,
    sessionGetMenus,
    sessionMenuListNew,
    sessionMenuListCreate,
    sessionMenuListAdd,
    sessionMenuListGet,
    createSourceFile,
    sourceFileSetFilePath,
    sfPanel,
    sfPanelHwnd,
    sfEditor,
    sfFilePath,
    sourceFileGetFilePathString,
    sourceFileMatchesHwnd,
    projectGetFiles,
    projectSetFiles,
    createProject,
    sessionReadSourceFiles,
    sessionIsOpeningState,
    isSourceFileInList,
    sourceFilePathIs,
    updateSourceFile,
    sourceFileIsSame
) where


import Graphics.UI.WX
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
                            menus       :: SessionMenuList,
                            status      :: StatusField,
                            debug       :: ScnEditor}
                                                        
 -- project data is mutable
type TProject = TVar Project

data Project = Project { files :: [SourceFile] }

-- Session data
data SourceFile = SourceFile {  sfPanel     :: Panel (),        -- The panel added to the AuiNotebookManager
                                sfPanelHwnd :: Word64,          -- HWND of panel
                                sfEditor    :: ScnEditor,       -- The Scintilla editor, child window of panel
                                sfFilePath    :: Maybe String}    -- Source file path, Nothing = file name not set yet

type SessionNameMenuPair = (String, MenuItem ())                               
type SessionMenuList = [SessionNameMenuPair]

                        
sessionGetMainFrame :: Session -> Frame ()
sessionGetMainFrame (Session x _ _ _ _ _ _) = x
 
sessionGetAuiManager :: Session -> AuiManager ()
sessionGetAuiManager (Session _ x _ _ _ _ _) = x
 
sessionGetNotebook :: Session -> AuiNotebook ()
sessionGetNotebook (Session _ _ x _ _ _ _) = x

sessionGetProject :: Session -> TProject
sessionGetProject (Session _ _ _ x _ _ _) = x

sessionGetMenus :: Session -> SessionMenuList
sessionGetMenus (Session _ _ _ _ x _ _) = x

sessionGetStatus :: Session -> StatusField
sessionGetStatus (Session _ _ _ _ _ x _) = x

sessionGetDebug :: Session -> ScnEditor
sessionGetDebug (Session _ _ _ _ _ _ x) = x

createSession :: Frame () -> AuiManager () -> AuiNotebook () -> Project -> SessionMenuList -> StatusField -> ScnEditor -> IO (Session)
createSession mf am nb pr ms sf db = do 
    tpr <- atomically $ newTVar (createProject [])
    return (Session mf am nb tpr ms sf db)

--------------------    

-- creates a new menu item lookup list
-- a dummy entry is provided for failed lookups to simplfy client calls to menuListGet 
sessionMenuListNew :: IO SessionMenuList 
sessionMenuListNew = do
    mi <- menuItemCreate
    return ([("", mi)])

sessionMenuListCreate :: [SessionNameMenuPair] -> IO SessionMenuList
sessionMenuListCreate nmps = do
    ml <- sessionMenuListNew
    return (sessionMenuListAdd nmps ml)

sessionMenuListAdd :: [SessionNameMenuPair] -> SessionMenuList -> SessionMenuList
sessionMenuListAdd nmps ml = ml ++ nmps

sessionMenuListGet :: Session -> String -> MenuItem ()
sessionMenuListGet ss s = 
    case (lookup s ml) of
        Just mi -> mi
        Nothing -> snd $ last ml
    where ml = sessionGetMenus ss
        
--------------------

sourceFileGetPanel :: SourceFile -> Panel()                        
sourceFileGetPanel (SourceFile x _ _ _ ) = x                        
                      
sourceFileGetPanelHwnd :: SourceFile -> Word64                        
sourceFileGetPanelHwnd (SourceFile _ x _ _ ) = x                        

sourceFileGetEditor :: SourceFile -> ScnEditor                        
sourceFileGetEditor (SourceFile _ _ x _) = x                        

sourceFileGetEditorHwnd :: SourceFile -> Word64                        
sourceFileGetEditorHwnd (SourceFile _ _ e _) = ptrToWord64 $ scnGetHwnd e                        

sourceFileGetFilePath :: SourceFile -> Maybe String                        
sourceFileGetFilePath (SourceFile _ _ _ x) = x                       

sourceFileGetFilePathString :: SourceFile -> String                        
sourceFileGetFilePathString sf = 
    case (sourceFileGetFilePath sf) of
        Just fp -> fp
        Nothing -> ""

sourceFileSetFilePath :: SourceFile -> String -> SourceFile
sourceFileSetFilePath (SourceFile p hp e _) fp = (SourceFile p hp e (Just fp))

sourceFileMatchesHwnd :: SourceFile -> Word64 -> Bool
sourceFileMatchesHwnd sf h = h == (sourceFileGetPanelHwnd sf)

createSourceFile :: Panel() -> ScnEditor -> Maybe String -> IO SourceFile
createSourceFile p e mfp = do
    hp <- windowGetHandle p
    return (SourceFile p (ptrToWord64 hp) e mfp)
 
isSourceFileInList :: String -> [SourceFile] -> Bool
isSourceFileInList fp fs = 
    case find (\sf -> sourceFilePathIs sf (Just fp)) fs of
        Just _ -> True
        Nothing -> False
 
sourceFilePathIs :: SourceFile -> Maybe String -> Bool
sourceFilePathIs (SourceFile _ _ _ mfp1) mfp2 = fmap (map toLower) mfp1 == fmap (map toLower) mfp2

projectGetFiles :: Project -> [SourceFile]
projectGetFiles (Project x) = x

projectSetFiles :: Project -> [SourceFile] -> Project
projectSetFiles (Project _) x = (Project x)

sessionIsOpeningState :: [SourceFile] -> IO Bool
sessionIsOpeningState [sf@(SourceFile _ _ e Nothing)] = do
    b <- scnIsClean e
    return (b)
sessionIsOpeningState _ = return (False)

sessionReadSourceFiles :: Session -> IO [SourceFile]
sessionReadSourceFiles ss = do 
    pr <- readProject ss 
    return (projectGetFiles pr)

createProject :: [SourceFile] -> Project
createProject sfs = (Project sfs)

sourceFileToString :: SourceFile -> String
sourceFileToString (SourceFile _ hp e mfp) = 
        "{SourceFile} Panel: 0x" ++ (showHex hp "" ) ++
        ", (" ++ show (e) ++ "), " ++ 
        ", File: " ++ show (mfp)
        
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
    
updateProject :: Session -> (Project -> Project) -> IO Project
updateProject ss f = atomically (do
                        let tpr = sessionGetProject ss
                        pr <- readTVar $ tpr
                        let pr' = f pr
                        writeTVar tpr pr'
                        return (pr))
                        

-- updates the mutable project data to include the modified source file                        
updateSourceFile :: Session -> SourceFile -> IO Project                        
updateSourceFile ss sf' = do
    pr' <- updateProject ss (\pr -> updateFile (projectGetFiles pr) h)
    return (pr')
    
    where
        h = sourceFileGetPanelHwnd sf'
        updateFile sfs h = createProject(map (\sf -> if (sourceFileMatchesHwnd sf h) then sf' else sf) sfs)
    
readProject :: Session -> IO Project
readProject ss = atomically $ readTVar $ sessionGetProject ss

sourceFileIsSame :: SourceFile -> SourceFile -> Bool
sourceFileIsSame sf1 sf2 = (sourceFileGetPanelHwnd sf1) == (sourceFileGetPanelHwnd sf2)



