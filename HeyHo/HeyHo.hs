
module Main where

-- library imports
import Graphics.UI.WX
import Graphics.UI.WXCore
import qualified Data.ByteString.Char8 as BS (pack, readFile, writeFile, ByteString)

import Data.String.Combinators (punctuate)
import Control.Concurrent.STM

import Data.List (find)
import Data.Char (toLower)
import System.FilePath.Windows (takeFileName)


-- project imports
import Scintilla
import ScintillaConstants
import Misc

      
main = start mainGUI

mainGUI :: IO ()
mainGUI = do 
  
    -- main window
    mf <- frame []    
    set mf [ text := "HeyHo", size := (Size 1300 800)]  
     
    -- AUI manager and child windows
    ss <- setUpMainWindow mf
    
     -- create statusbar field
    statusBar' <- statusField   [text := "Welcome to wxHaskell"]

    -- set the statusbar and menubar
    set mf [ statusBar := [statusBar']]

    set mf [on closing := onClosing ss]
        
    return ()
   
----------------------------------------------------------
-- Session data and ToString functions
----------------------------------------------------------

data Session = Session {    mainFrame   :: Frame (),            -- Main window
                            auiMgr      :: AuiManager (),       -- Application level AUI manager
                            editorNB    :: AuiNotebook (),      -- Notebook of source file editors
                            project     :: TProject,            -- Project data (mutable)
                            menus       :: Menus }
                                                        
 
-- project data is mutable
type TProject = TVar Project

data Project = Project { files :: [SourceFile] }

-- Session data
data SourceFile = SourceFile {  edPanel     :: Panel (),        -- The panel added to the AuiNotebookManager
                                editor      :: ScnEditor,       -- The Scintilla editor, child window of panel
                                filePath    :: Maybe String,    -- Source file path, Nothing = file name not set yet
                                isClean     :: Bool }           -- False = file needs saving

data Menus = Menus {    menuFileOpen    :: MenuItem (), 
                        menuFileSave    :: MenuItem (),
                        menuFileSaveAs  :: MenuItem (),
                        menuFileSaveAll :: MenuItem () }

sourceFileToString :: SourceFile -> IO String
sourceFileToString (SourceFile p e mfp ic) = do
    ps <- panelToString p    
    return (
        "{SourceFile} Panel: " ++  ps ++
        ", (" ++ show (e) ++ "), " ++ 
        ", File: " ++ show (mfp) ++ 
        ", Clean: " ++ show (ic))
 
sessionToString :: Session -> IO String
sessionToString (Session mf _ _ tpr _) = do
    fs <- frameToString mf
    prs <- projectToString tpr
    return ("{Session} Main: " ++ fs ++ prs)

projectToString :: TProject -> IO String
projectToString tpr = do
    (Project fs) <- atomically $ readTVar tpr
    ss <- mapM sourceFileToString fs
    let s = concat $ punctuate ", " ss
    return ("Files : " ++ s)
    
updateProject :: Session -> (Project -> Project) -> IO (Project)
updateProject (Session _ _ _ tpr _) f = atomically (do 
                        pr <- readTVar tpr
                        let pr' = f pr
                        writeTVar tpr pr'
                        return (pr))

readProject :: Session -> IO Project
readProject (Session _ _ _ tpr _) = atomically $ readTVar tpr

------------------------------------------------------------    
-- Setup main window, AUI manager its child windows and the menus
------------------------------------------------------------    
   
setUpMainWindow :: Frame () -> IO (Session)    
setUpMainWindow mf = do 

    am <- auiManagerCreate mf wxAUI_MGR_DEFAULT
      
    -- add dockable tree
    tree <- createTree mf
    api <- auiPaneInfoCreateDefault
    auiPaneInfoCaption api "Tree Control"
    auiPaneInfoLeft api
    auiPaneInfoCloseButton api True
    
    auiManagerAddPaneByPaneInfo am tree api
    
    -- add dockable grid
    grid <- createGrid mf
    api <- auiPaneInfoCreateDefault
    auiPaneInfoCaption api "Grid Control"
    auiPaneInfoBottom api
    auiPaneInfoCloseButton api True
    
    auiManagerAddPaneByPaneInfo am grid api
    
    -- add editor notebook
    enb <- createEditorNoteBook mf
   
    api <- auiPaneInfoCreateDefault
    auiPaneInfoCaption api "Editor"
    auiPaneInfoCentre api
    auiPaneInfoCloseButton api True
    auiPaneInfoMaximizeButton api True

    auiManagerAddPaneByPaneInfo am enb api
   
    -- add output pane
    nb <- createNoteBook mf
    
    api <- auiPaneInfoCreateDefault
    auiPaneInfoCaption api "Output"
    auiPaneInfoBottom api
    auiPaneInfoCloseButton api True

    auiManagerAddPaneByPaneInfo am nb api

    auiManagerUpdate am
 
    -- create the mutable project data
    tpr <- atomically $ newTVar (Project [])

    -- setup the menus
    menus@(Menus menuFileOpen menuFileSave menuFileSaveAs menuFileSaveAll) <- setupMenus mf 

    -- create the session data
    let ss = (Session mf am enb tpr menus)
    
    -- add blank file to editor
    editorAddNewFile ss
    
    -- setup menu handlers
    set menuFileOpen [on command := onFileOpen ss]
    set menuFileSaveAs [on command := onFileSaveAs ss]
    
    return (ss)
    
------------------------------------------------------------    
-- Setup menus
------------------------------------------------------------    

setupMenus :: Frame () -> IO (Menus)
setupMenus mf  = do

    -- file menu  
    menuFile        <- menuPane             [text := "&File"]
    menuFileOpen    <- menuItem menuFile    [text := "Open ...\tCtrl-O", help := "Opens a file"]
                                             
    menuFileSave    <- menuItem menuFile    [text := "Save\tCtrl-S", help := "Saves a file", enabled := False]
    menuFileSaveAs  <- menuItem menuFile    [text := "Save As ...", help := "Saves a file", enabled := True]
    menuFileSaveAll <- menuItem menuFile    [text := "Save All\tCtrl-Shift-S", help := "Saves all files", enabled := False]
                                             
    menuAppendSeparator menuFile
                             
    menuQuit  <- menuQuit menuFile [help := "Quit the demo", on command := close mf]

    menuEdit         <- menuPane            [text := "&Edit"]
    menuEditUndo     <- menuItem menuEdit   [text := "Undo\tCtrl-Z"]
    
    menuBuild        <- menuPane            [text := "Build"]
    menuBuildCompile <- menuItem menuBuild  [text := "Compile\tCtrl-F7",       help := "Compiles current source file"]
    menuBuildBuild   <- menuItem menuBuild  [text := "Build\tF7",              help := "Build the project"]
    menuBuildReBuild <- menuItem menuBuild  [text := "Rebuild\tCtrl-Alt-F7",   help := "Rebuild the project"]
    menuBuildClean   <- menuItem menuBuild  [text := "Clean",                  help := "Clean the project"]
          
    -- create Help menu
    menuHelp'        <- menuHelp []
    menuHelpAbout    <- menuAbout menuHelp' [help := "About HeyHo", on command := infoDialog mf "About HeyHo" "mmmmm !"]
      
    set mf [ menuBar := [menuFile, menuEdit, menuBuild, menuHelp']]

    return (Menus menuFileOpen menuFileSave menuFileSaveAs menuFileSaveAll)
    
------------------------------------------------------------    
-- Event handlers
------------------------------------------------------------    
  
closeFile :: SourceFile -> IO (SourceFile)
closeFile (SourceFile p e _ _) = do
    e' <- scnDisableEvents e
    return (SourceFile p e' Nothing True)

onClosing :: Session -> IO ()
onClosing ss@(Session mf am nb _ _) = do
    (Project fs) <- readProject ss
    mapM (\f -> closeFile f) fs
    auiManagerUnInit am
    windowDestroy mf
    return ()

scnCallback :: Session -> SCNotification -> IO ()
scnCallback ss sn = do 

    case (scnNotifyGetCode sn) of
    
    -- If the constants are used rather than the real values the compiler
    -- gives some nonsense about overlapping cases !! compiler bug
    
        2002 -> do -- sCN_SAVEPOINTREACHED
            updateFileCleanStatus ss sn True
            updateSaveMenus ss
            return ()

        2003 -> do -- sCN_SAVEPOINTLEFT
            updateFileCleanStatus ss sn False
            updateSaveMenus ss
            return ()
          
        otherwise -> return ()
          
updateFileCleanStatus :: Session -> SCNotification -> Bool -> IO ()
updateFileCleanStatus ss sn ic' = do 

    -- update the source file clean flag
    updateProject ss (\(Project fs) -> (Project (findAndUpdate2 updateIfSource fs)))
    return ()
    
    where updateIfSource = (\(SourceFile p e fp ic) -> 
                                if (scnCompareHwnd e sn) then (Just (SourceFile p e fp ic'))
                                else Nothing )

updateSaveMenus :: Session -> IO ()   
updateSaveMenus ss@(Session _ _ nb _ (Menus _ mS mSAs mSAll)) = do

    (Project fs) <- readProject ss

    -- enable save all menu if any files are dirty
    if (any (\(SourceFile _ _ _ ic) -> not ic) fs) then
        set mSAll [enabled := True]
    else
        set mSAll [enabled := False]
            
    -- get hwnd of the panel associated with the selected notebook tab
    ix <- auiNotebookGetSelection nb
    p <- auiNotebookGetPage nb ix
    hp <- windowGetHandle p
    
    hs <- mapM (\(SourceFile p' _ _ ic) -> 
                    do 
                    hp' <- windowGetHandle p' 
                    return (hp',ic)) fs

    -- set file save enabled if the current tab selection is for a dirty file
    case find (\(hp',ic) -> (comparePtrs hp hp') && not ic) hs of
        Just _  -> set mS [enabled := True]
        Nothing -> set mS [enabled := False]
    
    -- enable save as if there are any tabs
    pc <- auiNotebookGetPageCount nb
    if pc == 0 then 
        set mSAs [enabled := False]
    else
        set mSAs [enabled := True]

-- File Open
onFileOpen :: Session -> IO ()
onFileOpen ss@(Session mf _ _ _ _) = do
                                                   -- wxFD_OPEN wxFD_FILE_MUST_EXIST
    fd <- fileDialogCreate mf "Open file" "." "" "*.hs" (Point 100 100) 0x11
    ans <- dialogShowModal fd
    if ans == wxID_OK
    then do
        fp <- fileDialogGetPath fd
        fileOpen ss fp
        return ()
    else
        return ()

-- File Save As  
onFileSaveAs :: Session -> IO ()
onFileSaveAs ss@(Session mf _ _ _ _) = do
                                                      -- wxFD_SAVE wxFD_OVERWRITE_PROMPT
    fd <- fileDialogCreate mf "Save file as" "." "" "*.hs" (Point 100 100) 0x6
    rs <- dialogShowModal fd    
    if rs == wxID_OK
    then do    
        fn <- fileDialogGetPath fd
--        fileSave ss fn
        return ()
    else 
        return ()
        
-----------------------------------------------------------------
-- Session management
-----------------------------------------------------------------

sessionGetMainFrame :: Session -> Frame ()
sessionGetMainFrame (Session mf _ _ _ _) = mf

fileOpen :: Session -> String -> IO ()
fileOpen ss@(Session _ _ nb tpr _) fp = do

    (Project fs) <- readProject ss
   
    case fs of 
        -- assign file to initial editor window that is opened by the app
        -- provided it is not dirty
        [sf@(SourceFile _ _ Nothing True)] ->  do 
        
            -- set 1st slot
            let sf' = sourceSetFileName sf fp
            writeSourceFileEditor sf'
            auiNotebookSetPageText nb 0 (takeFileName fp)
            updateProject ss (\_ -> (Project [sf']))
            return ()          
                                              
        otherwise -> do
        
            if (isSourceFileInList fp fs) then do
                
                -- if already in file list then just switch focus to editor
                setSourceFileFocus ss fp
                return ()
                
             else do
             
                -- new file so add to list, create window and set focus
                sf' <- openSourceFileEditor ss fp
                writeSourceFileEditor sf'
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
setSourceFileFocus ss@(Session _ _ nb _ _) fp = do
    sf <- getSourceFile ss fp
    case (sf) of
        Just (SourceFile p _ _ _) -> do
            ix <- auiNotebookGetPageIndex nb p
            auiNotebookSetSelection nb ix 
            return ()
        Nothing -> return ()

getSourceFile :: Session -> String -> IO (Maybe SourceFile)
getSourceFile ss fp = do 
    (Project fs) <- readProject ss
    let sf = find (\sf -> sourceFilePathIs sf (Just fp)) fs
    return (sf)

writeSourceFileEditor :: SourceFile -> IO ()
writeSourceFileEditor (SourceFile _ e (Just fp) _) = do
    text <- BS.readFile fp
    scnSetText e text
    scnSetSavePoint e
    return ()

openSourceFileEditor :: Session -> String -> IO (SourceFile)
openSourceFileEditor ss@(Session _ _ nb _ _) fp = do

    -- create panel with scintilla editor inside
    p <- panel nb []
    hwnd <- windowGetHandle p
    scn <- scnCreateEditor hwnd
    scnConfigureHaskell scn
    scn' <- scnEnableEvents scn (scnCallback ss)

    -- add panel to notebook
    auiNotebookAddPage nb p (takeFileName fp) False 0
    ta <- auiSimpleTabArtCreate
    auiNotebookSetArtProvider nb ta
    
    -- set focus to new page
    ix <- auiNotebookGetPageIndex nb p
    auiNotebookSetSelection nb ix  
  
    -- add source file to project
    let sf = (SourceFile p scn' (Just fp) True)
    updateProject ss (\(Project fs) -> (Project (sf:fs)))
          
    return (sf) 
  
{-
fileSave :: ScnEditor -> String -> IO ()
fileSave scn fn = do
    bs <- scnGetAllText scn
    BS.writeFile fn bs
    return ()
-}        
------------------------------------------------------------    
-- Create the source file editor notebook
------------------------------------------------------------    

createEditorNoteBook :: Frame f -> IO (AuiNotebook ())
createEditorNoteBook f = do

    -- create the notebook
    nb <- auiNotebookCreate f idAny (Point 0 0) (Size 0 0) (wxCLIP_CHILDREN + wxAUI_NB_TOP + wxAUI_NB_CLOSE_ON_ACTIVE_TAB)
    return (nb)
  
editorAddNewFile :: Session -> IO (SourceFile)  
editorAddNewFile ss@(Session _ _ nb _ _) = do
    
    -- create panel with scintilla editor inside
    p <- panel nb []
    hwnd <- windowGetHandle p
    scn <- scnCreateEditor hwnd
    scnConfigureHaskell scn

    -- add panel to notebook
    auiNotebookAddPage nb p "..." False 0
    ta <- auiSimpleTabArtCreate
    auiNotebookSetArtProvider nb ta

    -- enable events
    scn' <- scnEnableEvents scn (scnCallback ss)
    scnSetSavePoint scn'

    -- update mutable project
    let sf = (SourceFile p scn' Nothing True)
    updateProject ss (\(Project sfs) -> Project (sf:sfs)) 

    return (sf)
    
------------------------------------------------------------    
-- Notebook
------------------------------------------------------------    

createNoteBook :: Frame () -> IO (AuiNotebook ())
createNoteBook f = do
    nb <- auiNotebookCreate f idAny (Point 0 0) (Size 0 0) (wxCLIP_CHILDREN + wxAUI_NB_BOTTOM)
    set nb [] 
    p <- panel nb []
    hwnd <- windowGetHandle p
    scn <- scnCreateEditor hwnd
    auiNotebookAddPage nb p "Example" False 0
    ta <- auiSimpleTabArtCreate
    auiNotebookSetArtProvider nb ta
    return (nb)

------------------------------------------------------------    
-- Tree Control
------------------------------------------------------------    
    
createTree :: Frame () ->  IO (TreeCtrl ())
createTree f = do      
    tree <- treeCtrl f [size := (Size 100 100)] 
    root <- treeCtrlAddRoot tree "root" (-1) (-1) objectNull     
    _    <- treeCtrlAppendItem tree root "item1" (-1) (-1) objectNull
    _    <- treeCtrlAppendItem tree root "item2" (-1) (-1) objectNull
    _    <- treeCtrlAppendItem tree root "item3" (-1) (-1) objectNull
    treeCtrlExpand tree root
    cs <- treeCtrlGetChildren tree root
    return (tree)
    
------------------------------------------------------------    
-- Grid Control
------------------------------------------------------------    

createGrid :: Frame () -> IO (Grid ())
createGrid f = do
    -- grids
    g <- gridCtrl f []
    gridSetGridLineColour g (colorSystem Color3DFace)
    gridSetCellHighlightColour g black
    appendColumns g (head names)
    appendRows    g (map show [1..length (tail names)])
    mapM_ (setRow g) (zip [0..] (tail names))
    gridAutoSize g  
    return (g)
    
gridCtrl :: Window a -> [Prop (Grid ())] -> IO (Grid ())
gridCtrl parent_ props_
  = feed2 props_ 0 $
    initialWindow $ \id_ rect' -> \props' flags ->
    do g <- gridCreate parent_ id_ rect' flags
       gridCreateGrid g 0 0 0
       set g props'
       return g

appendColumns :: Grid a -> [String] -> IO ()
appendColumns _g []
  = return ()
appendColumns g labels
  = do n <- gridGetNumberCols g
       _ <- gridAppendCols g (length labels) True
       mapM_ (\(i, label_) -> gridSetColLabelValue g i label_) (zip [n..] labels)

appendRows :: Grid a -> [String] -> IO ()
appendRows _g []
  = return ()
appendRows g labels
  = do n <- gridGetNumberRows g
       _ <- gridAppendRows g (length labels) True
       mapM_ (\(i, label_) -> gridSetRowLabelValue g i label_) (zip [n..] labels)

setRow :: Grid a -> (Int, [String]) -> IO ()
setRow g (row_, values)
  = mapM_ (\(col,value_) -> gridSetCellValue g row_ col value_) (zip [0..] values)

names :: [[String]]
names
  = [["First Name", "Last Name"]
    ,["Daan","Leijen"],["Arjan","van IJzendoorn"]
    ,["Martijn","Schrage"],["Andres","Loh"]]
    
    

