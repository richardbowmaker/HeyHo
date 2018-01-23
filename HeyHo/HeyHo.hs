
module Main where

-- library imports
import Graphics.UI.WX
import Graphics.UI.WXCore
import qualified Data.ByteString.Char8 as BS (pack, readFile, writeFile, ByteString)


import System.FilePath.Windows (takeFileName)
import Data.List (find)
import Data.Word (Word64)



-- project imports
import Debug
import Misc
import Scintilla
import ScintillaConstants
import Session

      
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
    
    -- add floating debug window
    dp <- panel mf [size := (Size 800 800)]
    hwnd <- windowGetHandle dp
    scn <- scnCreateEditor hwnd
    scnConfigureHaskell scn
    scnSetReadOnly scn True
    
    api <- auiPaneInfoCreateDefault
    auiPaneInfoCaption api "Debug"
    auiPaneInfoFloat api
    auiPaneInfoCloseButton api True

    auiManagerAddPaneByPaneInfo am dp api   

    auiManagerUpdate am
 
    -- setup the menus
    menus <- setupMenus mf 

    -- create the session data
    ss <- createSession mf am enb (createProject []) menus scn
    
    -- add blank file to editor
    editorAddNewFile ss
    
    -- setup menu handlers
    set (sessionMenuListGet ss "FileOpen")   [on command := onFileOpen   ss]
    set (sessionMenuListGet ss "FileSaveAs") [on command := onFileSaveAs ss]
    set (sessionMenuListGet ss "FileClose")  [on command := onFileClose  ss]
    
    set enb [on auiNotebookOnPageCloseEvent   := editorPageClose ss]
    set enb [on auiNotebookOnPageChangedEvent := editorPageChanged ss]
   
    return (ss)
    
------------------------------------------------------------    
-- Setup menus
------------------------------------------------------------    

setupMenus :: Frame () -> IO (SessionMenuList)
setupMenus mf  = do

    -- file menu  
    menuFile            <- menuPane             [text := "&File"]
    menuFileOpen        <- menuItem menuFile    [text := "Open ...\tCtrl-O",        help := "Opens a file"]
    menuFileNew         <- menuItem menuFile    [text := "New\tCtrl-N",             help := "Starts a new file"]
    menuFileClose       <- menuItem menuFile    [text := "Close",                   help := "Closes the current file"]
    menuFileCloseAll    <- menuItem menuFile    [text := "Close All",               help := "Closes all files"]                                             
    menuFileSave        <- menuItem menuFile    [text := "Save\tCtrl-S",            help := "Saves a file", enabled := False]
    menuFileSaveAs      <- menuItem menuFile    [text := "Save As ...",             help := "Saves a file"]
    menuFileSaveAll     <- menuItem menuFile    [text := "Save All\tCtrl-Shift-S",  help := "Saves all files", enabled := False]
                                             
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

    -- create lookup list of menus for session data
    ml <- sessionMenuListCreate [   ("FileOpen",        menuFileOpen), 
                                    ("FileSave",        menuFileSave), 
                                    ("FileNew",         menuFileNew), 
                                    ("FileClose",       menuFileClose), 
                                    ("FileCloseAll",    menuFileCloseAll), 
                                    ("FileSaveAs",      menuFileSaveAs), 
                                    ("FileSaveAll",     menuFileSaveAll)] 
    
    return (ml)
    
------------------------------------------------------------    
-- Event handlers
------------------------------------------------------------    
  
closeFile :: SourceFile -> IO (SourceFile)
closeFile sf = do
    e' <- scnDisableEvents $ sourceFileGetEditor sf
    sf <- createSourceFile (sourceFileGetPanel sf) e' Nothing True
    return (sf)

onClosing :: Session -> IO ()
onClosing ss = do
    let mf = sessionGetMainFrame ss
    let am = sessionGetAuiManager ss
    fs  <- sessionReadSourceFiles ss
    mapM (\f -> closeFile f) fs
    auiManagerUnInit am
    windowDestroy mf
    return ()
    
onFileClose :: Session -> IO ()
onFileClose ss = do
    sf <- enbGetSelectedSourceFile ss
    let e = sourceFileGetEditor sf
    scnClose e
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
    updateProject ss (\pr -> (createProject (findAndUpdate2 updateIfSource (projectGetFiles pr))))
    return ()
    
    where updateIfSource = (\sf -> 
                                if (scnCompareHwnd (sourceFileGetEditor sf) sn) then 
                                (Just (sourceFileSetIsClean sf ic'))
                                else Nothing )

-- updates the enabled state of the Save, SaveAs and SaveAll menus                                
updateSaveMenus :: Session -> IO ()   
updateSaveMenus ss = do

    fs <- sessionReadSourceFiles ss
    ic <- enbSelectedSourceFileIsClean ss
   
    set (sessionMenuListGet ss "FileSave")      [enabled := not ic]
    set (sessionMenuListGet ss "FileSaveAs")    [enabled := length fs > 0]
    set (sessionMenuListGet ss "FileClose")     [enabled := length fs > 0]
    set (sessionMenuListGet ss "FileCloseAll")  [enabled := length fs > 0]
    set (sessionMenuListGet ss "FileSaveAll")   [enabled := anyDirtyFiles fs]
     
    return ()
    
-- File Open
onFileOpen :: Session -> IO ()
onFileOpen ss = do
    
    let mf = sessionGetMainFrame ss                     -- wxFD_OPEN wxFD_FILE_MUST_EXIST
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
onFileSaveAs ss = do

    let mf = sessionGetMainFrame ss                   -- wxFD_SAVE wxFD_OVERWRITE_PROMPT
    fd <- fileDialogCreate mf "Save file as" "." "" "*.hs" (Point 100 100) 0x6
    rs <- dialogShowModal fd    
    if rs == wxID_OK
    then do    
        fn <- fileDialogGetPath fd
--        fileSave ss fn
        return ()
    else 
        return ()
        
        
editorPageClose :: Session -> EventAuiNotebook -> IO ()
editorPageClose ss ev@(AuiNotebookPageClose _ (WindowSelection  _ mpw)) = do
    case mpw of
    
        Nothing ->  do -- shouldn't happen
                 
                updateSaveMenus ss
                return ()
                
        Just (PageWindow _ w) -> do
        
                h <- windowGetHandle w
                let hw = ptrToWord64 h
                updateProject ss $ removeSourceFileFromProject hw
                updateSaveMenus ss
                return ()               
                
    where   
            removeSourceFileFromProject hw pr = createProject (findAndRemove (isFileWindow hw) (projectGetFiles pr))
            isFileWindow hw sf = hw == (sourceFileGetPanelHwnd sf)

editorPageChanged :: Session -> EventAuiNotebook -> IO ()
editorPageChanged ss ev@(AuiNotebookPageChanged _ _) = do
    updateSaveMenus ss
    return ()    
        
-----------------------------------------------------------------
-- Session management
-----------------------------------------------------------------

fileOpen :: Session -> String -> IO ()
fileOpen ss fp = do

    fs <- sessionReadSourceFiles ss
    
    if sessionIsOpeningState fs then do
    
        -- set 1st slot
        let sf' = sourceFileSetFilePath (head fs) fp
        let nb = sessionGetNotebook ss
        writeSourceFileEditor sf'
        auiNotebookSetPageText nb 0 (takeFileName fp)
        updateProject ss (\_ -> createProject [sf'])
        return ()          
    
    else do
    
        if (isSourceFileInList fp fs) then do
            
            -- if already in file list then just switch focus to editor
            setSourceFileFocus ss fp
            return ()
            
         else do
         
            -- new file so add to list, create window and set focus
            sf' <- openSourceFileEditor ss fp
            writeSourceFileEditor sf'
            return ()          
    
setSourceFileFocus :: Session -> String -> IO ()
setSourceFileFocus ss fp = do
    msf <- getSourceFile ss fp
    case (msf) of
        Just sf -> do
            let nb = sessionGetNotebook ss
            ix <- auiNotebookGetPageIndex nb $ sourceFileGetPanel sf
            auiNotebookSetSelection nb ix 
            return ()
        Nothing -> return ()

getSourceFile :: Session -> String -> IO (Maybe SourceFile)
getSourceFile ss fp = do 
    fs <- sessionReadSourceFiles ss
    let sf = find (\sf -> sourceFilePathIs sf (Just fp)) fs
    return (sf)

writeSourceFileEditor :: SourceFile -> IO ()
writeSourceFileEditor sf = do
    let mfp = sourceFileGetFilePath sf
    case mfp of
        Just fp -> do
            text <- BS.readFile fp
            let e = sourceFileGetEditor sf
            scnSetText e text
            scnSetSavePoint e
            return ()
        Nothing -> return () -- error

openSourceFileEditor :: Session -> String -> IO (SourceFile)
openSourceFileEditor ss fp = do

    let nb = sessionGetNotebook ss

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
    sf <- createSourceFile p scn' (Just fp) True
    updateProject ss (\pr -> projectSetFiles pr (sf:(projectGetFiles pr)))
          
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

createEditorNoteBook :: Frame mf -> IO (AuiNotebook ())
createEditorNoteBook mf = do

    -- create the notebook
    nb <- auiNotebookCreate mf idAny (Point 0 0) (Size 0 0) (wxCLIP_CHILDREN + wxAUI_NB_TOP + wxAUI_NB_CLOSE_ON_ACTIVE_TAB)
    return (nb)
  
editorAddNewFile :: Session -> IO (SourceFile)  
editorAddNewFile ss = do
    
    let nb = sessionGetNotebook ss

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
    sf <- createSourceFile p scn' Nothing True
    updateProject ss (\pr -> projectSetFiles pr (sf:(projectGetFiles pr)))

    return (sf)
    
------------------------------------------------------------    
-- Editor notebook helpers
------------------------------------------------------------    
    
-- returns the HWND of the child panel of the currently selected notebook page
enbGetSelectedTabHwnd :: Session -> IO Word64
enbGetSelectedTabHwnd ss = do
    let nb = sessionGetNotebook ss
    ix <- auiNotebookGetSelection nb
    p <- auiNotebookGetPage nb ix
    hp <- windowGetHandle p
    return (ptrToWord64 hp)
 
-- returns the source file for the currently selected tab
enbGetSelectedSourceFile :: Session -> IO SourceFile
enbGetSelectedSourceFile ss = do  
    fs <- sessionReadSourceFiles ss
    hp <- enbGetSelectedTabHwnd ss   
    case (find (\sf -> sourceFileMatchesHwnd sf hp) fs) of
        Just sf -> 
            return (sf)
        Nothing -> do 
            -- should not occur, like this to simplfy calling code
            debugOut ss "*** error: enbGetSelectedSourceFile no source file for current tab"
            return (head fs) 
                  
-- returns true of the source file the currently selected tab is clean 
enbSelectedSourceFileIsClean :: Session -> IO Bool
enbSelectedSourceFileIsClean ss = do  
    fs <- sessionReadSourceFiles ss
    if (length fs > 0) then do
        sf <- enbGetSelectedSourceFile ss
        let e = sourceFileGetEditor sf
        ic <- scnIsClean e
        return (ic)
    else do return (True)

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
    
    

