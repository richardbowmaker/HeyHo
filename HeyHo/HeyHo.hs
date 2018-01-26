
module Main where

-- library imports
import Graphics.UI.WX
import Graphics.UI.WXCore
import qualified Data.ByteString.Char8 as BS (pack, readFile, writeFile, ByteString)


import System.FilePath.Windows (takeFileName)
import Data.List (find, findIndex)
import Data.Word (Word64)
import Numeric (showHex)


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
    set (sessionMenuListGet ss "FileOpen")     [on command := onFileOpen     ss]
    set (sessionMenuListGet ss "FileNew")      [on command := onFileNew      ss]
    set (sessionMenuListGet ss "FileSave")     [on command := onFileSave     ss]
    set (sessionMenuListGet ss "FileSaveAs")   [on command := onFileSaveAs   ss]
    set (sessionMenuListGet ss "FileSaveAll")  [on command := onFileSaveAll  ss]
    set (sessionMenuListGet ss "FileClose")    [on command := onFileClose    ss]
    set (sessionMenuListGet ss "FileCloseAll") [on command := onFileCloseAll ss]
    
    set enb [on auiNotebookOnPageCloseEvent   := onTabClose   ss]
    set enb [on auiNotebookOnPageChangedEvent := onTabChanged ss]
   
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

onClosing :: Session -> IO ()
onClosing ss = do
    let mf = sessionGetMainFrame ss
    let am = sessionGetAuiManager ss
    fileCloseAll ss
    auiManagerUnInit am
    windowDestroy mf
    return ()

onTabChanged :: Session -> EventAuiNotebook -> IO ()
onTabChanged ss ev@(AuiNotebookPageChanged _ _) = do
    updateSaveMenus ss
    return ()    

onTabClose :: Session -> EventAuiNotebook -> IO ()
onTabClose ss _ = do
    sf <- enbGetSelectedSourceFile ss
    closeEditor ss sf   
    updateSaveMenus ss      
    return ()
    
onFileClose :: Session -> IO ()
onFileClose ss = do
    sf <- enbGetSelectedSourceFile ss
    fileClose ss sf
    updateSaveMenus ss    
    return ()
  
onFileCloseAll :: Session -> IO ()
onFileCloseAll = fileCloseAll

onFileSave :: Session -> IO ()
onFileSave ss = do
    sf <- enbGetSelectedSourceFile ss
    fileSave ss sf
    return ()

onFileSaveAs :: Session -> IO ()
onFileSaveAs ss = do
    sf <- enbGetSelectedSourceFile ss
    fileSaveAs ss sf
    return ()
    
onFileSaveAll :: Session -> IO ()    
onFileSaveAll ss = do   
    fileSaveAll ss
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

onFileNew :: Session -> IO ()
onFileNew ss = do
    editorAddNewFile ss
    return ()
        
-------------------------------------------------

closeEditor :: Session -> SourceFile -> IO Bool
closeEditor ss sf = do

    let e = sourceFileGetEditor sf      
    ic <- scnIsClean e
    
    if ic then do
        
       -- save file
        b <- fileSave ss sf
        
        if b then do
        
            closeTab ss sf         
            return (True)

        -- veto close, don't know how to do this yet ??
        else return (False)         
        
    else do
    
        -- file is dirty so prompt the user if they want to save it
        enbSelectTab ss sf
        b <- confirmDialog (sessionGetMainFrame ss) 
                "Heyho" 
                ("Do you wish to save the file: ?\n" ++ (show $ sourceFileGetFilePathString sf))
                True
                
        if b then do
            
           -- save file
            b <- fileSave ss sf
            
            if b then do
            
                closeTab ss sf         
                return (True)

            -- veto close, don't know how to do this yet ??
            else return (False) 
    
        else do
            closeTab ss sf
            return (True)
          
closeTab :: Session -> SourceFile -> IO ()
closeTab ss sf = do
    
    -- close down scintilla editor
    let e = sourceFileGetEditor sf
    scnDisableEvents e
    scnClose e
    
    -- remove source file from project
    updateProject ss (\pr -> createProject (findAndRemove (sourceFileIsSame sf) (projectGetFiles pr)))
    
    return ()
    
fileCloseAll :: Session -> IO ()
fileCloseAll ss = do 
    sfs <- sessionReadSourceFiles ss  
    doWhileTrueIO (fileClose ss) sfs
    updateSaveMenus ss    
    return ()
       
fileClose :: Session -> SourceFile -> IO Bool
fileClose ss sf = do

    b <- closeEditor ss sf
    
    if b then do
    
        -- remove page from notebook
        mix <- enbGetTabIndex ss sf 
        case mix of
            Just ix -> do
                let nb = sessionGetNotebook ss        
                auiNotebookDeletePage nb ix  
                return (True)
            Nothing -> do
                debugOut ss "*** error: fileClose, no tab for source file"
                return (True)
        
    else return (False)
    
fileSaveAll :: Session -> IO (Bool)
fileSaveAll ss = do

        sfs <- sessionReadSourceFiles ss
        b <- doWhileTrueIO (fileSave ss) sfs
        return (b)

-- if file is dirty then writes it to file
-- if no filename has been set then file save as is called
-- returns false if user cancelled        
fileSave :: Session -> SourceFile -> IO Bool
fileSave ss sf = do

    let e = sourceFileGetEditor sf      
    ic <- scnIsClean e
    
    if (ic) then do
        return (True)
    else       
        case (sourceFileGetFilePath sf) of
            Just fp -> do
                writeSourceFile sf
                return (True)
            
            -- source file has no name, so prompt user for one
            Nothing -> do
                b <- fileSaveAs ss sf
                return (b)
                   
-- File Save As, returns False if user opted to cancel the save 
fileSaveAs :: Session -> SourceFile -> IO Bool
fileSaveAs ss sf = do
   
    -- ensure source file is displayed
    enbSelectTab ss sf
    
    -- prompt user for name to save to
    let mf = sessionGetMainFrame ss                   -- wxFD_SAVE wxFD_OVERWRITE_PROMPT
    fd <- fileDialogCreate mf "Save file as" "." "" "*.hs" (Point 100 100) 0x6
    rs <- dialogShowModal fd 
    
    case rs of
--        wxID_OK -> do
-- ?? don't know how to fix pattern match against a function    
        5100 -> do    
            fp <- fileDialogGetPath fd
            
            -- save new name to mutable project data
            let sf' = sourceFileSetFilePath sf fp
            updateSourceFile ss sf'
            writeSourceFile sf'
            
            -- update tab name
            enbSetTabText ss sf'
            return (True)
            
        --wxID_CANCEL -> do
        5101 -> do
            return (False)
            
        otherwise  -> do
            return (True)
           
-- writes file to disk and sets editor to clean
writeSourceFile :: SourceFile -> IO ()
writeSourceFile sf = do
    let e = sourceFileGetEditor sf
    case (sourceFileGetFilePath sf) of
        Just fp -> do
            bs <- scnGetAllText e
            BS.writeFile fp bs
            scnSetSavePoint e
            return ()
        
        Nothing -> do
            -- bug, shouldn't end up here
            return ()
            
-----------------------    

scnCallback :: Session -> SCNotification -> IO ()
scnCallback ss sn = do 

    case (scnNotifyGetCode sn) of
    
    -- If the constants are used rather than the real values the compiler
    -- gives some nonsense about overlapping cases !! compiler bug
    
        2002 -> do -- sCN_SAVEPOINTREACHED
            updateSaveMenus ss
            return ()

        2003 -> do -- sCN_SAVEPOINTLEFT
            updateSaveMenus ss
            return ()
          
        otherwise -> return ()
          

-- updates the enabled state of the Save, SaveAs and SaveAll menus                                
updateSaveMenus :: Session -> IO ()   
updateSaveMenus ss = do
 
    fs <- sessionReadSourceFiles ss
    
    if (length fs > 0) then do
    
        ic <- enbSelectedSourceFileIsClean ss       
        set (sessionMenuListGet ss "FileSave")      [enabled := not ic]        
        set (sessionMenuListGet ss "FileSaveAs")    [enabled := True]       
        b <- allFilesClean fs
        set (sessionMenuListGet ss "FileSaveAll")   [enabled := not b]
        set (sessionMenuListGet ss "FileClose")     [enabled := True]
        set (sessionMenuListGet ss "FileCloseAll")  [enabled := True]    
        return ()
        
    else do
    
        set (sessionMenuListGet ss "FileSave")      [enabled := False]
        set (sessionMenuListGet ss "FileSaveAs")    [enabled := False]
        set (sessionMenuListGet ss "FileClose")     [enabled := False]
        set (sessionMenuListGet ss "FileCloseAll")  [enabled := False]
        set (sessionMenuListGet ss "FileSaveAll")   [enabled := False]
        return ()
       
    where allFilesClean fs = do
            b <- doWhileTrueIO (\sf -> scnIsClean $ sourceFileGetEditor sf) fs
            return (b)
    
        
-----------------------------------------------------------------
-- Session management
-----------------------------------------------------------------

fileOpen :: Session -> String -> IO ()
fileOpen ss fp = do

    fs <- sessionReadSourceFiles ss
    b <- sessionIsOpeningState fs
   
    if b then do
    
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
         
            -- existing file so add to list, create window and set focus
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
      
    -- add source file to project
    sf <- createSourceFile p scn' (Just fp)
    updateProject ss (\pr -> projectSetFiles pr (sf:(projectGetFiles pr)))
          
    -- set focus to new page
    ix <- auiNotebookGetPageIndex nb p
    auiNotebookSetSelection nb ix  

    return (sf) 
  

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

    sf <- createSourceFile p scn Nothing

    -- enable events
    scn' <- scnEnableEvents scn (scnCallback ss)

    -- update mutable project
    updateProject ss (\pr -> projectSetFiles pr (sf:(projectGetFiles pr)))
    
    scnSetSavePoint scn'
    
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
                  
-- returns true if the source file of the currently selected tab is clean 
enbSelectedSourceFileIsClean :: Session -> IO Bool
enbSelectedSourceFileIsClean ss = do
    sf <- enbGetSelectedSourceFile ss
    ic <- scnIsClean $ sourceFileGetEditor sf
    return (ic)
    
enbSelectTab :: Session -> SourceFile -> IO ()
enbSelectTab ss sf = do
    let nb = sessionGetNotebook ss
    mix <- enbGetTabIndex ss sf
    case mix of
        Just ix -> do
            auiNotebookSetSelection nb ix
            return ()            
        Nothing -> return ()
    return ()

enbCloseTab :: Session -> SourceFile -> IO ()
enbCloseTab ss sf = do
    let nb = sessionGetNotebook ss
    mix <- enbGetTabIndex ss sf
    case (mix) of
        Just ix -> do
            auiNotebookSetSelection nb ix
            auiNotebookRemovePage nb ix
            return ()
        Nothing -> do
            debugOut ss "*** error: enbCloseTab, source file not in tabs"
            return ()

enbGetTabIndex :: Session -> SourceFile -> IO (Maybe Int)
enbGetTabIndex ss sf = do

    let nb = sessionGetNotebook ss
    pc <- auiNotebookGetPageCount nb

    -- get list of window handles as ints
    hs <- mapM (getHwnd nb) [0..(pc-1)]

    -- find tab with hwnd that matches the source file
    return (findIndex (\h -> sourceFileMatchesHwnd sf h) hs)
    
    where getHwnd nb i = do
            w <- auiNotebookGetPage nb i
            h <- windowGetHandle w
            return (ptrToWord64 h)
 
enbSetTabText :: Session -> SourceFile -> IO ()
enbSetTabText ss sf = do
    mix <- enbGetTabIndex ss sf
    case mix of
        Just ix -> do
            case (sourceFileGetFilePath sf) of
                Just fp -> do
                    auiNotebookSetPageText (sessionGetNotebook ss) ix $ takeFileName fp
                    return ()
                Nothing -> return ()                   
        Nothing -> return ()
    return ()
 
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
    
    

