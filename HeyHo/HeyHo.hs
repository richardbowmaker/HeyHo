
module Main where
import Graphics.UI.WX
import Graphics.UI.WXCore
import Scintilla
import qualified Data.ByteString.Char8 as BS (pack, readFile, writeFile, ByteString)


-- Session data
data SourceFile = SourceFile {  edPanel     :: Panel (),        -- The panel added to the AuiNotebookManager
                                editor      :: ScnEditor,       -- The Scintilla editor, child window of panel
                                filePath    :: Maybe String,    -- Source file path, Nothing = file name not set yet
                                isClean     :: Bool }           -- False = file needs saving
                                                        
data Session = Session {    mainFrame   :: Frame (),             -- Main window
                            auiMgr      :: AuiManager (),       -- Application level AUI manager
                            editorNB    :: AuiNotebook (),      -- Notebook of source file editors
                            openFiles   :: [SourceFile] }       -- List of open source files
                

{-                
openSourceFile :: Session -> String -> IO (Session)
openSourceFile s fn = do

    if fileAlreadyOpen then
        -- select editor
    else if emptyWindow then 
            -- set editor
            -- select editor
          else
            -- add new editor
       

    return (s')
-}


fileOpen :: Session -> String -> IO (Session)
fileOpen (Session aui nb fs) fp = do
    
    -- create panel with scintilla editor inside
    p <- panel nb []
    hwnd <- windowGetHandle p
    scn <- scnCreateEditor hwnd
    scnConfigureHaskell scn
    scnEnableEvents scn scnCallback 

    -- add panel to notebook
    auiNotebookAddPage nb p fp False 0
    ta <- auiSimpleTabArtCreate
    auiNotebookSetArtProvider nb ta

    -- read file to editor
    bs <- BS.readFile fp
    scnSetText scn bs
    
    -- update session, list of source files
    let s' = (Session aui nb (SourceFile p scn fp True) :: fs)
    return (s')
                
main = start mainGUI

mainGUI :: IO ()
mainGUI = do 
  
    -- main window
    f <- frame []    
    set f [ text := "HeyHo", size := (Size 1300 800)]
     
    -- AUI manager and child windows
    ss <- setUpMainWindow f
  
    -- menus
    menus <- setupMenus ss

     -- create statusbar field
    statusBar' <- statusField   [text := "Welcome to wxHaskell"]

    -- set the statusbar and menubar
    set f [ statusBar := [statusBar'], menuBar := menus]

--    set f [on closing := onClosing f am scn]
        
    return ()
   
------------------------------------------------------------    
-- Setup menus
------------------------------------------------------------    

setupMenus :: Session -> IO ([Menu()])
setupMenus ss@(Session f _ _ _) = do

    -- file menu  
    menuFile        <- menuPane             [text := "&File"]
    menuFileOpen    <- menuItem menuFile    [text := "Open ...\tCtrl-O", help := "Opens a file"]
    set menuFileOpen [on command := onFileOpen ss]
                                             
    menuFileSave    <- menuItem menuFile    [text := "Save\tCtrl-S", help := "Saves a file"]
    menuFileSaveAs  <- menuItem menuFile    [text := "Save As ...\tCtrl-Shift-S", help := "Saves a file"]
    set menuFileSaveAs [on command := onFileSaveAs ss]
                                             
    menuAppendSeparator menuFile
                             
    menuQuit  <- menuQuit menuFile [help := "Quit the demo", on command := close f]

    menuEdit         <- menuPane            [text := "&Edit"]
    menuEditUndo     <- menuItem menuEdit   [text := "Undo\tCtrl-Z"]
    
    menuBuild        <- menuPane            [text := "Build"]
    menuBuildCompile <- menuItem menuBuild  [text := "Compile\tCtrl-F7",       help := "Compiles current source file"]
    menuBuildBuild   <- menuItem menuBuild  [text := "Build\tF7",              help := "Build the project"]
    menuBuildReBuild <- menuItem menuBuild  [text := "Rebuild\tCtrl-Alt-F7",   help := "Rebuild the project"]
    menuBuildClean   <- menuItem menuBuild  [text := "Clean",                  help := "Clean the project"]
          
    -- create Help menu
    menuHelp'        <- menuHelp []
    menuHelpAbout    <- menuAbout menuHelp' [help := "About HeyHo", on command := infoDialog f "About HeyHo" "mmmmm !"]
    
    return ([menuFile, menuEdit, menuBuild, menuHelp'])

------------------------------------------------------------    
-- Setup AUI manager and its child windows
------------------------------------------------------------    
   
setUpMainWindow :: Frame () -> IO (Session)    
setUpMainWindow s = do 

    am <- auiManagerCreate f wxAUI_MGR_DEFAULT
      
    -- add dockable tree
    tree <- createTree f
    api <- auiPaneInfoCreateDefault
    auiPaneInfoCaption api "Tree Control"
    auiPaneInfoLeft api
    auiPaneInfoCloseButton api True
    
    auiManagerAddPaneByPaneInfo am tree api
    
    -- add dockable grid
    grid <- createGrid f
    api <- auiPaneInfoCreateDefault
    auiPaneInfoCaption api "Grid Control"
    auiPaneInfoBottom api
    auiPaneInfoCloseButton api True
    
    auiManagerAddPaneByPaneInfo am grid api
    
    -- add editor notebook
    (enb sf) <- createEditorNoteBook f
   
    api <- auiPaneInfoCreateDefault
    auiPaneInfoCaption api "Editor"
    auiPaneInfoCentre api
    auiPaneInfoCloseButton api True
    auiPaneInfoMaximizeButton api True

    auiManagerAddPaneByPaneInfo am enb api
   
    -- add output pane
    nb <- createNoteBook f
    
    api <- auiPaneInfoCreateDefault
    auiPaneInfoCaption api "Output"
    auiPaneInfoBottom api
    auiPaneInfoCloseButton api True

    auiManagerAddPaneByPaneInfo am nb api

    auiManagerUpdate am
 
    -- return the session data
    return (Session f am enb [sf])

------------------------------------------------------------    
-- Event handlers
------------------------------------------------------------    

onClosing :: Frame() -> AuiManager() -> ScnEditor -> IO ()
onClosing f aui scn = do 
    scnDisableEvents scn
    auiManagerUnInit aui
    windowDestroy f
    return ()

scnCallback :: SCNotification -> IO ()
scnCallback _ = return ()

-- File Open
onFileOpen :: Session -> IO (Session)
onFileOpen ss@(Session f _ _ _) = do             -- wxFD_OPEN wxFD_FILE_MUST_EXIST
    fd <- fileDialogCreate f "Open file" "." "" "*.hs" (Point 100 100) 0x11
    ans <- dialogShowModal fd
    if ans == wxID_OK
    then do
        fn <- fileDialogGetPath fd
        fileOpen ss fn 
    else
        return ()

-- File Save As  
onFileSaveAs :: Frame () -> ScnEditor -> IO ()
onFileSaveAs f scn = do                                       -- wxFD_SAVE wxFD_OVERWRITE_PROMPT
    fd <- fileDialogCreate f "Save file as" "." "" "*.hs" (Point 100 100) 0x6
    rs <- dialogShowModal fd    
    if rs == wxID_OK
    then do    
        fn <- fileDialogGetPath fd
        fileSave scn fn
        return ()
    else 
        return ()

fileSave :: ScnEditor -> String -> IO ()
fileSave scn fn = do
    bs <- scnGetAllText scn
    BS.writeFile fn bs
    return ()
        
------------------------------------------------------------    
-- Create the source file editor notebook
------------------------------------------------------------    

createEditorNoteBook :: Frame () -> IO (AuiNotebook (), SourceFile)
createEditorNoteBook f = do

    -- create the notebook
    nb <- auiNotebookCreate f idAny (Point 0 0) (Size 0 0) (wxCLIP_CHILDREN + wxAUI_NB_TOP)
    set nb []

    -- create panel with scintilla editor inside
    p <- panel nb []
    hwnd <- windowGetHandle p
    scn <- scnCreateEditor hwnd
    scnConfigureHaskell scn
    scnEnableEvents scn scnCallback 

    -- add panel to notebook
    auiNotebookAddPage nb p "..." False 0
    ta <- auiSimpleTabArtCreate
    auiNotebookSetArtProvider nb ta
    
    -- source file info
    let sf = (SourceFile p scn Nothing True)
    return (nb, sf)

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
