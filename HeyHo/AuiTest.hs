
module Main where
import Graphics.UI.WX
import Graphics.UI.WXCore
import Scintilla

main = start mainGUI

mainGUI :: IO ()
mainGUI = do 

    f <- frame [] 
    
    set f [ text := "Aui Test", 
            size := (Size 500 500)]
                       
    auiMgr <- auiManagerCreate f wxAUI_MGR_DEFAULT
      
    -- add dockable tree
    tree <- createTree f   
    api <- auiPaneInfoCreateDefault
    auiPaneInfoCaption api "Tree Control"
    auiPaneInfoLeft api
    auiPaneInfoLayer api 1
    auiPaneInfoPosition api 1
    auiPaneInfoCloseButton api True
    auiPaneInfoMaximizeButton api True
    
    auiManagerAddPaneByPaneInfo auiMgr tree api
    
    -- add dockable grid
    grid <- createGrid f
    api <- auiPaneInfoCreateDefault
    auiPaneInfoCaption api "Grid Control"
    auiPaneInfoRight api
    auiPaneInfoLayer api 1
    auiPaneInfoPosition api 1
    auiPaneInfoCloseButton api True
    auiPaneInfoMaximizeButton api True
    
    auiManagerAddPaneByPaneInfo auiMgr grid api
    
    -- add scintilla editor
    p <- panel f []
    hwnd <- windowGetHandle p
    scn <- scnCreateEditor hwnd
    
    set p [ bgcolor := red ]
    api <- auiPaneInfoCreateDefault
    auiPaneInfoCaption api "Scintilla"
    auiPaneInfoCentre api
    auiPaneInfoLayer api 1
    auiPaneInfoPosition api 1
    auiPaneInfoCloseButton api True
    auiPaneInfoMaximizeButton api True

    auiManagerAddPaneByPaneInfo auiMgr p api
    
             
--    menuFileOpen <- menuItemCreate
--    menuItemSetItemLabel menuFileOpen "Open"
    
    menuFile <- menuCreate "Open" 0
 --   menuAppendItem menuFile menuFileOpen
    
    mbar <- menuBarCreate 0
    menuBarAppend mbar menuFile "&File"
    
    frameSetMenuBar f mbar
 
    auiManagerUpdate auiMgr

    set f [on closing := onClosing f auiMgr]
     
    return ()

cmdClose :: Button () -> IO ()
cmdClose b = do
    set b [ text := "Clicked! "]
    return ()
    
onClosing :: Frame() -> AuiManager() -> IO ()
onClosing f aui = do   
    auiManagerUnInit aui
    windowDestroy f
    return ()
    
------------------------------------------------------------    
-- Tree Control
------------------------------------------------------------    
    
createTree :: Frame () ->  IO (TreeCtrl ())
createTree f = do      
    tree <- treeCtrl f []
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
