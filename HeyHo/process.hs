

module Main where
import Graphics.UI.WX
import Graphics.UI.WXCore
import System.Process.Common
import System.Process
import System.IO
import Control.Monad (unless)
import Control.Exception (try, throwIO)
import qualified GHC.IO.Exception as G
import Pipes

main = start mainGUI


stdoutLn :: Consumer String IO ()
stdoutLn = do
    str <- await  -- 'await' a 'String'
    x   <- lift $ try $ putStrLn str
    case x of
        -- Gracefully terminate if we got a broken pipe error
        Left e@(G.IOError { G.ioe_type = t}) ->        
            lift $ unless (t == G.ResourceVanished) $ throwIO e
        -- Otherwise loop
        Right () -> stdoutLn
        
compile :: IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)        
compile = do
    x <- createProcess_ "errors" (proc "D:\\_Rick's\\haskell\\HeyHo\\build.bat" ["heyho"]){ cwd = Just "D:\\_Rick's\\haskell\\HeyHo",
                                        std_out = CreatePipe }
    return (x)
    
compile' :: IO Handle        
compile' = do

    (_, Just hout, _, _) <- createProcess_ "errors" (proc "D:\\_Rick's\\haskell\\HeyHo\\build.bat" ["heyho"]){ cwd = Just "D:\\_Rick's\\haskell\\HeyHo",
                                        std_out = CreatePipe }
    return (hout)

mainGUI :: IO ()
mainGUI = do 
  
    -- main window
    f <- frame []    
    
    p <- panel f []
     
    -- create statusbar field
    sf1 <- statusField [text := "SF1", statusWidth := 20]
    sf2 <- statusField [text := "SF2"]

{-    
--    r <- createProcess (proc "dir" [])

    (_, Just hout, _, _) <-
        createProcess_ "errors" (proc "notepad.exe" []){ cwd = Just "D:\\_Rick's\\haskell\\HeyHo",
                                        std_out = CreatePipe }
-}                                        
                                        
{- 
    -- set the statusbar and menubar
    set f [statusBar := [sf1,sf2]]
    
    h <- compile'
    runEffect $ lift (hGetContents h) >-> stdoutLn

--    runEffect $ lift getLine >~ stdoutLn
-}

    (_, Just hout, _, _) <- createProcess_ "errors" (proc "D:\\_Rick's\\haskell\\HeyHo\\build.bat" ["heyho"]){ cwd = Just "D:\\_Rick's\\haskell\\HeyHo",
                                        std_out = CreatePipe }
    str <- hGetContents hout
    putStrLn "compiling"
    putStrLn str    
    putStrLn "finished"


    return ()

