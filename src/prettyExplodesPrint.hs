module PrettyExplodesPrint where
--import Data.Tree.Pretty
import GHC
import GHC.Paths (libdir)
import DynFlags
import GHC.SYB.Utils

file :: FilePath
file = "testing/listfunctions.hs"


run :: IO ()
run = do
  p <- runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    let dflags' = foldl xopt_set dflags [Opt_Cpp, Opt_ImplicitPrelude, Opt_MagicHash]
    setSessionDynFlags dflags'
    target <- guessTarget file Nothing
    setTargets [target]
    load LoadAllTargets
    modSum <- getModSummary $ mkModuleName "S"
    parseModule modSum
  putStrLn $ showData Parser 3 p

