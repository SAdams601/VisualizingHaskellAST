module PrintTypes where
import GHC
import Outputable
import System.Environment (getArgs)
import GHC.Paths ( libdir )
--GHC.Paths is available via cabal install ghc-paths
import SYB
import DynFlags

main :: IO ()
main = do
  [targetFile, modNm] <- getArgs
  str <- runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    return ""
  putStrLn str
  


