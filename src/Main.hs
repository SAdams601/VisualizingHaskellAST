{-# LANGUAGE ScopedTypeVariables #-}
module Main where
   import GHC
   import Outputable
   import GHC.Paths ( libdir )
   import DynFlags
   import System.Environment
   import System.IO ( hPutStrLn, stderr )
   import System.Exit ( exitFailure )
   import System.IO.Unsafe
   import Data.Generics
   import GHC.SYB.Utils
   import Data.List
   import FastString
   import Control.Monad.State
   import System.Directory
   import Data.Maybe
   import System.FilePath.Posix

   --targetFile = "src/A.hs"

   main :: IO ()
   main = do
      args <- getArgs
      case args of
         [targetFiles] -> do
            let filesList = read targetFiles :: [String]

            strs <- example filesList
            mapM_ (\s -> putStr (s ++ "\n===================END OF MODULE=================\n")) strs
 
         _ -> do
            name <- getProgName
            hPutStrLn stderr $ "usage: " ++ name ++ " <string> <integer>"
            exitFailure

   example :: [String] -> IO [String]
   example targetFiles =
      runGhc (Just libdir) $ do
            dflags <- getSessionDynFlags
            let dflags' = foldl  xopt_set dflags [Opt_Cpp, Opt_ImplicitPrelude, Opt_MagicHash]
            setSessionDynFlags dflags'
            
            targets <- mapM (flip guessTarget Nothing) targetFiles
            setTargets targets
            ts <- getTargets

            load LoadAllTargets
            
            graph <- getModuleGraph
            targets <- getTargets
            let modNames = getModNames graph

            desugaredMods <- mapM getDesugaredMod modNames
            
            return $ getModNames graph

            return $ (map (\m -> showData TypeChecker 2 ((tm_typechecked_source . dm_typechecked_module) m)) desugaredMods)

               where 
                  getDesugaredMod :: GhcMonad m => String -> m DesugaredModule 
                  getDesugaredMod modName = do
                     modSum <- getModSummary $ mkModuleName modName
                     p <- parseModule modSum
                     t <- typecheckModule p
                     desugarModule t
                  getModNames :: [ModSummary] -> [String]
                  getModNames sums = map extractName sums
                  extractName :: ModSummary -> String
                  extractName ms =  moduleNameString ((GHC.moduleName . ms_mod) ms)
                  targetIsModule :: Target -> Bool 
                  targetIsModule t = case (targetId t) of
                     TargetModule _ -> True
                     otherwise      -> False

 