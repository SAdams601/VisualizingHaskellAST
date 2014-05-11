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
         (compilerStage: targetFiles) -> do
            (modNames,strs) <- readModuleStrings compilerStage targetFiles
            mapM_ putStr $ constructOutputStrings modNames strs
 
         _ -> do
            name <- getProgName
            hPutStrLn stderr $ "usage: " ++ name ++ " <string> <integer>"
            exitFailure

   constructOutputStrings :: [String] -> [String] -> [String]
   constructOutputStrings [] [] = [] 
   constructOutputStrings (m:ms) (s:strs) = (s ++ "\n===================END OF MODULE: " ++ m ++ "=================\n") : constructOutputStrings ms strs

   readModuleStrings :: String -> [String] -> IO ([String],[String])
   readModuleStrings compilerStage targetFiles =
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

            mods <- mapM getMods modNames
            let stage = getStage compilerStage
            return $ (modNames, (map (retrieveSource compilerStage) mods))

               where 
                  getMods :: GhcMonad m => String -> m (ParsedModule, TypecheckedModule, DesugaredModule)
                  getMods modName = do
                     modSum <- getModSummary $ mkModuleName modName
                     p <- parseModule modSum
                     t <- typecheckModule p
                     d <- desugarModule t
                     return (p,t,d)
                  getModNames :: [ModSummary] -> [String]
                  getModNames sums = map extractName sums
                  extractName :: ModSummary -> String
                  extractName ms =  moduleNameString ((GHC.moduleName . ms_mod) ms)
                  targetIsModule :: Target -> Bool 
                  targetIsModule t = case (targetId t) of
                     TargetModule _ -> True
                     otherwise      -> False
                  retrieveSource :: String -> (ParsedModule, TypecheckedModule, DesugaredModule) -> String
                  retrieveSource compilerStage (p, t, d) =
                     case compilerStage of
                        "parsed" -> showData Parser 2 $ pm_parsed_source p
                        "typed" -> showData TypeChecker 2 $ tm_typechecked_source t
                        "desugared" -> showData TypeChecker 2 $ (tm_typechecked_source . dm_typechecked_module) d
                  getStage compilerStage =
                     if compilerStage == "parsed" then Parser else TypeChecker

 