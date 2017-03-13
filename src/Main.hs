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
import ArgsParser
import qualified Language.Haskell.GHC.ExactPrint.Parsers as Exactprint
import AnnsViewer
import qualified Language.Haskell.GHC.ExactPrint as Exactprint
import qualified Language.Haskell.GHC.ExactPrint.Utils as Exactprint
import PrettyExplodesPrint

   --targetFile = "src/A.hs"

   
main :: IO ()
main = do
  args <- getArgs
  let argStr = unwords args
      parseRes = ArgsParser.parser argStr
  case parseRes of
    (Left err) -> do
      putStrLn "Error parsing arguments, arguments must take the following form."
      putStr "--stage={renamer|parser|typechecker} --mode={ast|anns|both} "
      putStr "Followed by a list of files."
      print err
      exitFailure
    (Right args) ->
      case (mode args) of
        AST -> runAst args
        Anns -> runAnns args
        Both -> runBoth args
  {-case args of
    (compilerStage: targetFiles) -> do
      (modNames,strs) <- readModuleStrings compilerStage targetFiles
      mapM_ putStr $ constructOutputStrings modNames strs
    _ -> do
      name <- getProgName
      hPutStrLn stderr $ "usage: " ++ name ++ " <string> <integer>"
      exitFailure-}

runBoth :: Args -> IO ()
runBoth args = do
  dflags <- runGhc (Just libdir) $ getSessionDynFlags
  res <- mapM Exactprint.parseModule (files args)
  mapM_ handleRes res
    where --handleRes :: Either (SrcSpan, String) (Exactprint.Anns, ParsedSource) -> IO ()
          handleRes pRes =
            case pRes of
              (Right (anns, src)) -> do
                  let sRes = Exactprint.showAnnData anns 3 src
                  putStr sRes
                  putStrLn "============================="                  
              (Left (spn,str)) -> do
                putStrLn "Parsing failed with message: "
                putStrLn str
                putStrLn "============================="

runAst :: Args -> IO ()
runAst args = do
    (modNames',strs) <- runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      let dflags' = foldl  xopt_set dflags [Opt_Cpp, Opt_ImplicitPrelude, Opt_MagicHash]
      setSessionDynFlags dflags'
      targets <- mapM (flip guessTarget Nothing) (files args)
      setTargets targets
      load LoadAllTargets
      graph <- getModuleGraph
      let modNames = getModNames graph
      mods <- mapM getMods modNames
      return (modNames, (map (retrieveSource (stage args)) mods))
    mapM_ putStr $ constructOutputStrings modNames' strs
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
      retrieveSource :: Stage -> (ParsedModule, TypecheckedModule, DesugaredModule) -> String
      retrieveSource compilerStage (p, t, d) =
        case compilerStage of
          Parser -> showData Parser 2 $ pm_parsed_source p
          TypeChecker -> showData TypeChecker 2 $ tm_typechecked_source t
          Renamer -> showData Renamer 22$ (fromJust . tm_renamed_source) t


constructOutputStrings :: [String] -> [String] -> [String]
constructOutputStrings [] [] = [] 
constructOutputStrings (m:ms) (s:strs) = (s ++ "\n===================END OF MODULE: " ++ m ++ "=================\n") : constructOutputStrings ms strs


runAnns :: Args -> IO ()
runAnns args = do
  parsed <- mapM Exactprint.parseModule (files args)
  mapM_ (\(Right (as, _)) -> printAnns as) parsed

readModuleStrings :: String -> [String] -> IO ([String],[String])
readModuleStrings compilerStage targetFiles =
  runGhc (Just libdir) $ do
  dflags <- getSessionDynFlags
  let dflags' = undefined
            
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
          "renamed" -> showData Renamer 22$ (fromJust . tm_renamed_source) t
      getStage compilerStage =
        case compilerStage of
          "parsed" -> Parser
          "renamed" -> Renamer
          otherwise ->TypeChecker

 
