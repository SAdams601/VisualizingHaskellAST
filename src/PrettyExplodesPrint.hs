{-# LANGUAGE FlexibleContexts #-}
module PrettyExplodesPrint where
import Data.Tree.Pretty
import GHC as GHC
import GHC.Paths (libdir)
import DynFlags
import GHC.SYB.Utils
import Data.Tree
import Data.Generics as SYB
import OccName (occNameString)
import Data.Maybe
import System.IO.Unsafe

file :: FilePath
file = "testing/explodes2.hs"


run :: IO ()
run = do
  p <- runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    let dflags' = foldl xopt_set dflags [Opt_Cpp, Opt_ImplicitPrelude, Opt_MagicHash]
    setSessionDynFlags dflags'
    target <- guessTarget file Nothing
    setTargets [target]
    load LoadAllTargets
    modSum <- getModSummary $ mkModuleName "Explodes2"
    parseModule modSum
  let pSource = pm_parsed_source p
      forest = makeTree pSource
--      s = drawVerticalForest forest
  print $ length (hsmodDecls (unLoc pSource))
  mapM_ (\t -> (putStrLn $ drawVerticalTree t) >> (putStrLn (replicate 112 '='))) forest

makeTree :: ParsedSource -> Forest String
makeTree (GHC.L _ mod) = everything (++) ([] `mkQ` (\e -> [comp e])) (hsmodDecls mod)
  where
    comp :: HsBind RdrName -> Tree String 
    comp (GHC.FunBind (GHC.L _ id) _ matches _ _ _) =
      let label = "BIND: " ++ (processRdr id)
          mTree = something (Nothing `mkQ` (\e -> Just $ exprC e)) matches
          forest = fromMaybe [] mTree in
        Node label forest
    exprC :: LHsExpr RdrName -> Forest String
    exprC (L _ (HsVar id)) = [Node (processRdr id) []]
    exprC (L _ (HsLam mg)) =
          let mf = something (Nothing `mkQ` (\l -> Just $ exprC l)) mg
              f = case mf of
                    Nothing -> debug mg
                    Just i -> i
          in
            [Node "Lam" f]
    exprC (L _ (HsApp l r)) =
      let ml = (exprC l)
          mr = (exprC r) in
        [Node "App" (ml ++ mr)]
    exprC (L _ (HsPar e)) = exprC e
    exprC e = gmapQ ((Node "Error: exprC" []) `mkQ` (\ i -> Node "" (exprC i))) e
    
    debug :: MatchGroup RdrName (LHsExpr RdrName) -> Forest String
    debug e = unsafePerformIO ((putStrLn ("Failed on : " ++ showData Parser 3 e))
                             >> (return []))
            
getFor :: Maybe (Tree String) -> Forest String
getFor Nothing = []
getFor (Just t) = [t]

genericTree :: (Typeable a, Typeable b) => (b -> Tree String) -> a -> Tree String
genericTree = mkQ (Node "ERROR" [])

processRdr :: RdrName -> String
processRdr (Unqual occ) = "Var: " ++ occNameString occ
processRdr (Qual modnm occ) = "Var: " ++ (moduleNameString modnm) ++ "." ++ occNameString occ
