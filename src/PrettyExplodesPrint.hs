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

file :: FilePath
file = "testing/explodes.hs"


run :: IO ()
run = do
  p <- runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    let dflags' = foldl xopt_set dflags [Opt_Cpp, Opt_ImplicitPrelude, Opt_MagicHash]
    setSessionDynFlags dflags'
    target <- guessTarget file Nothing
    setTargets [target]
    load LoadAllTargets
    modSum <- getModSummary $ mkModuleName "Explodes"
    parseModule modSum
  let pSource = pm_parsed_source p
      forest = makeTree pSource
      s = drawVerticalForest forest
  print $ length (hsmodDecls (unLoc pSource))
  mapM_ (\t -> putStr $ drawVerticalTree t) forest

makeTree :: ParsedSource -> Forest String
makeTree (GHC.L _ mod) = let decls = hsmodDecls mod in
  everything (++)  ([] `mkQ` comp) decls
  where
    comp :: HsBind RdrName -> Forest String 
    comp (GHC.FunBind (GHC.L _ id) _ matches _ _ _) =
          let label = "BIND: " ++ (processRdr id)
              mTree  = something (Nothing `mkQ` exprC) matches 
              forest = case mTree of
                Nothing -> []
                Just t -> [t]
                --everything (++) ([] `mkQ` exprC) (mg_alts matches)
          in
            [Node label forest]
      where exprC :: HsExpr RdrName -> Maybe (Tree String)
            exprC (HsVar id) = Just (Node (processRdr id) [])
            exprC (HsLam mg) =
              let f = case (something (Nothing `mkQ` exprC) mg) of
                    Nothing -> []
                    Just t -> [t] in
                Just $ Node "Lam" f
            exprC (HsApp (L _ l) (L _ r)) = Just $ Node "App" [(exprC l),(exprC r)]
            exprC _ = Nothing
            

genericTree :: (Typeable a, Typeable b) => (b -> Tree String) -> a -> Tree String
genericTree = mkQ (Node "ERROR" [])

processRdr :: RdrName -> String
processRdr (Unqual occ) = occNameString occ
processRdr (Qual modnm occ) = (moduleNameString modnm) ++ "." ++ occNameString occ
