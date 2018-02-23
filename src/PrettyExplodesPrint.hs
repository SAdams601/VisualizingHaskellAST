{-# LANGUAGE FlexibleContexts #-}
module PrettyExplodesPrint where
import Data.Tree.Pretty
import GHC as GHC
import GHC.Paths (libdir)
import DynFlags
import GHC.SYB.Utils
import Data.Tree
import Data.Generics as SYB
import GHC.SYB.Utils as SYB
import OccName (occNameString, occName)
import RdrName
import Data.Maybe
import System.IO.Unsafe
import FastString
import BasicTypes
import Language.Haskell.GHC.ExactPrint as EP
import Data.Generics.Strafunski.StrategyLib.StrategyLib

file :: FilePath
file = "testing/eval.hs"


run :: IO ()
run = do
  mPS <- EP.parseModule file
  case mPS of
    Left (_, s) -> putStrLn ("Error parsing file: " ++ s)
    Right (anns, pSource) -> do
      let forest = makeTree anns pSource
      print $ length (hsmodDecls (unLoc pSource))
      mapM_ (\t -> (putStrLn $ drawVerticalTree t) >> (putStrLn (replicate 112 '='))) forest



makeTree :: Anns -> ParsedSource -> Forest String
makeTree anns (GHC.L _ mod) = map (\(L _ (GHC.ValD bnd)) -> comp bnd) filterMod 
  where
    filterMod =  filter (\d ->case d of
                            (L _ (GHC.ValD (GHC.FunBind _ _ _ _ _ _ ))) -> True
                            _-> False) (hsmodDecls mod)
    comp :: HsBind RdrName -> Tree String 
    comp (GHC.FunBind (GHC.L _ id) _ mg _ _ _) =
      let rdr = processRdr id
          label = "BIND: " ++ rdr
          matches = mg_alts mg
          mTree = map (\(L _ m) -> something (Nothing `mkQ` (\e -> Just $ exprC e)) m) matches --everything (++) ([] `mkQ` exprC ) matches
          forest = map (\(n,mt) -> Node (rdr ++ (show n)) (fromJust mt)) (zip [1..] mTree) in
        Node label forest
    comp bnd = error ("Unexpected bind constructor: " ++ (SYB.showData SYB.Parser 3 bnd))
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
    exprC (L _ (OpApp lhs op _ rhs)) =
      let ml = (exprC lhs)
          mop = exactPrint op anns
          mr = (exprC rhs) in
        [Node ("Op: " ++ mop) (ml++mr)]
    exprC (L _ (HsCase expr pats)) =
      let cStr = [Node "Case Expression" (exprC expr)]
          pStr = handleMG pats in
        [Node ("Case") (cStr ++ pStr)]
            
    exprC (L _ (HsPar e)) = exprC e
    exprC (L _ (SectionL l r)) = let ml = exprC l
                                     mr = exprC r in
                                   [Node "Section" (ml++mr)] 
    exprC (L _ (HsOverLit lit)) = [Node (processLit lit) []]
    exprC (L _ (ExplicitTuple lst _)) = [Node "Tuple" (mapMaybe procTuple lst)]
          where procTuple :: LHsTupArg RdrName -> Maybe (Tree String)
                procTuple (L _ (Present e)) = Just $ Node "TupleElem" (exprC e)
                procTuple _ = Nothing
    exprC e = unsafePerformIO  ((putStrLn ("exprC failure: " ++ SYB.showData SYB.Parser 3 e)) >> return (gmapQ ((Node "Error: exprC" []) `mkQ` (\ i -> Node "" (exprC i))) e))
    handleMG :: MatchGroup RdrName (LHsExpr RdrName) -> Forest String
    handleMG mg =
      let alts = mg_alts mg in
        map handleM alts
      where handleM :: LMatch RdrName (LHsExpr RdrName) -> Tree String
            handleM (L _ m) = let pStr = foldr (++) "" (map (\p -> exactPrint p anns) (m_pats m))
                                  mGrhsE = something (Nothing `mkQ` (\e -> Just $ exprC e)) (m_grhss m) in
              Node ("Match: " ++ pStr) $ fromMaybe [] mGrhsE                          
                            
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
processRdr (Exact nm) = occNameString (occName nm)

processLit :: HsOverLit RdrName -> String
processLit lit = "Lit: " ++ (prc val)
      where val = ol_val lit
            prc (HsIntegral _ i) = show i
            prc (HsFractional f) = fl_text f
            prc (HsIsString _ fs) = unpackFS fs
