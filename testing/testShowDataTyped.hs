{Bag(Located (HsBind Var)): 
  [
   (L {testing/test.hs:5:1-13} 
    -- The binding for g
    (AbsBinds 
     -- abs_tvs :: [TyVar]
     [{Var: a}] 
     -- abs_ev_vars :: [EvVar]
     [{Var: $dNum}] 
     -- abs_exports :: [ABExport idL]
     [
      (ABE {Var: Test.g} {Var: g} 
       (WpCompose 
        (WpCompose 
         (WpCompose 
          (WpTyLam {Var: a}) 
          (WpEvLam {Var: $dNum})) 
         (WpLet 
          ({abstract:TcEvBinds}))) 
        (WpCompose 
         (WpEvApp 
          (EvId {Var: $dNum})) 
         (WpTyApp a))) 
       (SpecPrags 
        []))] 
     --abs_ev_binds :: TcEvBinds
     ({abstract:TcEvBinds}) 
     -- abs_binds :: LHsBinds idL
     {Bag(Located (HsBind Var)): 
     [
      (L {testing/test.hs:5:1-13} 
       (FunBind 
        -- fun_id :: Located idL
        (L {testing/test.hs:5:1} {Var: g}) 
        -- fun_infix :: Bool - This is true when this is an infix declaration
        (False)
        -- fun_matches :: MatchGroup idR - the payload 
        (MatchGroup 
         [
          (L {testing/test.hs:5:1-13} 
           (Match 
            [
             (L {testing/test.hs:5:3} 
              (VarPat {Var: a})),
             (L {testing/test.hs:5:5} 
              (VarPat {Var: b}))] 
            (Nothing) 
            (GRHSs 
             [
              (L {testing/test.hs:5:9-13} 
               (GRHS 
                [] 
                (L {testing/test.hs:5:9-13} 
                 (OpApp 
                  (L {testing/test.hs:5:9} 
                   (HsVar {Var: a})) 
                  (L {testing/test.hs:5:11} 
                   (HsWrap 
                    (WpCompose 
                     (WpEvApp 
                      (EvId {Var: $dNum})) 
                     (WpTyApp a)) 
                    (HsVar {Var: GHC.Num.-}))) {Fixity: infixl 6} 
                  (L {testing/test.hs:5:13} 
                   (HsVar {Var: b}))))))] 
             (EmptyLocalBinds))))] a -> a -> a)
        -- fun_co_fn :: HsWrapper      
        (WpHole) 
        -- bind_fvs :: NameSet
        {!NameSet placeholder here!}
        -- fun_tick :: Maybe (Tickish Id)
        (Nothing)))]})),
   -- The binding for f
   (L {testing/test.hs:3:1-13} 
    (AbsBinds 
     [{Var: a}] 
     [{Var: $dNum}] 
     [
      (ABE {Var: Test.f} {Var: f} 
       (WpCompose 
        (WpCompose 
         (WpCompose 
          (WpTyLam {Var: a}) 
          (WpEvLam {Var: $dNum})) 
         (WpLet 
          ({abstract:TcEvBinds}))) 
        (WpCompose 
         (WpEvApp 
          (EvId {Var: $dNum})) 
         (WpTyApp a))) 
       (SpecPrags 
        []))] 
     ({abstract:TcEvBinds}) {Bag(Located (HsBind Var)): 
     [
      (L {testing/test.hs:3:1-13} 
       (FunBind 
        (L {testing/test.hs:3:1} {Var: f}) 
        (False) 
        (MatchGroup 
         [
          (L {testing/test.hs:3:1-13} 
           (Match 
            [
             (L {testing/test.hs:3:3} 
              (VarPat {Var: a})),
             (L {testing/test.hs:3:5} 
              (VarPat {Var: b}))] 
            (Nothing) 
            (GRHSs 
             [
              (L {testing/test.hs:3:9-13} 
               (GRHS 
                [] 
                (L {testing/test.hs:3:9-13} 
                 (OpApp 
                  (L {testing/test.hs:3:9} 
                   (HsVar {Var: a})) 
                  (L {testing/test.hs:3:11} 
                   (HsWrap 
                    (WpCompose 
                     (WpEvApp 
                      (EvId {Var: $dNum})) 
                     (WpTyApp a)) 
                    (HsVar {Var: GHC.Num.+}))) {Fixity: infixl 6} 
                  (L {testing/test.hs:3:13} 
                   (HsVar {Var: b}))))))] 
             (EmptyLocalBinds))))] a -> a -> a) 
        (WpHole) {!NameSet placeholder here!} 
        (Nothing)))]}))]}
===================END OF MODULE: Test=================
