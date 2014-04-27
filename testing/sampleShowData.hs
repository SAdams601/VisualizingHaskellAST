{Bag(Located (HsBind Var)): 
  [
   -- ============== y = 0 line 6
   (L {../HaRe/test/GenDef/D4.hs:6:1-3} 
    (AbsBinds 
     --abs_tvs :: [TyVar] type TyVar = Var
     [] 
     -- abs_ev_vars :: [EvVar], type EvVar = EvId, type EvId = Id, type Id = Var
     [] 
     -- abs_exports :: [ABExport idL] This seems to be some sort of structure to help handle what this variable looks like to a client module ?
     [
      (ABE {Var: D4.y} {Var: y} 
       (WpHole) 
       (SpecPrags 
        []))]
      -- abs_ev_binds :: TcEvBinds   
     ({abstract:TcEvBinds}) 
     -- abs_binds :: LHsBinds idL
     {Bag (Located (HsBind Var)): 
     [
      (L {../HaRe/test/GenDef/D4.hs:6:1-3} 
       (FunBind 
        (L {../HaRe/test/GenDef/D4.hs:6:1} {Var: y}) 
        (False) 
        (MatchGroup 
         [
          (L {../HaRe/test/GenDef/D4.hs:6:1-3} 
           (Match 
            [] 
            (Nothing) 
            (GRHSs 
             [
              (L {../HaRe/test/GenDef/D4.hs:6:3} 
               (GRHS 
                [] 
                (L {../HaRe/test/GenDef/D4.hs:6:3} 
                 (HsOverLit 
                  (OverLit 
                   (HsIntegral 
                    (0)) 
                   (False) 
                   (HsApp 
                    (L {<no location info>} 
                     (HsWrap 
                      (WpCompose 
                       (WpEvApp 
                        (EvId {Var: $dNum})) 
                       (WpTyApp GHC.Integer.Type.Integer)) 
                      (HsVar {Var: GHC.Num.fromInteger}))) 
                    (L {<no location info>} 
                     (HsLit 
                      (HsInteger 
                       (0) GHC.Integer.Type.Integer)))) GHC.Integer.Type.Integer)))))] 
             (EmptyLocalBinds))))] GHC.Integer.Type.Integer) 
        (WpHole) {!NameSet placeholder here!} 
        (Nothing)))]})),

      -- =============== End line six ===================

      -- =============== Line 8: f x =x + ( y + 1) =========================
   (L {../HaRe/test/GenDef/D4.hs:8:1-17} 
    (AbsBinds 
     [] 
     [] 
     [
      (ABE {Var: D4.f} {Var: f} 
       (WpHole) 
       (SpecPrags 
        []))] 
     ({abstract:TcEvBinds}) {Bag(Located (HsBind Var)): 
     [
      (L {../HaRe/test/GenDef/D4.hs:8:1-17} 
       (FunBind 
        (L {../HaRe/test/GenDef/D4.hs:8:1} {Var: f}) 
        (False) 
        (MatchGroup 
         [
          (L {../HaRe/test/GenDef/D4.hs:8:1-17} 
           (Match 
            [
             (L {../HaRe/test/GenDef/D4.hs:8:3} 
              (VarPat {Var: x}))] 
            (Nothing) 
            (GRHSs 
             [
              (L {../HaRe/test/GenDef/D4.hs:8:6-17} 
               (GRHS 
                [] 
                (L {../HaRe/test/GenDef/D4.hs:8:6-17} 
                 (OpApp 
                  (L {../HaRe/test/GenDef/D4.hs:8:6} 
                   (HsVar {Var: x})) 
                  (L {../HaRe/test/GenDef/D4.hs:8:8} 
                   (HsWrap 
                    (WpCompose 
                     (WpEvApp 
                      (EvId {Var: $dNum})) 
                     (WpTyApp GHC.Integer.Type.Integer)) 
                    (HsVar {Var: GHC.Num.+}))) {Fixity: infixl 6} 
                  (L {../HaRe/test/GenDef/D4.hs:8:10-17} 
                   (HsPar 
                    (L {../HaRe/test/GenDef/D4.hs:8:12-16} 
                     (OpApp 
                      (L {../HaRe/test/GenDef/D4.hs:8:12} 
                       (HsVar {Var: D4.y})) 
                      (L {../HaRe/test/GenDef/D4.hs:8:14} 
                       (HsWrap 
                        (WpCompose 
                         (WpEvApp 
                          (EvId {Var: $dNum})) 
                         (WpTyApp GHC.Integer.Type.Integer)) 
                        (HsVar {Var: GHC.Num.+}))) {Fixity: infixl 6} 
                      (L {../HaRe/test/GenDef/D4.hs:8:16} 
                       (HsOverLit 
                        (OverLit 
                         (HsIntegral 
                          (1)) 
                         (False) 
                         (HsApp 
                          (L {<no location info>} 
                           (HsWrap 
                            (WpCompose 
                             (WpEvApp 
                              (EvId {Var: $dNum})) 
                             (WpTyApp GHC.Integer.Type.Integer)) 
                            (HsVar {Var: GHC.Num.fromInteger}))) 
                          (L {<no location info>} 
                           (HsLit 
                            (HsInteger 
                             (1) GHC.Integer.Type.Integer)))) GHC.Integer.Type.Integer)))))))))))] 
             (EmptyLocalBinds))))] GHC.Integer.Type.Integer -> GHC.Integer.Type.Integer) 
        (WpHole) {!NameSet placeholder here!} 
        (Nothing)))]})),

    -- ======================== End Line 8 ===========================

    -- ======================== Line 10: sumFun xs = sum $ map f xs ==
   (L {../HaRe/test/GenDef/D4.hs:10:1-26} 
    (AbsBinds 
     [] 
     [] 
     [
      (ABE {Var: D4.sumFun} {Var: sumFun} 
       (WpHole) 
       (SpecPrags 
        []))] 
     ({abstract:TcEvBinds}) {Bag(Located (HsBind Var)): 
     [
      (L {../HaRe/test/GenDef/D4.hs:10:1-26} 
       (FunBind 
        -- fun_id
        (L {../HaRe/test/GenDef/D4.hs:10:1-6} {Var: sumFun}) 
        -- fun_infix
        (False)
        -- fun_matches :: HsExpr.MatchGroup
        (MatchGroup 
         --[LMatch id] 
         -- LMatch id = Located (Match id)
         [
          (L {../HaRe/test/GenDef/D4.hs:10:1-26} 
           (Match 
            [
             (L {../HaRe/test/GenDef/D4.hs:10:8-9} 
              (VarPat {Var: xs}))] 
            (Nothing) 
            (GRHSs 
             [
              (L {../HaRe/test/GenDef/D4.hs:10:13-26} 
               (GRHS 
                [] 
                (L {../HaRe/test/GenDef/D4.hs:10:13-26} 
                 (OpApp 
                  (L {../HaRe/test/GenDef/D4.hs:10:13-15} 
                   (HsWrap 
                    (WpCompose 
                     (WpEvApp 
                      (EvId {Var: $dNum})) 
                     (WpTyApp GHC.Integer.Type.Integer)) 
                    (HsVar {Var: Data.List.sum}))) 
                  (L {../HaRe/test/GenDef/D4.hs:10:17} 
                   (HsWrap 
                    (WpCompose 
                     (WpTyApp GHC.Integer.Type.Integer) 
                     (WpTyApp [GHC.Integer.Type.Integer])) 
                    (HsVar {Var: GHC.Base.$}))) {Fixity: infixr 0} 
                  (L {../HaRe/test/GenDef/D4.hs:10:19-26} 
                   (HsApp 
                    (L {../HaRe/test/GenDef/D4.hs:10:19-23} 
                     (HsApp 
                      (L {../HaRe/test/GenDef/D4.hs:10:19-21} 
                       (HsWrap 
                        (WpCompose 
                         (WpTyApp GHC.Integer.Type.Integer) 
                         (WpTyApp GHC.Integer.Type.Integer)) 
                        (HsVar {Var: GHC.Base.map}))) 
                      (L {../HaRe/test/GenDef/D4.hs:10:23} 
                       (HsVar {Var: D4.f})))) 
                    (L {../HaRe/test/GenDef/D4.hs:10:25-26} 
                     (HsVar {Var: xs}))))))))] 
             (EmptyLocalBinds))))] [GHC.Integer.Type.Integer] -> GHC.Integer.Type.Integer)
        -- fun_co_fn      
        (WpHole) 
        -- bind_fvs
        {!NameSet placeholder here!}
        --fun_tick
        (Nothing)
        ))]}))
]}
