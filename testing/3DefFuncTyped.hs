{Bag(Located (HsBind Var)): 
  [
   (L {testing/3DefFunc.hs:(2,4)-(4,16)} 
    (AbsBinds 
     [{Var: a},{Var: a}] 
     [{Var: $dEq},{Var: $dNum},{Var: $dNum}] 
     [
      (ABE {Var: Test.g} {Var: g} 
       (WpCompose 
        (WpCompose 
         (WpCompose 
          (WpCompose 
           (WpTyLam {Var: a}) 
           (WpTyLam {Var: a})) 
          (WpCompose 
           (WpEvLam {Var: $dEq}) 
           (WpCompose 
            (WpEvLam {Var: $dNum}) 
            (WpEvLam {Var: $dNum})))) 
         (WpLet 
          ({abstract:TcEvBinds}))) 
        (WpCompose 
         (WpCompose 
          (WpCompose 
           (WpEvApp 
            (EvId {Var: $dNum})) 
           (WpEvApp 
            (EvId {Var: $dNum}))) 
          (WpEvApp 
           (EvId {Var: $dEq}))) 
         (WpCompose 
          (WpTyApp a) 
          (WpTyApp a)))) 
       (SpecPrags 
        []))] 
     ({abstract:TcEvBinds}) {Bag(Located (HsBind Var)): 
     [
      (L {testing/3DefFunc.hs:(2,4)-(4,16)} 
       (FunBind 
        (L {testing/3DefFunc.hs:2:4} {Var: g}) 
        (False) 
        (MatchGroup 
         [
          (L {testing/3DefFunc.hs:2:4-12} 
           (Match 
            [
             (L {testing/3DefFunc.hs:2:6} 
              (NPat 
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
                    (WpTyApp a)) 
                   (HsVar {Var: GHC.Num.fromInteger}))) 
                 (L {<no location info>} 
                  (HsLit 
                   (HsInteger 
                    (0) GHC.Integer.Type.Integer)))) a) 
               (Nothing) 
               (HsWrap 
                (WpCompose 
                 (WpEvApp 
                  (EvId {Var: $dEq})) 
                 (WpTyApp a)) 
                (HsVar {Var: GHC.Classes.==})))),
             (L {testing/3DefFunc.hs:2:8} 
              (VarPat {Var: y}))] 
            (Nothing) 
            (GRHSs 
             [
              (L {testing/3DefFunc.hs:2:12} 
               (GRHS 
                [] 
                (L {testing/3DefFunc.hs:2:12} 
                 (HsVar {Var: y}))))] 
             (EmptyLocalBinds)))),
          (L {testing/3DefFunc.hs:3:4-16} 
           (Match 
            [
             (L {testing/3DefFunc.hs:3:6} 
              (NPat 
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
                    (WpTyApp a)) 
                   (HsVar {Var: GHC.Num.fromInteger}))) 
                 (L {<no location info>} 
                  (HsLit 
                   (HsInteger 
                    (1) GHC.Integer.Type.Integer)))) a) 
               (Nothing) 
               (HsWrap 
                (WpCompose 
                 (WpEvApp 
                  (EvId {Var: $dEq})) 
                 (WpTyApp a)) 
                (HsVar {Var: GHC.Classes.==})))),
             (L {testing/3DefFunc.hs:3:8} 
              (VarPat {Var: x}))] 
            (Nothing) 
            (GRHSs 
             [
              (L {testing/3DefFunc.hs:3:12-16} 
               (GRHS 
                [] 
                (L {testing/3DefFunc.hs:3:12-16} 
                 (OpApp 
                  (L {testing/3DefFunc.hs:3:12} 
                   (HsVar {Var: x})) 
                  (L {testing/3DefFunc.hs:3:14} 
                   (HsWrap 
                    (WpCompose 
                     (WpEvApp 
                      (EvId {Var: $dNum})) 
                     (WpTyApp a)) 
                    (HsVar {Var: GHC.Num.+}))) {Fixity: infixl 6} 
                  (L {testing/3DefFunc.hs:3:16} 
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
                         (WpTyApp a)) 
                        (HsVar {Var: GHC.Num.fromInteger}))) 
                      (L {<no location info>} 
                       (HsLit 
                        (HsInteger 
                         (1) GHC.Integer.Type.Integer)))) a)))))))] 
             (EmptyLocalBinds)))),
          (L {testing/3DefFunc.hs:4:4-16} 
           (Match 
            [
             (L {testing/3DefFunc.hs:4:6} 
              (WildPat a)),
             (L {testing/3DefFunc.hs:4:8} 
              (VarPat {Var: z}))] 
            (Nothing) 
            (GRHSs 
             [
              (L {testing/3DefFunc.hs:4:12-16} 
               (GRHS 
                [] 
                (L {testing/3DefFunc.hs:4:12-16} 
                 (OpApp 
                  (L {testing/3DefFunc.hs:4:12} 
                   (HsVar {Var: z})) 
                  (L {testing/3DefFunc.hs:4:14} 
                   (HsWrap 
                    (WpCompose 
                     (WpEvApp 
                      (EvId {Var: $dNum})) 
                     (WpTyApp a)) 
                    (HsVar {Var: GHC.Num.-}))) {Fixity: infixl 6} 
                  (L {testing/3DefFunc.hs:4:16} 
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
                         (WpTyApp a)) 
                        (HsVar {Var: GHC.Num.fromInteger}))) 
                      (L {<no location info>} 
                       (HsLit 
                        (HsInteger 
                         (1) GHC.Integer.Type.Integer)))) a)))))))] 
             (EmptyLocalBinds))))] a -> a -> a) 
        (WpHole) {!NameSet placeholder here!} 
        (Nothing)))]}))]}