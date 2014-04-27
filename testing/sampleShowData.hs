Preprocessing executable 'VisualizingHaskellAST' for
VisualizingHaskellAST-0.1.0.0...
{Bag(Located (HsBind Var)): 
  [
   (L {testing/A.hs:6:4-14} 
    (AbsBinds 
     [{Var: a}] 
     [{Var: $dNum}] 
     [
      (ABE {Var: A.b} {Var: b} 
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
      (L {testing/A.hs:6:4-14} 
       (FunBind 
        (L {testing/A.hs:6:4} {Var: b}) 
        (False) 
        (MatchGroup 
         [
          (L {testing/A.hs:6:4-14} 
           (Match 
            [
             (L {testing/A.hs:6:6} 
              (VarPat {Var: x}))] 
            (Nothing) 
            (GRHSs 
             [
              (L {testing/A.hs:6:10-14} 
               (GRHS 
                [] 
                (L {testing/A.hs:6:10-14} 
                 (OpApp 
                  (L {testing/A.hs:6:10} 
                   (HsVar {Var: x})) 
                  (L {testing/A.hs:6:12} 
                   (HsWrap 
                    (WpCompose 
                     (WpEvApp 
                      (EvId {Var: $dNum})) 
                     (WpTyApp a)) 
                    (HsVar {Var: GHC.Num.+}))) {Fixity: infixl 6} 
                  (L {testing/A.hs:6:14} 
                   (HsOverLit 
                    (OverLit 
                     (HsIntegral 
                      (5)) 
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
                         (5) GHC.Integer.Type.Integer)))) a)))))))] 
             (EmptyLocalBinds))))] a -> a) 
        (WpHole) {!NameSet placeholder here!} 
        (Nothing)))]})),
   (L {testing/A.hs:4:4-43} 
    (AbsBinds 
     [] 
     [] 
     [
      (ABE {Var: A.main} {Var: main} 
       (WpHole) 
       (SpecPrags 
        []))] 
     ({abstract:TcEvBinds}) {Bag(Located (HsBind Var)): 
     [
      (L {testing/A.hs:4:4-43} 
       (FunBind 
        (L {testing/A.hs:4:4-7} {Var: main}) 
        (False) 
        (MatchGroup 
         [
          (L {testing/A.hs:4:4-43} 
           (Match 
            [] 
            (Nothing) 
            (GRHSs 
             [
              (L {testing/A.hs:4:11-43} 
               (GRHS 
                [] 
                (L {testing/A.hs:4:11-43} 
                 (HsApp 
                  (L {testing/A.hs:4:11-15} 
                   (HsWrap 
                    (WpCompose 
                     (WpEvApp 
                      (EvId {Var: $dShow})) 
                     (WpTyApp [GHC.Types.Char])) 
                    (HsVar {Var: System.IO.print}))) 
                  (L {testing/A.hs:4:17-43} 
                   (HsPar 
                    (L {testing/A.hs:4:18-42} 
                     (OpApp 
                      (L {testing/A.hs:4:18-27} 
                       (HsLit 
                        (HsString {FastString: "Result: "}))) 
                      (L {testing/A.hs:4:29-30} 
                       (HsWrap 
                        (WpTyApp GHC.Types.Char) 
                        (HsVar {Var: GHC.Base.++}))) {Fixity: infixr 5} 
                      (L {testing/A.hs:4:32-42} 
                       (HsPar 
                        (L {testing/A.hs:4:33-41} 
                         (HsApp 
                          (L {testing/A.hs:4:33-36} 
                           (HsWrap 
                            (WpCompose 
                             (WpEvApp 
                              (EvId {Var: $dShow})) 
                             (WpTyApp GHC.Integer.Type.Integer)) 
                            (HsVar {Var: GHC.Show.show}))) 
                          (L {testing/A.hs:4:37-41} 
                           (HsPar 
                            (L {testing/A.hs:4:38-40} 
                             (HsApp 
                              (L {testing/A.hs:4:38} 
                               (HsWrap 
                                (WpCompose 
                                 (WpEvApp 
                                  (EvId {Var: $dNum})) 
                                 (WpTyApp GHC.Integer.Type.Integer)) 
                                (HsVar {Var: A.b}))) 
                              (L {testing/A.hs:4:40} 
                               (HsOverLit 
                                (OverLit 
                                 (HsIntegral 
                                  (5)) 
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
                                     (5) GHC.Integer.Type.Integer)))) GHC.Integer.Type.Integer)))))))))))))))))))] 
             (EmptyLocalBinds))))] GHC.Types.IO ()) 
        (WpHole) {!NameSet placeholder here!} 
        (Nothing)))]}))]}
===================END OF MODULE: A=================
{Bag(Located (HsBind Var)): 
  [
   (L {testing/B.hs:3:4-21} 
    (AbsBinds 
     [{Var: a}] 
     [{Var: $dNum}] 
     [
      (ABE {Var: B.plusFive} {Var: plusFive} 
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
      (L {testing/B.hs:3:4-21} 
       (FunBind 
        (L {testing/B.hs:3:4-11} {Var: plusFive}) 
        (False) 
        (MatchGroup 
         [
          (L {testing/B.hs:3:4-21} 
           (Match 
            [
             (L {testing/B.hs:3:13} 
              (VarPat {Var: x}))] 
            (Nothing) 
            (GRHSs 
             [
              (L {testing/B.hs:3:17-21} 
               (GRHS 
                [] 
                (L {testing/B.hs:3:17-21} 
                 (OpApp 
                  (L {testing/B.hs:3:17} 
                   (HsVar {Var: x})) 
                  (L {testing/B.hs:3:19} 
                   (HsWrap 
                    (WpCompose 
                     (WpEvApp 
                      (EvId {Var: $dNum})) 
                     (WpTyApp a)) 
                    (HsVar {Var: GHC.Num.+}))) {Fixity: infixl 6} 
                  (L {testing/B.hs:3:21} 
                   (HsOverLit 
                    (OverLit 
                     (HsIntegral 
                      (5)) 
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
                         (5) GHC.Integer.Type.Integer)))) a)))))))] 
             (EmptyLocalBinds))))] a -> a) 
        (WpHole) {!NameSet placeholder here!} 
        (Nothing)))]}))]}
===================END OF MODULE: B=================
