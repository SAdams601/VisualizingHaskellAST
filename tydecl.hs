                      ((,,,) 
                       (HsGroup 
                        (ValBindsOut 
                         [
                          ((,) 
                           (NonRecursive) {Bag(Located (HsBind Name)): 
                           [
                            (L {testing/types.hs:6:1-12} 
                             (FunBind 
                              (L {testing/types.hs:6:1} {Name: Types.f}) 
                              (False) 
                              (MatchGroup 
                               [
                                (L {testing/types.hs:6:1-12} 
                                 (Match 
                                  [
                                   (L {testing/types.hs:6:3-8} 
                                    (TuplePat 
                                     [
                                      (L {testing/types.hs:6:4} 
                                       (WildPat {!type placeholder here?!})),
                                      (L {testing/types.hs:6:7} 
                                       (VarPat {Name: i}))] 
                                     (Boxed) {!type placeholder here?!}))] 
                                  (Nothing) 
                                  (GRHSs 
                                   [
                                    (L {testing/types.hs:6:12} 
                                     (GRHS 
                                      [] 
                                      (L {testing/types.hs:6:12} 
                                       (HsVar {Name: i}))))] 
                                   (EmptyLocalBinds))))] {!type placeholder here?!}) 
                              (WpHole) {NameSet: 
                              []} 
                              (Nothing)))]})] 
                         [
                          (L {testing/types.hs:5:1-25} 
                           (TypeSig 
                            [
                             (L {testing/types.hs:5:1} {Name: Types.f})] 
                            (L {testing/types.hs:5:6-25} 
                             (HsFunTy 
                              (L {testing/types.hs:5:6-18} 
                               (HsTupleTy 
                                (HsBoxedOrConstraintTuple) 
                                [
                                 (L {testing/types.hs:5:7-12} 
                                  (HsTyVar {Name: GHC.Base.String})),
                                 (L {testing/types.hs:5:15-17} 
                                  (HsTyVar {Name: GHC.Types.Int}))])) 
                              (L {testing/types.hs:5:23-25} 
                               (HsTyVar {Name: GHC.Types.Int}))))))]) 
                        [
                         [
                          (L {testing/types.hs:3:1-24} 
                           (TyDecl 
                            (L {testing/types.hs:3:6-8} {Name: Types.Foo}) 
                            (HsQTvs 
                             [] 
                             []) 
                            (TySynonym 
                             (L {testing/types.hs:3:12-24} 
                              (HsTupleTy 
                               (HsBoxedOrConstraintTuple) 
                               [
                                (L {testing/types.hs:3:13-18} 
                                 (HsTyVar {Name: GHC.Base.String})),
                                (L {testing/types.hs:3:21-23} 
                                 (HsTyVar {Name: GHC.Types.Int}))]))) {NameSet: 
                            [{Name: GHC.Types.Int},{Name: GHC.Base.String}]}))]] 
                        [] 
                        [] 
                        [] 
                        [] 
                        [] 
                        [] 
                        [] 
                        [] 
                        [] 
                        []) 
                       [
                        (L {testing/types.hs:1:8-12} 
                         (ImportDecl 
                          (L {testing/types.hs:1:8-12} {ModuleName: Prelude}) 
                          (Nothing) 
                          (False) 
                          (False) 
                          (False) 
                          (True) 
                          (Nothing) 
                          (Nothing)))] 
                       (Nothing) 
                       (Nothing))
