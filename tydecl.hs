                      ((,,,) 
                       (HsGroup 
                        (ValBindsOut 
                         [
                          ((,) 
                           (NonRecursive) {Bag(Located (HsBind Name)): 
                           [
                            (L {testing/types.hs:14:1-16} 
                             (FunBind 
                              (L {testing/types.hs:14:1-6} {Name: Types.getStr}) 
                              (False) 
                              (MatchGroup 
                               [
                                (L {testing/types.hs:14:1-16} 
                                 (Match 
                                  [
                                   (L {testing/types.hs:14:8-12} 
                                    (TuplePat 
                                     [
                                      (L {testing/types.hs:14:9} 
                                       (VarPat {Name: s})),
                                      (L {testing/types.hs:14:11} 
                                       (WildPat {!type placeholder here?!}))] 
                                     (Boxed) {!type placeholder here?!}))] 
                                  (Nothing) 
                                  (GRHSs 
                                   [
                                    (L {testing/types.hs:14:16} 
                                     (GRHS 
                                      [] 
                                      (L {testing/types.hs:14:16} 
                                       (HsVar {Name: s}))))] 
                                   (EmptyLocalBinds))))] {!type placeholder here?!}) 
                              (WpHole) {NameSet: 
                              []} 
                              (Nothing)))]}),
                          ((,) 
                           (NonRecursive) {Bag(Located (HsBind Name)): 
                           [
                            (L {testing/types.hs:11:1-20} 
                             (FunBind 
                              (L {testing/types.hs:11:1-9} {Name: Types.getString}) 
                              (False) 
                              (MatchGroup 
                               [
                                (L {testing/types.hs:11:1-20} 
                                 (Match 
                                  [
                                   (L {testing/types.hs:11:11} 
                                    (VarPat {Name: i}))] 
                                  (Nothing) 
                                  (GRHSs 
                                   [
                                    (L {testing/types.hs:11:15-20} 
                                     (GRHS 
                                      [] 
                                      (L {testing/types.hs:11:15-20} 
                                       (HsApp 
                                        (L {testing/types.hs:11:15-18} 
                                         (HsVar {Name: GHC.Show.show})) 
                                        (L {testing/types.hs:11:20} 
                                         (HsVar {Name: i}))))))] 
                                   (EmptyLocalBinds))))] {!type placeholder here?!}) 
                              (WpHole) {NameSet: 
                              []} 
                              (Nothing)))]}),
                          ((,) 
                           (NonRecursive) {Bag(Located (HsBind Name)): 
                           [
                            (L {testing/types.hs:8:1-16} 
                             (FunBind 
                              (L {testing/types.hs:8:1-6} {Name: Types.getInt}) 
                              (False) 
                              (MatchGroup 
                               [
                                (L {testing/types.hs:8:1-16} 
                                 (Match 
                                  [
                                   (L {testing/types.hs:8:8-12} 
                                    (TuplePat 
                                     [
                                      (L {testing/types.hs:8:9} 
                                       (WildPat {!type placeholder here?!})),
                                      (L {testing/types.hs:8:11} 
                                       (VarPat {Name: i}))] 
                                     (Boxed) {!type placeholder here?!}))] 
                                  (Nothing) 
                                  (GRHSs 
                                   [
                                    (L {testing/types.hs:8:16} 
                                     (GRHS 
                                      [] 
                                      (L {testing/types.hs:8:16} 
                                       (HsVar {Name: i}))))] 
                                   (EmptyLocalBinds))))] {!type placeholder here?!}) 
                              (WpHole) {NameSet: 
                              []} 
                              (Nothing)))]})] 
                         [
                          (L {testing/types.hs:13:1-33} 
                           (TypeSig 
                            [
                             (L {testing/types.hs:13:1-6} {Name: Types.getStr})] 
                            (L {testing/types.hs:13:11-33} 
                             (HsFunTy 
                              (L {testing/types.hs:13:11-23} 
                               (HsTupleTy 
                                (HsBoxedOrConstraintTuple) 
                                [
                                 (L {testing/types.hs:13:12-17} 
                                  (HsTyVar {Name: GHC.Base.String})),
                                 (L {testing/types.hs:13:20-22} 
                                  (HsTyVar {Name: GHC.Types.Int}))])) 
                              (L {testing/types.hs:13:28-33} 
                               (HsTyVar {Name: GHC.Base.String})))))),
                          (L {testing/types.hs:10:1-36} 
                           (TypeSig 
                            [
                             (L {testing/types.hs:10:1-9} {Name: Types.getString})] 
                            (L {testing/types.hs:10:14-36} 
                             (HsForAllTy 
                              (Implicit) 
                              (HsQTvs 
                               [] 
                               [
                                (L {testing/types.hs:10:14-36} 
                                 (UserTyVar {Name: a}))]) 
                              (L {testing/types.hs:10:14-21} 
                               [
                                (L {testing/types.hs:10:14-21} 
                                 (HsParTy 
                                  (L {testing/types.hs:10:15-20} 
                                   (HsAppTy 
                                    (L {testing/types.hs:10:15-18} 
                                     (HsTyVar {Name: GHC.Show.Show})) 
                                    (L {testing/types.hs:10:20} 
                                     (HsTyVar {Name: a}))))))]) 
                              (L {testing/types.hs:10:26-36} 
                               (HsFunTy 
                                (L {testing/types.hs:10:26} 
                                 (HsTyVar {Name: a})) 
                                (L {testing/types.hs:10:31-36} 
                                 (HsTyVar {Name: GHC.Base.String})))))))),
                          (L {testing/types.hs:7:1-20} 
                           (TypeSig 
                            [
                             (L {testing/types.hs:7:1-6} {Name: Types.getInt})] 
                            (L {testing/types.hs:7:11-20} 
                             (HsFunTy 
                              (L {testing/types.hs:7:11-13} 
                               (HsTyVar {Name: Types.Foo})) 
                              (L {testing/types.hs:7:18-20} 
                               (HsTyVar {Name: GHC.Types.Int}))))))])
                        ------------------End VAL BINDs -------------------
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
                            [{Name: GHC.Types.Int},{Name: GHC.Base.String}]}))],
                         [
                          (L {testing/types.hs:5:1-24} 
                           (TyDecl 
                            (L {testing/types.hs:5:6-8} {Name: Types.Baz}) 
                            (HsQTvs 
                             [] 
                             []) 
                            (TySynonym 
                             (L {testing/types.hs:5:12-24} 
                              (HsTupleTy 
                               (HsBoxedOrConstraintTuple) 
                               [
                                (L {testing/types.hs:5:13-15} 
                                 (HsTyVar {Name: Types.Foo})),
                                (L {testing/types.hs:5:18-23} 
                                 (HsTyVar {Name: GHC.Base.String}))]))) {NameSet: 
                            [{Name: GHC.Base.String},{Name: Types.Foo}]}))]] 
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
===================END OF MODULE: Types=================
