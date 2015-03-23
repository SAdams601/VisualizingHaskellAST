Preprocessing executable 'VisualizingHaskellAST' for
VisualizingHaskellAST-0.1.0.0...
Running VisualizingHaskellAST...

                      ((,,,) 
                       (HsGroup 
                        (ValBindsOut 
                         [] 
                         []) 
                        [
                         [
                          (L {testing/types.hs:7:1-18} 
                           (TyDecl 
                            (L {testing/types.hs:7:6-9} {Name: Types.Name}) 
                            (HsQTvs 
                             [] 
                             []) 
                            (TySynonym 
                             (L {testing/types.hs:7:13-18} 
                              (HsTyVar {Name: GHC.Base.String}))) {NameSet: 
                            [{Name: GHC.Base.String}]}))],
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
