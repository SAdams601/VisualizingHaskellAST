
  (L {testing/test.hs:1:1} 
   (HsModule 
    -- hsmodName :: Maybe (Located ModuleName)
    (Just 
     (L {testing/test.hs:1:8-11} {ModuleName: Test})) 
    -- hsmodExports :: Maybe [LIE name] - when nothing export everything
    (Nothing) 
    -- hsmodImports :: [LImportDecl name] 
    []
    -- hsmodDecls :: [LHsDecl name] "Type, class, value, and interface signature decls"
    [
     (L {testing/test.hs:3:1-13} 
      -- ValD (HsBind is) is a constructor for HsDecl id
      -- Parsed representation of f
      (ValD 
       (FunBind 
        (L {testing/test.hs:3:1} 
         (Unqual {OccName: f})) 
        (False) 
        (MatchGroup 
         [
          (L {testing/test.hs:3:1-13} 
           (Match 
            [
             (L {testing/test.hs:3:3} 
              (VarPat 
               (Unqual {OccName: a}))),
             (L {testing/test.hs:3:5} 
              (VarPat 
               (Unqual {OccName: b})))] 
            (Nothing) 
            (GRHSs 
             [
              (L {testing/test.hs:3:9-13} 
               (GRHS 
                [] 
                (L {testing/test.hs:3:9-13} 
                 (OpApp 
                  (L {testing/test.hs:3:9} 
                   (HsVar 
                    (Unqual {OccName: a}))) 
                  (L {testing/test.hs:3:11} 
                   (HsVar 
                    (Unqual {OccName: +}))) {!fixity placeholder here?!} 
                  (L {testing/test.hs:3:13} 
                   (HsVar 
                    (Unqual {OccName: b})))))))] 
             (EmptyLocalBinds))))] {!type placeholder here?!}) 
        (WpHole) {!NameSet placeholder here!} 
        (Nothing)))),
      -- Parsed representation of g
     (L {testing/test.hs:5:1-13} 
      (ValD 
       (FunBind 
        (L {testing/test.hs:5:1} 
         (Unqual {OccName: g})) 
        (False) 
        (MatchGroup 
         [
          (L {testing/test.hs:5:1-13} 
           (Match 
            [
             (L {testing/test.hs:5:3} 
              (VarPat 
               (Unqual {OccName: a}))),
             (L {testing/test.hs:5:5} 
              (VarPat 
               (Unqual {OccName: b})))] 
            (Nothing) 
            (GRHSs 
             [
              (L {testing/test.hs:5:9-13} 
               (GRHS 
                [] 
                (L {testing/test.hs:5:9-13} 
                 (OpApp 
                  (L {testing/test.hs:5:9} 
                   (HsVar 
                    (Unqual {OccName: a}))) 
                  (L {testing/test.hs:5:11} 
                   (HsVar 
                    (Unqual {OccName: -}))) {!fixity placeholder here?!} 
                  (L {testing/test.hs:5:13} 
                   (HsVar 
                    (Unqual {OccName: b})))))))] 
             (EmptyLocalBinds))))] {!type placeholder here?!}) 
        (WpHole) {!NameSet placeholder here!} 
        (Nothing))))]
    -- hsmodDeprecMessage :: Maybe WarningTxt 
    (Nothing)
    -- hsmodHaddockModHeader :: Maybe LHsDocString
    (Nothing)))
===================END OF MODULE: Test=================
