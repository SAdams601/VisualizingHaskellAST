
  (L {testing/test.hs:1:1} 
   (HsModule 
    -- hsmodName :: Maybe (Located ModuleName)
    (Just 
     (L {testing/test.hs:1:8-11} {ModuleName: Test})) 
    -- hsmodExports :: Maybe [LIE name]
    (Nothing) 
    -- hsmodImports :: [LImportDecl name]
    [] 
    -- hsmodDecls :: [LHsDecl name]
    [
     (L {testing/test.hs:3:1-13} 
      -- ValD (HsBind id) - constructor for HsDecl 
      (ValD 
       (FunBind 
        -- fun_id :: Located idL
        (L {testing/test.hs:3:1} 
         (Unqual {OccName: f})) 
        -- fun_infix :: Bool - if true this is an infix declaration
        (False)
        -- fun_matches :: MatchGroup idR - the payload 
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
        -- fun_co_fn :: HsWrapper
        (WpHole)
        -- bind_fvs :: NameSet 
        {!NameSet placeholder here!}
        -- fun_tick :: Maybe (Tickish Id) 
        (Nothing))))]
    -- hsmodDeprecMessage :: Maybe WarningTxt     
    (Nothing)
    -- hsmodHaddockModHeader :: Maybe LHsDocString 
    (Nothing)))
===================END OF MODULE: Test=================
