
  (L {testing/test.hs:1:1} 
   (HsModule 
    (Just 
     (L {testing/test.hs:1:8-11} {ModuleName: Test})) 
    (Nothing) 
    [] 
    [
     (L {testing/test.hs:3:1-13} 
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
        (Nothing))))] 
    (Nothing) 
    (Nothing)))
===================END OF MODULE: Test=================
