module AnnsSample where

type Foo = (String, Int)

getInt :: (String, Int) -> Int
getInt (_, i) = i

getInt2 :: Foo -> Int
getInt2 t = snd t

{-


=== Type of getInt annotations ===

Key: AnnKey testing/AnnsSample.hs:5:1-6 CN "Unqual"
Ann: (Ann (DP (0,0)) [] [] [((G AnnVal),DP (0,0))] Nothing Nothing)
This is just for the name
=================================
Key: AnnKey testing/AnnsSample.hs:5:1-30 CN "TypeSig"
Ann: (Ann (DP (2,0)) [] [] [((G AnnDcolon),DP (0,1))] Nothing Nothing)
=================================
Key: AnnKey testing/AnnsSample.hs:5:11-23 CN "HsTupleTy"
Ann: (Ann (DP (0,0)) [] [] [((G AnnOpenP),DP (0,0)),((G AnnCloseP),DP (0,0))] Nothing Nothing)
=================================
Key: AnnKey testing/AnnsSample.hs:5:11-30 CN "HsForAllTy"
Ann: (Ann (DP (0,1)) [] [] [] Nothing Nothing)
=================================
Key: AnnKey testing/AnnsSample.hs:5:11-30 CN "HsFunTy"
Ann: (Ann (DP (0,0)) [] [] [((G AnnRarrow),DP (0,1))] Nothing Nothing)
=================================
Key: AnnKey testing/AnnsSample.hs:5:12-17 CN "HsTyVar"
Ann: (Ann (DP (0,0)) [] [] [((G AnnVal),DP (0,0)),((G AnnComma),DP (0,0))] Nothing Nothing)
=================================
Key: AnnKey testing/AnnsSample.hs:5:20-22 CN "HsTyVar"
Ann: (Ann (DP (0,1)) [] [] [((G AnnVal),DP (0,0))] Nothing Nothing)
=================================
Key: AnnKey testing/AnnsSample.hs:5:28-30 CN "HsTyVar"
Ann: (Ann (DP (0,1)) [] [] [((G AnnVal),DP (0,0))] Nothing Nothing)
=================================

=== Type of getInt2 annotations ===

Key: AnnKey testing/AnnsSample.hs:8:1-7 CN "Unqual"
Ann: (Ann (DP (0,0)) [] [] [((G AnnVal),DP (0,0))] Nothing Nothing)
=================================
Key: AnnKey testing/AnnsSample.hs:8:1-21 CN "TypeSig"
Ann: (Ann (DP (2,0)) [] [] [((G AnnDcolon),DP (0,1))] Nothing Nothing)
=================================
Key: AnnKey testing/AnnsSample.hs:8:12-14 CN "HsTyVar"
Ann: (Ann (DP (0,0)) [] [] [((G AnnVal),DP (0,0))] Nothing Nothing)
=================================
Key: AnnKey testing/AnnsSample.hs:8:12-21 CN "HsForAllTy"
Ann: (Ann (DP (0,1)) [] [] [] Nothing Nothing)
=================================
Key: AnnKey testing/AnnsSample.hs:8:12-21 CN "HsFunTy"
Ann: (Ann (DP (0,0)) [] [] [((G AnnRarrow),DP (0,1))] Nothing Nothing)
=================================
Key: AnnKey testing/AnnsSample.hs:8:19-21 CN "HsTyVar"
Ann: (Ann (DP (0,1)) [] [] [((G AnnVal),DP (0,0))] Nothing Nothing)
=================================


=== Definition of getInt annotations ===
Key: AnnKey testing/AnnsSample.hs:6:1-6 CN "Unqual"
Ann: (Ann (DP (0,0)) [] [] [((G AnnVal),DP (0,0))] Nothing Nothing)
=================================
Key: AnnKey testing/AnnsSample.hs:6:1-17 CN "FunBind"
Ann: (Ann (DP (1,0)) [] [] [] Nothing Nothing)
=================================
Key: AnnKey testing/AnnsSample.hs:6:1-17 CN "Match"
Ann: (Ann (DP (0,0)) [] [] [((G AnnEqual),DP (0,1))] Nothing Nothing)
=================================
Key: AnnKey testing/AnnsSample.hs:6:8-13 CN "TuplePat"
Ann: (Ann (DP (0,1)) [] [] [((G AnnOpenP),DP (0,0)),((G AnnCloseP),DP (0,0))] Nothing Nothing)
=================================
Key: AnnKey testing/AnnsSample.hs:6:9 CN "WildPat"
Ann: (Ann (DP (0,0)) [] [] [((G AnnVal),DP (0,0)),((G AnnComma),DP (0,0))] Nothing Nothing)
=================================
Key: AnnKey testing/AnnsSample.hs:6:12 CN "VarPat"
Ann: (Ann (DP (0,1)) [] [] [((G AnnVal),DP (0,0))] Nothing Nothing)
=================================
Key: AnnKey testing/AnnsSample.hs:6:15-17 CN "GRHS"
Ann: (Ann (DP (0,-1)) [] [] [] Nothing Nothing)
=================================
Key: AnnKey testing/AnnsSample.hs:6:17 CN "HsVar"
Ann: (Ann (DP (0,1)) [] [] [((G AnnVal),DP (0,0))] Nothing Nothing)
=================================

=== Definition of getInt2 annotations ===

Key: AnnKey testing/AnnsSample.hs:9:1-7 CN "Unqual"
Ann: (Ann (DP (0,0)) [] [] [((G AnnVal),DP (0,0))] Nothing Nothing)
=================================
Key: AnnKey testing/AnnsSample.hs:9:1-17 CN "FunBind"
Ann: (Ann (DP (1,0)) [] [] [] Nothing Nothing)
=================================
Key: AnnKey testing/AnnsSample.hs:9:1-17 CN "Match"
Ann: (Ann (DP (0,0)) [] [] [((G AnnEqual),DP (0,1))] Nothing Nothing)
=================================
Key: AnnKey testing/AnnsSample.hs:9:9 CN "VarPat"
Ann: (Ann (DP (0,1)) [] [] [((G AnnVal),DP (0,0))] Nothing Nothing)
=================================
Key: AnnKey testing/AnnsSample.hs:9:11-17 CN "GRHS"
Ann: (Ann (DP (0,-1)) [] [] [] Nothing Nothing)
=================================
Key: AnnKey testing/AnnsSample.hs:9:13-15 CN "HsVar"
Ann: (Ann (DP (0,0)) [] [] [((G AnnVal),DP (0,0))] Nothing Nothing)
=================================
Key: AnnKey testing/AnnsSample.hs:9:13-17 CN "HsApp"
Ann: (Ann (DP (0,1)) [] [] [] Nothing Nothing)
=================================
Key: AnnKey testing/AnnsSample.hs:9:17 CN "HsVar"
Ann: (Ann (DP (0,1)) [] [] [((G AnnVal),DP (0,0))] Nothing Nothing)
=================================
-}

{- Key: AnnKey testing/AnnsSample.hs:1:1 CN "HsModule"
Ann: (Ann (DP (0,0)) [] [] [((G AnnModule),DP (0,0)),((G AnnVal),DP (0,1)),((G AnnWhere),DP (0,1)),((G AnnEofPos),DP (1,0))] Nothing Nothing)
=================================

=== Annotations for Foo type synonym ===

Key: AnnKey testing/AnnsSample.hs:3:1-24 CN "SynDecl"
Ann: (Ann (DP (2,0)) [] [] [((G AnnType),DP (0,0)),((G AnnEqual),DP (0,1))] Just [testing/AnnsSample.hs:3:6-8] Nothing)
=================================
Key: AnnKey testing/AnnsSample.hs:3:6-8 CN "Unqual"
Ann: (Ann (DP (0,1)) [] [] [((G AnnVal),DP (0,0))] Nothing Nothing)
=================================
Key: AnnKey testing/AnnsSample.hs:3:12-24 CN "HsTupleTy"
Ann: (Ann (DP (0,1)) [] [] [((G AnnOpenP),DP (0,0)),((G AnnCloseP),DP (0,0))] Nothing Nothing)
=================================
Key: AnnKey testing/AnnsSample.hs:3:13-18 CN "HsTyVar"
Ann: (Ann (DP (0,0)) [] [] [((G AnnVal),DP (0,0)),((G AnnComma),DP (0,0))] Nothing Nothing)
=================================
Key: AnnKey testing/AnnsSample.hs:3:21-23 CN "HsTyVar"
Ann: (Ann (DP (0,1)) [] [] [((G AnnVal),DP (0,0))] Nothing Nothing)
=================================
-}
