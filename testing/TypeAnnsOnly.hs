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




This annotation is for the comma 
Key: AnnKey testing/AnnsSample.hs:5:12-17 CN "HsTyVar"
Ann: (Ann (DP (0,0)) [] [] [((G AnnVal),DP (0,0)),((G AnnComma),DP (0,0))] Nothing Nothing)



=================================
This is the space before the int keyword 
Key: AnnKey testing/AnnsSample.hs:5:20-22 CN "HsTyVar"
Ann: (Ann (DP (0,1)) [] [] [((G AnnVal),DP (0,0))] Nothing Nothing)
=================================
This is for int in the return type
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
-}
