VisualizingHaskellAST
=====================

A project to allow you to the what the GHC AST looks like as described by showData from the ghc-syb-utils package ([Hackage](http://hackage.haskell.org/package/ghc-syb-utils)).

How to Run
----------

This script takes in a compiler stage keyword and then the files that are a part of the module graph you want to visualize. There are three compiler stages that are currently supported "parsed", "typed", and "desugared."  

When run from the top directory this will output the desugared AST for the A and B modules.

```bash
cabal run -- desugared testing/A.hs testing/B.hs
```

Sample Output
-------------
The file *testing/sampleShowData.hs* contains sample output from the program running on A.hs and B.hs. It has the .hs file extension but is not an actual haskell file. The output is *similar enough* to Haskell code for syntax highlighting to help make the output much more readable.

