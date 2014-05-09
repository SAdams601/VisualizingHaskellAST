VisualizingHaskellAST
=====================

A project to allow you to the what the GHC AST looks like as described by showData from the ghc-syb-utils package ([Hackage](http://hackage.haskell.org/package/ghc-syb-utils)).

How to Run
----------

The script is expecting a list of strings that indicate what files to load into the module graph and which compiler stage you want to see. You have to fully escape the strings if you are running through cabal, an example of the fully escaped string is provided below. When run from the top directory this will output the desugared AST for the A and B modules.

```bash
cabal run -- desugared "[\"testing/A.hs\",\"testing/B.hs\"]"
```

The other two keywords are "parsed" and "typed" for output of the parser and typechecker respectively.

Sample Output
-------------
The file *testing/sampleShowData.hs* contains sample output from the program running on A.hs and B.hs. It has the .hs file extension but is not an actual haskell file. The output is *similar enough* to Haskell code for syntax highlighting to help make the output much more readable.

