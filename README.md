VisualizingHaskellAST
=====================

A project to allow you to the what the GHC AST looks like as described by showData from the ghc-syb-utils package ([Hackage](http://hackage.haskell.org/package/ghc-syb-utils)).

How to Run
----------

The script is expecting a list of strings that indicate what files to load into the module graph. You have to fully escape the strings if you are running through cabal, an example of the fully escaped string is provided below. When run from the top directory this will output the AST for the A and B modules.

```bash
cabal run -- "[\"testing/A.hs\",\"testing/B.hs\"]"

