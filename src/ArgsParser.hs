{-#LANGUAGE TupleSections #-}
module ArgsParser where
import GHC.SYB.Utils
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Control.Applicative hiding ((<|>), many)
import qualified Text.ParserCombinators.Parsec.Token as Token
import Language.Haskell.GHC.ExactPrint.Utils

data Mode = AST | Anns | Both
            deriving (Show, Eq)

data Args = Args {stage :: Stage,
                  mode :: Mode,
                  files :: [FilePath]
                 }
            deriving (Show, Eq)
            
defaultArgs = Args {stage = Parser, mode = AST, files = []}

parser :: String -> Either ParseError Args
parser argStr = parse parseArgs "parseArgs" argStr

parseArgs :: CharParser () Args
parseArgs = uncurry Args <$> parseOptions <*> (spaces *> parseFiles)

parseOptions :: CharParser () (Stage, Mode)
parseOptions =  (try modeOnly) <|> fullDecl <|> return (Parser, AST)
  where modeOnly = (Parser,) <$> (spaces *> parseMode)
        fullDecl = (,) <$> (spaces *> parseStage) <*> (spaces *> parseMode)

parseStage :: CharParser () Stage
parseStage = string "--stage" *> spaces *> char '=' *> spaces *> (parseRename <|> parseParser <|> parseTypecheck)
  where parseRename = Renamer <$ string "renamer"
        parseParser = Parser <$ string "parser"
        parseTypecheck = TypeChecker <$ string "typechecker"
        
parseMode :: CharParser () Mode
parseMode = string "--mode" *> spaces *> char '=' *> spaces *> ((try parseAstMd) <|> parseAnnsMd <|> parseBothMd)
  where parseAstMd = AST <$ string "ast"
        parseAnnsMd = Anns <$ string "anns"
        parseBothMd = Both <$ string "both"

-- I'm pretty sure this isn't quite correct because you could technically have escaped whitespace characters in file paths other than just the single space but what sort of monster does that! 
parseFiles :: CharParser () [FilePath]
parseFiles = onePath `sepBy` spaces
  where onePath = concat <$> many1
          (try (string "\\ ")
           <|> try (string "\\\"")
           <|> try (string "\\\'")
           <|> many1 (noneOf "\0\b\f\n\r\t\v "))
