module ArgsParser where
import GHC.SYB.Utils
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Control.Applicative hiding ((<|>), many)
import qualified Text.ParserCombinators.Parsec.Token as Token

data Mode = AST | Anns
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
parseArgs = Args <$> parseStage <*> (spaces *> parseMode) <*> (spaces *> parseFiles)

parseStage :: CharParser () Stage
parseStage = string "--stage" *> spaces *> char '=' *> spaces *> (parseRename <|> parseParser <|> parseTypecheck)
  where parseRename = Renamer <$ string "renamer"
        parseParser = Parser <$ string "parser"
        parseTypecheck = TypeChecker <$ string "typechecker"
        
parseMode :: CharParser () Mode
parseMode = string "--mode" *> spaces *> char '=' *> spaces *> (parseAstMd <|> parseAnnsMd)
  where parseAstMd = AST <$ string "ast"
        parseAnnsMd = Anns <$ string "anns"

-- I'm pretty sure this isn't quite correct because you could technically have escaped whitespace characters in file paths other than just the single space but what sort of monster does that! 
parseFiles :: CharParser () [FilePath]
parseFiles = onePath `sepBy` spaces
  where onePath = concat <$> many1
          (try (string "\\ ")
           <|> try (string "\\\"")
           <|> try (string "\\\'")
           <|> many1 (noneOf "\0\b\f\n\r\t\v "))
