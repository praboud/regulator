import Reg (lexerParser, compileLexer)
import Text.ParserCombinators.Parsec (parse)

main = do
    contents <- getContents
    case parse lexerParser "lexer" contents of
        Right lexer -> print $ compileLexer lexer
        Left err -> print err
