import Regulate (lexerParser, compileLexer, lexerTokenize)
import Text.ParserCombinators.Parsec (parse)
import System.Environment
import Control.Monad (liftM)

main :: IO ()
main = do
    lexfile <- liftM head getArgs >>= readFile
    case liftM compileLexer (parse lexerParser "lexer" lexfile) of
        Right lexer -> getContents >>= (mapM_ (print . lexerTokenize lexer) . lines)
        Left err -> print err
