import Regulate (lexerParse, compileLexer, lexerTokenize)
import System.Environment
import Control.Monad (liftM)

main :: IO ()
main = do
    lexfile <- liftM head getArgs >>= readFile
    case liftM compileLexer (lexerParse lexfile) of
        Right lexer -> getContents >>= (mapM_ (print . lexerTokenize lexer) . lines)
        Left err -> print err
