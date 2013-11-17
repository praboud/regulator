import Regulate.Lexer (compileLexer, lexerTokenize)
import System.Environment
import Control.Monad (liftM)

main :: IO ()
main = do
    lexer <- liftM head getArgs >>= readFile
    case compileLexer lexer of
        Right lexerDfa -> getContents >>= (mapM_ (print . lexerTokenize lexerDfa) . lines)
        Left err -> print err
