import Reg (lexerParser, compileLexer, LexerDFA(LexerDFA), acceptExtra)
import Text.ParserCombinators.Parsec (parse)
import System.Environment
import Control.Monad (liftM, (>=>))
import qualified Data.Map as Map

main = do
    lexfile <- liftM head getArgs >>= readFile
    case liftM compileLexer (parse lexerParser "lexer" lexfile) of
        Right (LexerDFA dfa as) -> getContents >>= (mapM_ (print . (acceptExtra dfa >=> flip Map.lookup as)) . lines)
        Left err -> print err
