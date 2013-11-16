import Regulate (regexpParser, compileEnfaToDfa, accept)
import Text.ParserCombinators.Parsec (parse)
import Data.Maybe (isJust)

main :: IO ()
main = do
    regex <- getLine
    case parse regexpParser "regex" regex of
        Right enfa -> getContents >>= (mapM_ (print . isJust . accept dfa) . lines)
            where dfa = compileEnfaToDfa enfa
        Left err -> print err
