import Reg (regexpParser, compileEnfaToDfa, accept)
import Text.ParserCombinators.Parsec (parse)

main = do
    regex <- getLine
    case parse regexpParser "regex" regex of
        Right enfa -> getContents >>= (mapM_ (print . accept dfa) . lines)
            where dfa = compileEnfaToDfa enfa
        Left err -> print err
