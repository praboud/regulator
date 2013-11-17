import Regulate (regexParse, compileEnfaToDfa, accept)
import Data.Maybe (isJust)

main :: IO ()
main = do
    regex <- getLine
    case regexParse regex of
        Right enfa -> getContents >>= (mapM_ (print . isJust . accept dfa) . lines)
            where dfa = compileEnfaToDfa enfa
        Left err -> print err
