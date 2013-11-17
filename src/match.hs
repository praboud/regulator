import Regulate (compileRegex, accept)
import Data.Maybe (isJust)

main :: IO ()
main = do
    regex <- getLine
    case compileRegex regex of
        Right dfa -> getContents >>= (mapM_ (print . isJust . accept dfa) . lines)
        Left err -> putStrLn err
