import Reg (lexerParser, compileLexer, LexerDFA(LexerDFA), DFA(DFA))
import Text.ParserCombinators.Parsec (parse)
import Data.List (intercalate, nub)
import Text.Printf (printf)
import Data.Ix (Ix, range, inRange)
import Data.Array ((!), bounds)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Map as Map
import Data.Char (chr)

toCpp :: LexerDFA -> String
toCpp (LexerDFA (DFA ts q0 _) as) = header ++ def ++ q0' ++ "\n" ++ ts' ++ "\n" ++ enum ++ "\n" ++ enum_to_string ++ "\n" ++ as' ++ footer
    where
    err = -1 :: Int
    header = "/* BEGIN GENERATED CODE */\n"
    def = printf "#define ST_ERR %d\n" err
    q0' = printf "#define ST_START %d\n" q0

    ts' = printf "int transitions[][%d] = {\n" char_count ++ intercalate ",\n" tlines ++ "};\n"
    tlines = [('{':) $ (++"}") $ intercalate ", " [gettr s c | c <- range (min_char, max_char)] | s <- range (min_st, max_st)]
    gettr s c = show $ fromMaybe err $ if inRange (bounds ts) (s, c) then ts ! (s, c) else Nothing

    as' = "enum type state_to_type[] = {\n" ++ unlines alines ++ "};\n"
    alines = ["TP_" ++ fromJust (Map.lookup i as) ++ ", " | i <- range (min_st, max_st)]

    enum_to_string = "string type_to_string[] = {\n" ++ unlines slines ++ "};\n";
    slines = ['"' : n ++ "\"," | n <- nub $ Map.elems as]

    enum = "enum type {\n" ++ unlines elines ++ "};\n";
    elines = ["TP_" ++ n ++ "," | n <- nub $ Map.elems as]

    footer = "/* END GENERATED CODE */"

    char_count = 256 :: Int
    ((min_st, _), (max_st, _)) = bounds ts
    min_char = chr 0
    max_char = chr 255

main :: IO()
main = do
    contents <- getContents
    case parse lexerParser "lexer" contents of
        Right lexer -> print $ toCpp $ compileLexer lexer
        Left err -> print err
