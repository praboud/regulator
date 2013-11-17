module Regulate
    ( parseRegex
    , compileRegex
    , accept
    , tokenize
    ) where

import Regulate.Types
import Regulate.Parse
import Regulate.Dfa
-- import Regulate.Util
-- import Regulate.Lexer

compileRegex :: String -> Either String DFA
compileRegex s = case parseRegex s of
    Right enfa -> Right $ compileEnfaToDfa enfa
    Left err -> Left $ show err
