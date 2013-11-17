{-# LANGUAGE FlexibleInstances #-}

module Regulate.Types where

import Data.Map (Map)
import Data.Set (Set)
import Data.Array (Array)

type State = Int
type Symbol = Char

data ENFA = ENFA (Map State (Map (Maybe Symbol) (Set State))) State (Set State) deriving Show

type ENFAMap = Map State (Map (Maybe Symbol) (Set State))

data DFA = DFA (Array (State, Symbol) (Maybe State)) State (Set State) deriving Show

type DFAMap = Map (Set State) (Map Symbol (Set State))

data Token x = Token x [Symbol]

type LexerENFA = [(ENFA, String)]

data LexerDFA = LexerDFA DFA (Map State String)

instance Show (Token String) where
    show (Token name lexeme) = name ++ ": '" ++ lexeme ++ "'"
