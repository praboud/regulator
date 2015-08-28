{-# LANGUAGE FlexibleInstances #-}

module Regulate.Types where

import Data.Map (Map)
import Data.Set (Set)
import Data.Array (Array)

type State = Int
type Symbol = Char

-- an epsilon non-deterministic finite automaton (ENFA)
-- represented by:
-- 1) an ENFAMap; a set of transitions from a state to 0 or more other states
--    which can be taken by consuming a specific character c (Just c), or no
--    character (Nothing)
-- 3) the starting state
-- 2) the set of accepting states
data ENFA = ENFA ENFAMap State (Set State) deriving Show

-- this representation is equivalent to: Map (State, Maybe Symbol) (Set State)
-- TODO: it may be worth switching to the single-map implementation
type ENFAMap = Map State (Map (Maybe Symbol) (Set State))

-- a deterministic finite automaton (DFA)
-- the transitions of the DFA are encoded as an array
-- if the array, indexed at some state q and some symbol c, has the value Just p,
-- then there is a transition from q to p via c.
-- If the value is Nothing, then there is no transition (formally, there is a
-- transition to an error state)
-- There is also a start state, and a set of accept states.
data DFA = DFA (Array (State, Symbol) (Maybe State)) State (Set State) deriving Show

-- a single token matched by some regex
-- parametric in x, which is the type of the DFA state that the token was
-- accepted in
data Token x = Token x [Symbol]

instance Show t => Show (Token t) where
    show (Token name lexeme) = show name ++ ": '" ++ lexeme ++ "'"

-- lexer types: assign a label to a particular accept state
type LexerENFA label = [(ENFA, label)]

data LexerDFA label = LexerDFA DFA (Map State label)
