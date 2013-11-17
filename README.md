Summary
-------

Regulator is a set of tools for converting regular expressions into
deterministic finite automata (DFAs).
DFAs are an efficient (O(n)) way of matching regular
expressions against input.

There are a few tools provided:

- ``genLex`` will read in a lexer definition file from the standard input,
  and write out a series of C++ definitions for the corresponding DFA,
  which include transitions, an enum of possible output types, string
  representations of the enum values, and an array to map DFA accept states
  to lexer token types. Note that if a string could be interpretted as two
  different tokens, you will need to edit the definitions manually to
  indicate which token type takes precedence. (A token type might be
  ``FOO_OR_BAZ`` - you should edit it to be ``FOO``.)
  Check out example.lex to see what a lex file should look like. Note that
  special characters need to be escaped.
- ``testLex`` takes the name of a lexer definition file as its argument,
  then reads the standard input for lines to tokenize accorning to the
  definition provided. As suggested by the name, this is primarily for testing.
- ``match`` is a yet more primitive tool. The first line of input is
  interpretted as a regex, then subsequent lines are matched against it, and
  it is output whether or not they matched.

Building
--------

It is on my todo list to get around to setting up a cabal build script.
Until then, simply invoke ``ghc genLex`` or similar to build the individual
tools. If you haven't guessed already, you will need GHC to build this project
as well as have the amazing Parsec parsing library installed.

Regex Syntax
------------

The syntax is totally non-standard, albeit *similar* to PCRE syntax.

Basic syntax is as follows.

- Apart from the special characters mentioned below, characters create patterns
  that will only match themselves. Duh.
- To group a part of the expression together, wrap it in parentheses. Otherwise,
  each character is treated as a group.
- ``*`` repeats a group 0 or more times.
- ``+`` repeats a group 1 or more times.
- ``?`` make a group optional
- Seperate parts of a pattern with ``|`` to allow the either part to be matched.
- Use ``[]`` to create character classes - just shorthand for a|b|c|d ...
- Better yet, use the built-in ranges ``\w``, ``\s``, ``\d``, ``\n``, or ``\t``
  for word, whitespace, digit, newline or tab characters respectively.

Example: ``(foo|baz)?[abc]e*``

General Documentation
---------------------

The objective is to generate DFA's from regexes (which can then be
used to quickly match regexes in linear time.
In particular, we want to automate the process of creating DFA's for
the lexers of compilers (such as the one I had to write for the
rather excellent CS241 course at the university of waterloo\*).
To this end, we want to read in a list of regular expressions which
match to named tokens, and generate an array representing the dfa,
and the lookup table converting valid accept states to each
particular token. the output format should be a pair of c/c++ array
initializers.

\* As a side note, please do not use this to generate a compiler for
CS241, or any similar course. You will likely get in trouble
when you cannot show you authored the code that generated the DFA.

Notes
-----

DFA:  Deterministic Finite Automata

At each state, each character either transitions to exactly 1
state, or goes to an error state.
very efficient and simple for the machine to verify whether a
string is accepted by the DFA. However, writing code that
stitches together DFA's is prohibitively complicated.
ENFA's are used for this purpose.

ENFA: Episilon Non-Deterministic Finite Automata

At each state, each character can transition to 0 or more states.

(Implicitly, if the character does not transition to any states
it is said to transition to an error state. In the map, this
can be represented by the character transitioning to an empty
set of states, or the character having no transitions at all. In
practice, we should only ever have the second case.)

A state can also transition to 0 or more states on the epsilon transition,
meaning a transition that does not consume a character. Repeated epsilon
transitions may be taken.
(Epsilon transitions are internally represented by a transition through
``Nothing``.)
ENFA's are pretty simple to chain together (the main operations
being alternation, repetition and concatenation). Therefore,
we use ENFA's as an intermediate form between regular expressions
and DFA's.
