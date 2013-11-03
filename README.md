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
