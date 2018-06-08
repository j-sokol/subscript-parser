Subs
====

To run Subs,

  stack exec runhaskell -- -W Subs.hs [<option>] <path>

<option> can be -i for interpreter, or -p for parser.

The default (no option) is to parse *and* interpret.

The code organization allows you to test the parser (or interpreter) without
exposing the parser (or interpreter) internals. Just write your tests in
Parser/Tests.hs (and Interpreter/Tests.hs).

You can then hop into a GHC interpreter to run the parser tests as follows:

  stack exec ghci -- -W Parser/Tests.hs
