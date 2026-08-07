When no command is given, mapply applies echo to command-line tokens.

  $ mapply7 one two
  one
  two

When no tokens are given, mapply reads them from standard input.

  $ printf 'one\ntwo\n' | mapply7
  one
  two

An explicit command and its arguments can still be given after "--".

  $ mapply7 one -- echo two three
  one two three

The separator must be followed by a command.

  $ mapply7 one -- 2>&1 | sed -n '1p'
  mapply7: -- must be followed by a command.

  $ mapply7 one -- > /dev/null 2>&1
  [2]

Tokens and command arguments containing spaces remain individual arguments.
Use test to verify mapply is not using more than one argument

  $ mapply7 'argument with spaces.litmus' -- test = 'argument with spaces.litmus'

  $ mapply7 -j 2 'argument with spaces.litmus' -- test = 'argument with spaces.litmus'
