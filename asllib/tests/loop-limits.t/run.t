Loop limits
===========

While loops:

  $ aslref while-correct.asl

  $ aslref while-incorrect.asl
  1
  2
  3
  4
  5
  File while-incorrect.asl, line 4, character 2 to line 7, character 6:
  ASL Dynamic error: loop limit reached.
  [1]

  $ aslref while-exact.asl

  $ aslref while-exact-minus-one.asl
  File while-exact-minus-one.asl, line 4, character 2 to line 6, character 6:
  ASL Dynamic error: loop limit reached.
  [1]

  $ aslref while-no-limit.asl
  File while-no-limit.asl, line 4, character 2 to line 6, character 6:
  ASL Warning: Loop does not have a limit.

Repeat loops:

  $ aslref repeat-correct.asl

  $ aslref repeat-incorrect.asl
  1
  2
  3
  4
  5
  File repeat-incorrect.asl, line 4, character 2 to line 7, character 30:
  ASL Dynamic error: loop limit reached.
  [1]

  $ aslref repeat-exact.asl

  $ aslref repeat-exact-minus-one.asl
  File repeat-exact-minus-one.asl, line 4, character 2 to line 6, character 30:
  ASL Dynamic error: loop limit reached.
  [1]

  $ aslref repeat-no-limit.asl
  File repeat-no-limit.asl, line 4, character 2 to line 6, character 18:
  ASL Warning: Loop does not have a limit.

Double loops
  $ aslref double-while-correct-correct.asl

  $ aslref double-while-correct-incorrect.asl
  File double-while-correct-incorrect.asl, line 9, character 4 to line 11,
    character 8:
  ASL Dynamic error: loop limit reached.
  [1]

  $ aslref double-while-incorrect-correct.asl
  File double-while-incorrect-correct.asl, line 6, character 2 to line 12,
    character 6:
  ASL Dynamic error: loop limit reached.
  [1]

  $ aslref double-while-incorrect-incorrect.asl
  File double-while-incorrect-incorrect.asl, line 9, character 4 to line 11,
    character 8:
  ASL Dynamic error: loop limit reached.
  [1]

Recursion limits:
=================

  $ aslref recursion-no-limit.asl
  Number of calls:  11
  Number of calls:  11

  $ aslref recursion-correct.asl
  Number of calls:  11
  Number of calls:  11

  $ aslref recursion-incorrect.asl
  0
  1
  2
  3
  4
  File recursion-incorrect.asl, line 6, characters 18 to 31:
  ASL Dynamic error: recursion limit reached.
  [1]

  $ aslref recursion-exact.asl
  Number of calls:  11
  Number of calls:  11

  $ aslref recursion-exact-minus-one.asl
  File recursion-exact-minus-one.asl, line 5, characters 18 to 31:
  ASL Dynamic error: recursion limit reached.
  [1]
