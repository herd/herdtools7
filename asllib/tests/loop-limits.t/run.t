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
    while (i < 10) looplimit 5 do
      i = i + 1;
      println(i);
    end;
  ASL Dynamic error: loop limit reached.
  [1]

  $ aslref while-exact.asl

  $ aslref while-exact-minus-one.asl
  File while-exact-minus-one.asl, line 4, character 2 to line 6, character 6:
    while (i < 10) looplimit 9 do
      i = i + 1;
    end;
  ASL Dynamic error: loop limit reached.
  [1]

  $ aslref while-no-limit.asl
  File while-no-limit.asl, line 4, character 2 to line 6, character 6:
    while (i < 10) do
      i = i + 1;
    end;
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
    repeat
      i = i + 1;
      println(i);
    until (i >= 10) looplimit 5;
  ASL Dynamic error: loop limit reached.
  [1]

  $ aslref repeat-exact.asl

  $ aslref repeat-exact-minus-one.asl
  File repeat-exact-minus-one.asl, line 4, character 2 to line 6, character 30:
    repeat
      i = i + 1;
    until (i >= 10) looplimit 9;
  ASL Dynamic error: loop limit reached.
  [1]

  $ aslref repeat-no-limit.asl
  File repeat-no-limit.asl, line 4, character 2 to line 6, character 18:
    repeat
      i = i + 1;
    until (i >= 10);
  ASL Warning: Loop does not have a limit.

Double loops
  $ aslref double-while-correct-correct.asl

  $ aslref double-while-correct-incorrect.asl
  File double-while-correct-incorrect.asl, line 9, character 4 to line 11,
    character 8:
      while (j < 10) looplimit 5 do
        j = j + 1;
      end;
  ASL Dynamic error: loop limit reached.
  [1]

  $ aslref double-while-incorrect-correct.asl
  File double-while-incorrect-correct.asl, line 6, character 2 to line 12,
    character 6:
    while (i < 10) looplimit 5 do
      i = i + 1;
      j = 0;
      while (j < 10) looplimit 20 do
        j = j + 1;
      end;
    end;
  ASL Dynamic error: loop limit reached.
  [1]

  $ aslref double-while-incorrect-incorrect.asl
  File double-while-incorrect-incorrect.asl, line 9, character 4 to line 11,
    character 8:
      while (j < 10) looplimit 5 do
        j = j + 1;
      end;
  ASL Dynamic error: loop limit reached.
  [1]

For loops
  $ aslref for-correct.asl
  $ aslref for-incorrect.asl
  File for-incorrect.asl, line 5, characters 4 to 26:
      counter = counter + 1;
      ^^^^^^^^^^^^^^^^^^^^^^
  ASL Dynamic error: loop limit reached.
  [1]
  $ aslref for-exact.asl
  File for-exact.asl, line 5, characters 4 to 26:
      counter = counter + 1;
      ^^^^^^^^^^^^^^^^^^^^^^
  ASL Dynamic error: loop limit reached.
  [1]
  $ aslref for-exact-minus-one.asl
  $ aslref for-no-limit.asl

Recursion limits:
=================

  $ aslref recursion-no-limit.asl
  File recursion-no-limit.asl, line 1, character 0 to line 5, character 4:
  func recurse (n: integer) => integer
  begin
    if n >= 10 then return 1;
    else return 1 + recurse (n+1); end;
  end;
  ASL Warning: the recursive function recurse has no recursive limit
  annotation.
  Number of calls: 11
  Number of calls: 11

  $ aslref recursion-correct.asl
  Number of calls: 11
  Number of calls: 11

  $ aslref recursion-incorrect.asl
  0
  1
  2
  3
  4
  File recursion-incorrect.asl, line 6, characters 18 to 31:
    else return 1 + recurse (n+1); end;
                    ^^^^^^^^^^^^^
  ASL Dynamic error: recursion limit reached.
  [1]

  $ aslref recursion-exact.asl
  Number of calls: 11
  Number of calls: 11

  $ aslref recursion-exact-minus-one.asl
  File recursion-exact-minus-one.asl, line 5, characters 18 to 31:
    else return 1 + recurse (n+1); end;
                    ^^^^^^^^^^^^^
  ASL Dynamic error: recursion limit reached.
  [1]
