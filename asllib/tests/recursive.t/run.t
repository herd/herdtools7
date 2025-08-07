  $ aslref simple-recursive.asl

  $ aslref simple-recursive-without-limit.asl
  File simple-recursive-without-limit.asl, line 1, character 0 to line 8,
    character 4:
  func f (x: integer) => integer
  begin
    if x >= 0 then
      return 1 + f (x - 1);
    else
      return 0;
    end;
  end;
  ASL Warning: the recursive function f has no recursive limit annotation.

  $ aslref double-recursive.asl

  $ aslref double-recursive-without-limit.asl
  File double-recursive-without-limit.asl, line 10, character 0 to line 13,
    character 4:
  func g (x: integer) => integer
  begin
    return f (x);
  end;
  ASL Warning: the mutually-recursive functions g, f have no recursive limit
  annotation.

  $ aslref recursive-constant.asl
  File recursive-constant.asl, line 1, characters 13 to 14:
  constant x = x + 3;
               ^
  ASL Error: Undefined identifier: 'x'
  [1]

  $ aslref double-recursive-constant.asl
  File double-recursive-constant.asl, line 2, characters 0 to 19:
  constant y = x + 2;
  ^^^^^^^^^^^^^^^^^^^
  ASL Type error: multiple recursive declarations: "y", "x".
  [1]

  $ aslref recursive-type.asl
  File recursive-type.asl, line 1, characters 0 to 35:
  type tree of (tree, integer, tree);
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Error: Undefined identifier: 'tree'
  [1]

  $ aslref double-recursive-types.asl
  File double-recursive-types.asl, line 2, characters 0 to 29:
  type node of (integer, tree);
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: multiple recursive declarations: "node", "tree".
  [1]

  $ aslref fn-val-recursive.asl
  File fn-val-recursive.asl, line 1, characters 0 to 17:
  var x = f (4, 5);
  ^^^^^^^^^^^^^^^^^
  ASL Type error: multiple recursive declarations: "f", "x".
  [1]

  $ aslref type-val-recursive.asl
  File type-val-recursive.asl, line 3, characters 0 to 24:
  type MyT of integer {x};
  ^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: multiple recursive declarations: "MyT", "x".
  [1]

  $ aslref enum-fn-recursive.asl

  $ aslref procedure-simple.asl
  2
  1
  0

  $ aslref --no-exec procedure-no-limit.asl
  File procedure-no-limit.asl, line 1, character 0 to line 6, character 4:
  func foo(x: integer)
  begin
    println x;
    if x <= 0 then return; end;
    foo(x - 1);
  end;
  ASL Warning: the recursive function foo has no recursive limit annotation.

  $ aslref procedure-hits-limit.asl
  3
  2
  1
  File procedure-hits-limit.asl, line 6, characters 2 to 13:
    foo(x - 1);
    ^^^^^^^^^^^
  ASL Dynamic error: recursion limit reached.
  [1]
