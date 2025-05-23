Explicit parameter tests:
  $ aslref explicit-parameters.asl

  $ aslref bad-declaration-order.asl
  File bad-declaration-order.asl, line 4, character 0 to line 7, character 4:
  func Bad{D,E,A,B,C}(x: bits(D), y: bits(E)) => bits(A * B + C)
  begin
    return Zeros{A * B + C};
  end;
  ASL Type error: incorrect parameter declaration for "Bad", expected
    {A, B, C, D, E} but {D, E, A, B, C} provided
  [1]

  $ aslref unused-parameter.asl
  File unused-parameter.asl, line 1, character 0 to line 4, character 4:
  func BadUnused{N}(x: integer) => integer
  begin
    return 0;
  end;
  ASL Type error: incorrect parameter declaration for "BadUnused", expected 
    {} but {N} provided
  [1]

  $ aslref undeclared-parameter.asl
  File undeclared-parameter.asl, line 1, character 0 to line 4, character 4:
  func BadUndeclared(N: integer{3}) => bits(N)
  begin
    return Zeros{N};
  end;
  ASL Type error: incorrect parameter declaration for "BadUndeclared", expected
    {N} but {} provided
  [1]

  $ aslref duplicate-parameter.asl
  File duplicate-parameter.asl, line 1, character 0 to line 4, character 4:
  func BadDuplicated{N}(N: integer{3}) => bits(N)
  begin
    return Zeros{N};
  end;
  ASL Type error: cannot declare already declared element "N".
  [1]

  $ aslref argument-omission.asl

  $ aslref bad-elided-parameter.asl
  File bad-elided-parameter.asl, line 8, characters 20 to 30:
    let x : bits(4) = Foo{,3}(0);
                      ^^^^^^^^^^
  ASL Static Error: Arity error while calling 'Foo':
    1 parameters expected and 2 provided
  [1]

  $ aslref omit-output-stdlib-param.asl
  File omit-output-stdlib-param.asl, line 3, characters 21 to 41:
    let x : bits(64) = Extend('1111', TRUE);
                       ^^^^^^^^^^^^^^^^^^^^
  ASL Static Error: Arity error while calling 'Extend-1':
    2 parameters expected and 1 provided
  [1]

  $ aslref shadowed-parameter.asl
  File shadowed-parameter.asl, line 3, character 0 to line 6, character 4:
  func MyBits{N}(x: bits(N)) => integer {N+1}
  begin
    return N + 1;
  end;
  ASL Type error: cannot declare already declared element "N".
  [1]
