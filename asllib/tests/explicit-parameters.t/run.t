Explicit parameter tests:
  $ aslref explicit-parameters.asl

  $ aslref bad-declaration-order.asl
  File bad-declaration-order.asl, line 4, character 0 to line 7, character 4:
  ASL Typing error: incorrect parameter declaration for "Bad", expected
    {A, B, C, D, E} but {D, E, A, B, C} provided
  [1]

  $ aslref unused-parameter.asl
  File unused-parameter.asl, line 1, character 0 to line 4, character 4:
  ASL Typing error: incorrect parameter declaration for "BadUnused", expected
    {} but {N} provided
  [1]

  $ aslref undeclared-parameter.asl
  File undeclared-parameter.asl, line 1, character 0 to line 4, character 4:
  ASL Typing error: incorrect parameter declaration for "BadUndeclared",
    expected {N} but {} provided
  [1]

  $ aslref duplicate-parameter.asl
  File duplicate-parameter.asl, line 1, character 0 to line 4, character 4:
  ASL Typing error: cannot declare already declared element "N".
  [1]

  $ aslref bad-argument-omission.asl
  File bad-argument-omission.asl, line 3, characters 21 to 24:
  ASL Error: Undefined identifier: 'Foo'
  [1]

  $ aslref bad-elided-parameter.asl
  File bad-elided-parameter.asl, line 8, characters 20 to 30:
  ASL Static Error: Arity error while calling 'Foo':
    1 parameters expected and 2 provided
  [1]

  $ aslref omit-output-stdlib-param.asl
  File omit-output-stdlib-param.asl, line 3, characters 21 to 41:
  ASL Static Error: Arity error while calling 'Extend-1':
    2 parameters expected and 1 provided
  [1]
