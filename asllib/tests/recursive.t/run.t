  $ aslref simple-recursive.asl

  $ aslref double-recursive.asl

  $ aslref recursive-constant.asl
  File recursive-constant.asl, line 1, characters 13 to 14:
  ASL Error: Undefined identifier: 'x'
  [1]

  $ aslref double-recursive-constant.asl
  File double-recursive-constant.asl, line 2, characters 0 to 19:
  ASL Typing error: multiple recursive declarations: "y", "x"
  [1]

  $ aslref recursive-type.asl
  File recursive-type.asl, line 1, characters 0 to 35:
  ASL Error: Undefined identifier: 'tree'
  [1]

  $ aslref double-recursive-types.asl
  File double-recursive-types.asl, line 2, characters 0 to 29:
  ASL Typing error: multiple recursive declarations: "node", "tree"
  [1]

  $ aslref fn-val-recursive.asl
  File fn-val-recursive.asl, line 1, characters 0 to 17:
  ASL Typing error: multiple recursive declarations: "f", "x"
  [1]

  $ aslref type-val-recursive.asl
  File type-val-recursive.asl, line 3, characters 0 to 24:
  ASL Typing error: multiple recursive declarations: "MyT", "x"
  [1]

  $ aslref enum-fn-recursive.asl

