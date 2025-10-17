  $ aslref binop-non-associative.asl
  File binop-non-associative.asl, line 1, characters 8 to 13:
  let x = a - b - c;
          ^^^^^
  ASL Grammar error: Cannot parse. Binary operator `-` is not associative -
    parenthesise to disambiguate.
  [1]

  $ aslref no-expression-elsif.asl
  File no-expression-elsif.asl, line 1, characters 23 to 28:
  let x = if TRUE then 1 elsif FALSE then 2 else 3;
                         ^^^^^
  ASL Grammar error: Cannot parse. Use `else if` instead.
  [1]

  $ aslref no_end_semicolon.asl
  File no_end_semicolon.asl, line 6, characters 2 to 8:
    return 0;
    ^^^^^^
  ASL Grammar error: Cannot parse. The `end` keyword must be followed by a
    semicolon (`;`).
    
  [1]

  $ aslref discard-locals.asl
  File discard-locals.asl, line 3, characters 6 to 12:
    let (-, -) = (1, 2);
        ^^^^^^
  ASL Grammar error: Cannot parse. A local declaration must declare at least
    one name.
  [1]

  $ aslref elided-parameter-non-bits.asl
  File elided-parameter-non-bits.asl, line 3, characters 20 to 27:
    let x : integer = Zeros{};
                      ^^^^^^^
  ASL Grammar error: Cannot parse. Cannot desugar elided parameter: left-hand
    side must have a `bits(...)` type annotation.
  [1]

  $ aslref hyphenated-pending-constraint.asl
  File hyphenated-pending-constraint.asl, line 1, characters 14 to 17:
  let x: integer{-} = 5;
                ^^^
  ASL Grammar error: Cannot parse. Pending constraints are written `integer{}`.
  [1]

  $ aslref local-constant.asl
  File local-constant.asl, line 3, characters 2 to 10:
    constant x = 1;
    ^^^^^^^^
  ASL Grammar error: Cannot parse. Local constant declarations are not valid
    ASL1. Did you mean `let`?.
  [1]

  $ aslref single-implication.asl
  File single-implication.asl, line 1, characters 10 to 13:
  let x = a --> b;
            ^^^
  ASL Grammar error: Cannot parse. Did you mean `==>`?
  [1]

  $ aslref binop-same-precedence.asl
  File binop-same-precedence.asl, line 1, characters 8 to 13:
  let x = a + b - c;
          ^^^^^
  ASL Grammar error: Cannot parse. Operators `-` and `+` have the same
    precedence - parenthesise to disambiguate.
  [1]

  $ aslref empty-record.asl
  File empty-record.asl, line 1, characters 10 to 16:
  type X of record;
            ^^^^^^
  ASL Grammar error: Cannot parse. Empty record types must be declared with
    empty field list `{-}`.
  [1]

  $ aslref global-ignored.asl
  File global-ignored.asl, line 1, characters 4 to 5:
  var - = 1;
      ^
  ASL Grammar error: Cannot parse. A global declaration must declare a name.
  [1]

  $ aslref global-let-comma.asl
  File global-let-comma.asl, line 1, characters 5 to 6:
  let x, y : integer = 1;
       ^
  ASL Grammar error: Cannot parse. A global `let` declaration must introduce a
    single name and have an initialising expression:
      let name = initial_expression;
      let name : type = initial_expression;
    
  [1]

  $ aslref local-let-no-parentheses.asl
  File local-let-no-parentheses.asl, line 3, characters 7 to 8:
    let a, b = foo();
         ^
  ASL Grammar error: Cannot parse. A local `let` declaration must be of one of
    the following forms:
      let name = expression;
      let name : type = expression;
      let (name1, -, ...) = expression;
      let (name1, -, ...) : type = expression;
    
  [1]

  $ aslref multiple-configs.asl
  File multiple-configs.asl, line 1, characters 8 to 9:
  config x, y : integer = 1;
          ^
  ASL Grammar error: Cannot parse. A `config` declaration must introduce a
    single name, and have both a type annotation and initialising expression:
      config name : type = initial_expression;
    
  [1]

  $ aslref multiple-constants.asl
  File multiple-constants.asl, line 1, characters 10 to 11:
  constant x, y : integer = 1;
            ^
  ASL Grammar error: Cannot parse. A `constant` declaration must introduce a
    single name and have an initialising expression:
      constant name = initial_expression;
      constant name : type = initial_expression;
    
  [1]

  $ aslref single-biimplication.asl
  File single-biimplication.asl, line 1, characters 10 to 13:
  let x = a <-> b;
            ^^^
  ASL Grammar error: Cannot parse. Did you mean `<=>`?
  [1]

  $ aslref uninitialised-config.asl
  File uninitialised-config.asl, line 1, characters 18 to 19:
  config x : integer;
                    ^
  ASL Grammar error: Cannot parse. A `config` declaration must introduce a
    single name, and have both a type annotation and initialising expression:
      config name : type = initial_expression;
    
  [1]

  $ aslref uninitialised-constant.asl
  File uninitialised-constant.asl, line 1, characters 20 to 21:
  constant x : integer;
                      ^
  ASL Grammar error: Cannot parse. A `constant` declaration must introduce a
    single name and have an initialising expression:
      constant name = initial_expression;
      constant name : type = initial_expression;
    
  [1]

  $ aslref uninitialised-let.asl
  File uninitialised-let.asl, line 1, characters 15 to 16:
  let x : integer;
                 ^
  ASL Grammar error: Cannot parse. Declarations using `let` must have
    initialising expressions.
    
  [1]
