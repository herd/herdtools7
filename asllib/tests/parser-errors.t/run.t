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
