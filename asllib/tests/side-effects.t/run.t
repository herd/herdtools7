  $ aslref binop-read-read.asl
  $ aslref binop-read-write.asl
  File binop-read-write.asl, line 11, characters 10 to 31:
  ASL Typing error: concurrent side effects WriteGlobal "X" and ReadGlobal "X"
  [1]
  $ aslref binop-write-write.asl
  File binop-write-write.asl, line 11, characters 10 to 47:
  ASL Typing error: concurrent side effects WriteGlobal "X" and WriteGlobal "X"
  [1]
  $ aslref binop-read-write-diff.asl
  $ aslref binop-write-write-diff.asl

  $ aslref binop-throw-read.asl
  E caught
  $ aslref binop-throw-write.asl
  File binop-throw-write.asl, line 18, characters 12 to 43:
  ASL Typing error: concurrent side effects RaiseException "E" and WriteGlobal "X"
  [1]
  $ aslref binop-throw-throw.asl
  File binop-throw-throw.asl, line 11, characters 12 to 37:
  ASL Typing error: concurrent side effects RaiseException "E" and RaiseException "E"
  [1]
  $ aslref binop-throw-caught.asl
  E caught
  $ aslref binop-throw-not-caught.asl
  File binop-throw-not-caught.asl, line 21, characters 12 to 37:
  ASL Typing error: concurrent side effects RaiseException "E" and RaiseException "E"
  [1]
  $ aslref binop-throw-otherwised.asl
  E caught

  $ aslref binop-throw-unknown.asl
  Caught E.
  $ aslref binop-write-unknown.asl
  $ aslref binop-unknown-unknown.asl

  $ aslref constant-func.asl
  $ aslref constant-func-read.asl
  File constant-func-read.asl, line 9, characters 0 to 21:
  ASL Typing error: a pure expression was expected, found foo(4)
  [1]
  $ aslref constant-func-write.asl
  File constant-func-write.asl, line 9, characters 0 to 21:
  ASL Typing error: a pure expression was expected, found foo(4)
  [1]
  $ aslref constant-func-unknown.asl
  File constant-func-unknown.asl, line 7, characters 0 to 21:
  ASL Typing error: a pure expression was expected, found foo(4)
  [1]
  $ aslref constant-func-throw.asl
  File constant-func-throw.asl, line 8, characters 0 to 21:
  ASL Typing error: a pure expression was expected, found foo(4)
  [1]
  $ aslref constant-func-throw-caught.asl
  $ aslref constant-func-local-var.asl

  $ aslref for-var-no-edit.asl
  $ aslref for-var-edits.asl
  File for-var-edits.asl, line 6, character 2 to line 8, character 5:
  ASL Typing error: concurrent side effects ReadLocal "x" and WriteLocal "x"
  [1]
  $ aslref while-var-edits.asl
  $ aslref repeat-var-edits.asl

