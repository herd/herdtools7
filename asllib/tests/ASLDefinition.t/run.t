Examples used in ASL High-level Definition:
  $ aslref main0.asl
  $ aslref main_uncaught.asl
  Uncaught exception: MyException {}.
  [1]
  $ aslref --no-exec spec1.asl
  $ aslref --no-exec spec2.asl
  $ aslref spec3.asl

  $ aslref --no-exec Bitfields.asl
  $ aslref Bitfields_nested.asl
  $ aslref Bitvector_slices.asl
  $ aslref Bitvector_rotate.asl
  bv=0x14, rotated twice=0x05

  $ aslref CaseStatement.discriminant.asl
  num_tests: 0
  selected case 2

  $ aslref CaseStatement.otherwise.asl
  num_tests: 0
  selected otherwise

  $ aslref CaseStatement.no_otherwise.asl
  num_tests: 0
  File CaseStatement.no_otherwise.asl, line 17, characters 9 to 30:
  ASL Dynamic error: Unreachable reached.
  [1]

  $ aslref UnreachableStatement.asl
  diagnostic assertion failed: example message
  File UnreachableStatement.asl, line 5, characters 8 to 22:
  ASL Dynamic error: Unreachable reached.
  [1]

  $ aslref AssertionStatement.asl
  File AssertionStatement.asl, line 5, characters 11 to 22:
  ASL Execution error: Assertion failed: ((a + b) < 256).
  [1]

  $ aslref TypingErrorReporting.asl
  File TypingErrorReporting.asl, line 3, characters 11 to 22:
  ASL Typing error: Illegal application of operator + on types integer {5}
    and string.
  [1]

  $ aslref DynamicErrorReporting.asl
  ASL Dynamic error: Illegal application of operator DIV for values 128 and 7.
  [1]

  $ aslref --no-exec Accessor.asl
