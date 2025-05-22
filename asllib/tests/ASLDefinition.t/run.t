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
      case test_and_increment(x) of
           ^^^^^^^^^^^^^^^^^^^^^
  ASL Dynamic error: Unreachable reached.
  [1]

  $ aslref UnreachableStatement.asl
  diagnostic assertion failed: example message
  File UnreachableStatement.asl, line 5, characters 8 to 22:
          Unreachable();
          ^^^^^^^^^^^^^^
  ASL Dynamic error: Unreachable reached.
  [1]

  $ aslref AssertionStatement.asl
  File AssertionStatement.asl, line 5, characters 11 to 22:
      assert a + b < 256;
             ^^^^^^^^^^^
  ASL Execution error: Assertion failed: ((a + b) < 256).
  [1]

  $ aslref TypingErrorReporting.asl
  File TypingErrorReporting.asl, line 3, characters 11 to 22:
      return 5 + "hello";
             ^^^^^^^^^^^
  ASL Type error: Illegal application of operator + on types integer {5}
    and string.
  [1]

  $ aslref DynamicErrorReporting.asl
  ASL Dynamic error: Illegal application of operator DIV for values 128 and 7.
  [1]

  $ aslref --no-exec Accessor.asl
  $ aslref --no-exec Overriding.asl
  $ aslref --no-exec OverridingBad.asl
  File OverridingBad.asl, line 1, character 0 to line 4, character 4:
  implementation func Foo{N: integer{32,64}}(n : boolean) => bits(N)
  begin
    return Zeros{N};
  end;
  ASL Type error: multiple overlapping `implementation` functions for Foo:
    File OverridingBad.asl, line 1, character 0 to line 4, character 4
    File OverridingBad.asl, line 11, character 0 to line 14, character 4
  [1]

  $ aslref GlobalNamespace.asl
  $ aslref EvaluationOrder.asl
  Function calls:
  1234
  Tuples:
  12
  Non-short-circuiting binary operations:
  123
  Array-indexing:
  1
  2
  Slicing:
  132345677
  Record construction:
  12
  Print statements:
  12341234
  For-loop start/end expressions:
  1234
  $ aslref --no-exec GuideRule.BitvectorWidthBounds.asl
  $ aslref GuideRule.TupleLength.asl
  $ aslref GuideRule.TupleElementAccess.asl
  $ aslref GuideRule.AnonymousEnumerations.bad.asl
  File GuideRule.AnonymousEnumerations.bad.asl, line 4, characters 12 to 23:
      var x : enumeration {RED, GREEN, BLUE};
              ^^^^^^^^^^^
  ASL Error: Cannot parse.
  [1]
  $ aslref GuideRule.TupleImmutability.asl
  File GuideRule.TupleImmutability.asl, line 7, characters 6 to 11:
      x.item1 = '1'; // Illegal: tuples are immutable.
        ^^^^^
  ASL Type error: cannot assign to the (immutable) tuple value x.
  [1]

  $ aslref ParameterElision.asl
  $ aslref ParameterElision.bad.asl
  File ParameterElision.bad.asl, line 13, characters 25 to 35:
      var foo : bits(64) = X(data, n);
                           ^^^^^^^^^^
  ASL Static Error: Arity error while calling 'X':
    1 parameters expected and 0 provided
  [1]
  $ aslref ParameterOmission.bad.asl
  File ParameterOmission.bad.asl, line 6, characters 17 to 18:
      result = LSL{}(result, 3);
                   ^
  ASL Error: Cannot parse.
  [1]
