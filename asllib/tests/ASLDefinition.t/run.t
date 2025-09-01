Examples used in ASL High-level Definition:
  $ aslref main0.asl
  $ aslref main_uncaught.asl
  ASL Dynamic error: Uncaught exception: MyException {}.
  [1]
  $ aslref --no-exec spec1.asl
  $ aslref --no-exec spec2.asl
  $ aslref spec3.asl

  $ aslref --no-exec Bitfields.asl
  $ aslref Bitfields_nested.asl
  $ aslref Bitvector_slices.asl
  $ aslref --no-exec Bitvector_slices2.asl
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
  ASL Dynamic error: unreachable reached.
  [1]
  $ aslref CaseStatement.where.asl

  $ aslref UnreachableStatement.asl
  diagnostic assertion failed: example message
  File UnreachableStatement.asl, line 5, characters 8 to 20:
          unreachable;
          ^^^^^^^^^^^^
  ASL Dynamic error: unreachable reached.
  [1]

  $ aslref AssertionStatement.asl
  File AssertionStatement.asl, line 5, characters 11 to 22:
      assert a + b < 256;
             ^^^^^^^^^^^
  ASL Dynamic error: Assertion failed: ((a + b) < 256).
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
  Record construction:
  12
  Print statements:
  12341234
  $ aslref --no-exec GuideRule.BitvectorWidthBounds.asl
  $ aslref GuideRule.TupleLength.asl
  $ aslref GuideRule.TupleElementAccess.asl
  $ aslref GuideRule.TupleElementAccess.bad.asl
  File GuideRule.TupleElementAccess.bad.asl, line 5, characters 18 to 25:
      x = (x.item1, x.item2);
                    ^^^^^^^
  ASL Type error: There is no field 'item2' on type (integer, integer).
  [1]
  $ aslref GuideRule.AnonymousEnumerations.bad.asl
  File GuideRule.AnonymousEnumerations.bad.asl, line 4, characters 12 to 23:
      var x : enumeration {RED, GREEN, BLUE};
              ^^^^^^^^^^^
  ASL Grammar error: Cannot parse.
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
  ASL Static error: Arity error while calling 'X':
    1 parameters expected and 0 provided
  [1]
  $ aslref ParameterOmission.bad.asl
  File ParameterOmission.bad.asl, line 6, characters 17 to 18:
      result = LSL{}(result, 3);
                   ^
  ASL Grammar error: Cannot parse.
  [1]
  $ aslref --no-exec NamedTypes.asl
  $ aslref --no-exec NamedTypes2.asl
  $ aslref --no-exec NamedTypes3.asl
  $ aslref --no-exec NamedTypes4.asl
  $ aslref --no-exec NamedTypes.bad.asl
  File NamedTypes.bad.asl, line 8, characters 4 to 5:
      b = K; // Illegal: a Char cannot be directly assigned to a Byte
      ^
  ASL Type error: a subtype of Byte was expected, provided Char.
  [1]
  $ aslref --no-exec GuideRule.GlobalStorageCycles.bad1.asl
  File GuideRule.GlobalStorageCycles.bad1.asl, line 4, characters 0 to 10:
  var b = a;
  ^^^^^^^^^^
  ASL Type error: multiple recursive declarations: "b", "a".
  [1]
  $ aslref --no-exec GuideRule.GlobalStorageCycles.bad2.asl
  File GuideRule.GlobalStorageCycles.bad2.asl, line 3, characters 0 to 23:
  var var1 : bits(size1); // cycle -- the type of var1 depends on size1 which depends
  ^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: multiple recursive declarations: "var1", "size1".
  [1]
  $ aslref --no-exec SubprogramDeclarations.asl
  $ aslref --no-exec GlobalPragma.asl
  File GlobalPragma.asl, line 2, characters 0 to 25:
  pragma asl_op 1, "start";
  ^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Warning: pragma asl_op will be ignored.
  File GlobalPragma.asl, line 1, characters 0 to 19:
  pragma asl_pragma1;
  ^^^^^^^^^^^^^^^^^^^
  ASL Warning: pragma asl_pragma1 will be ignored.
  $ aslref --no-exec GlobalPragma2.asl
  File GlobalPragma2.asl, line 2, characters 0 to 33:
  pragma other_tool_op '0010', 123;
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Warning: pragma other_tool_op will be ignored.
  File GlobalPragma2.asl, line 1, characters 0 to 23:
  pragma my_tool_pragma1;
  ^^^^^^^^^^^^^^^^^^^^^^^
  ASL Warning: pragma my_tool_pragma1 will be ignored.
  $ aslref MutualRecursion.bad.asl
  File MutualRecursion.bad.asl, line 3, characters 11 to 17:
      return bar(a);
             ^^^^^^
  ASL Dynamic error: recursion limit reached.
  [1]
  $ aslref --no-exec Guide.OperatorPrecedence.asl
  $ aslref --no-exec TupleExpressions.asl
  $ aslref --no-exec LocalStorageDeclarations.asl
  $ aslref CatchingExceptions.asl
  $ aslref --no-exec CatchingExceptions2.asl
  $ aslref --no-exec ParametricFunction.asl
  $ aslref --no-exec ParametricFunction2.asl
  $ aslref --no-exec symbolic_bitwidth.asl
  $ aslref --no-exec constrained_bitwidth.asl
  $ aslref --no-exec ConstrainedIntegers1.asl
  $ aslref --no-exec ConstrainedIntegers2.asl
  $ aslref --no-exec ConstrainedIntegers.bad.asl
  File ConstrainedIntegers.bad.asl, line 7, characters 4 to 5:
      B = A; // illegal: {2,4,8} is not a subset of {2,4}.
      ^
  ASL Type error: a subtype of integer {2, 4} was expected,
    provided integer {2, 4, 8}.
  [1]
  $ aslref --no-exec ConstrainedIntegers.bad2.asl
  File ConstrainedIntegers.bad2.asl, line 8, characters 4 to 10:
      myIntA = myIntB; // Illegal even though at this point
      ^^^^^^
  ASL Type error: a subtype of integer {1..10} was expected,
    provided integer {0..20}.
  [1]
  $ aslref --no-exec PrimitiveOperations.asl
  $ aslref Types.asl
  $ aslref Expressions.asl
  $ aslref Patterns.asl
  $ aslref AssignableExpressions.asl
  $ aslref Statements.asl
  $ aslref --no-exec GlobalStorageDeclarations.asl
  $ aslref --no-exec IR.asl
  $ aslref Bit.asl
  $ aslref PrefixSlice.asl
  $ aslref --no-exec GuideRule.NoEmptyBody.asl
