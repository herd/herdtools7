Hello world should work:

  $ aslref hello_world.asl
  Hello, world!

Type-checking errors:

  $ aslref subtype-satisfaction-arrray-illegal.asl
  File subtype-satisfaction-arrray-illegal.asl, line 4, characters 0 to 36:
  ASL Typing error: a subtype of m was expected, provided array [10] of n.
  [1]

  $ aslref anonymous-types-example.asl
  File anonymous-types-example.asl, line 21, characters 2 to 6:
  ASL Typing error: a subtype of pairT was expected,
    provided (integer {1}, T2).
  [1]

  $ aslref duplicate_function_args.asl
  File duplicate_function_args.asl, line 1, character 0 to line 4, character 3:
  ASL Typing error: cannot declare already declared element "i".
  [1]

  $ aslref duplicate_record_fields.asl
  File duplicate_record_fields.asl, line 1, character 0 to line 5, character 2:
  ASL Typing error: cannot declare already declared element "i".
  [1]

  $ aslref duplicate_enumeration_items.asl
  File duplicate_enumeration_items.asl, line 1, characters 0 to 34:
  ASL Typing error: cannot declare already declared element "i".
  [1]

  $ aslref constant-zeros.asl

Bad types:
  $ aslref overlapping-slices.asl
  File overlapping-slices.asl, line 1, character 0 to line 4, character 2:
  ASL Typing error: overlapping slices 0+:11, 3+:2.
  [1]

Global ignored:
  $ cat >global_ignored.asl <<EOF
  > var - = 3 / 0;
  > func main () => integer
  > begin return 0; end
  > EOF

  $ aslref global_ignored.asl
  File global_ignored.asl, line 1, characters 8 to 13:
  ASL Typing error: Illegal application of operator / on types integer {3}
    and integer {0}.
  [1]

Constrained-type satisfaction:
  $ cat >type-sat1.asl <<EOF
  > func illegal_f1()
  > begin
  >   var x: integer { 8, 16 };
  >   var y: integer { 8, 16, 32};
  >   x = y; // illegal as domain of x is not a subset of domain of y
  > end
  > EOF

  $ aslref type-sat1.asl
  File type-sat1.asl, line 5, characters 2 to 3:
  ASL Typing error: a subtype of integer {8, 16} was expected,
    provided integer {8, 16, 32}.
  [1]

  $ cat >type-sat2.asl <<EOF
  > func illegal_f2()
  > begin
  >   var x: integer { 8 , 16 };
  >   var y: integer;
  >   x = y; // illegal
  > end
  > EOF

  $ aslref type-sat2.asl
  File type-sat2.asl, line 5, characters 2 to 3:
  ASL Typing error: a subtype of integer {8, 16} was expected,
    provided integer.
  [1]

  $ aslref type_satisfaction_illegal_f3.asl
  File type_satisfaction_illegal_f3.asl, line 9, characters 4 to 17:
  ASL Typing error: a subtype of integer {8, 16} was expected,
    provided integer.
  [1]

  $ aslref type_satisfaction_illegal_f4.asl
  File type_satisfaction_illegal_f4.asl, line 9, characters 4 to 17:
  ASL Typing error: a subtype of integer {8, 16} was expected,
    provided integer {8..64}.
  [1]

  $ cat >type-sat3.asl <<EOF
  > func illegal_f5 {N} (b: bits(N))
  > begin
  >   // N is under-constrained integer
  >   var x: integer { 2, 4} = N;
  >    return;
  > end
  > EOF

  $ aslref type-sat3.asl
  File type-sat3.asl, line 4, characters 2 to 29:
  ASL Typing error: a subtype of integer {2, 4} was expected,
    provided integer {N}.
  [1]

  $ cat >type-sat4.asl <<EOF
  > func invokeMe_2 {N} (b: bits(N))
  > begin
  >   // N is under-constrained integer
  >   var x: integer { 2, 4} = N;
  >   return;
  > end
  > EOF

  $ aslref type-sat4.asl
  File type-sat4.asl, line 4, characters 2 to 29:
  ASL Typing error: a subtype of integer {2, 4} was expected,
    provided integer {N}.
  [1]

Runtime checks:
  $ cat >runtime-type-sat1.asl <<EOF
  > func main () => integer
  > begin
  >   let x: integer {1} = 2 as integer {1};
  >   return 0;
  > end
  > EOF

  $ aslref runtime-type-sat1.asl
  File runtime-type-sat1.asl, line 3, characters 23 to 24:
  ASL Execution error: Mismatch type:
    value 2 does not belong to type integer {1}.
  [1]

  $ cat >runtime-type-sat2.asl <<EOF
  > func test(size: integer {3, 4}) begin
  >   let - = Zeros(4) as bits(size);
  > end
  > func main () => integer begin
  >   test(4);
  >   test(3);
  >   return 0;
  > end
  > EOF

  $ aslref runtime-type-sat2.asl
  File runtime-type-sat2.asl, line 2, characters 10 to 18:
  ASL Execution error: Mismatch type:
    value '0000' does not belong to type bits(size).
  [1]

  $ aslref under-constrained-used.asl

Parameterized integers:
  $ aslref bad-underconstrained-call.asl
  File bad-underconstrained-call.asl, line 9, characters 9 to 23:
  ASL Typing error: a subtype of integer {0..(M - 1)} was expected,
    provided integer {M}.
  [1]
  $ aslref bad-underconstrained-call-02.asl
  File bad-underconstrained-call-02.asl, line 8, characters 2 to 13:
  ASL Typing error: a subtype of integer {M} was expected,
    provided integer {3}.
  [1]
  $ aslref bad-underconstrained-call-03.asl
  File bad-underconstrained-call-03.asl, line 8, characters 2 to 17:
  ASL Typing error: a subtype of integer {M} was expected,
    provided integer {(M + 1)}.
  [1]
  $ aslref bad-underconstrained-ctc.asl
  File bad-underconstrained-ctc.asl, line 3, characters 12 to 13:
  ASL Execution error: Mismatch type:
    value 4 does not belong to type integer {(N - 1)}.
  [1]
  $ aslref bad-underconstrained-return.asl
  File bad-underconstrained-return.asl, line 3, characters 2 to 15:
  ASL Typing error: a subtype of integer {0..N} was expected,
    provided integer {(N + 1)}.
  [1]
  $ aslref bad-underconstrained-return-02.asl
  File bad-underconstrained-return-02.asl, line 3, characters 2 to 11:
  ASL Typing error: a subtype of integer {0..N} was expected,
    provided integer {5}.
  [1]

  $ aslref named-types-in-slices.asl
  '11111111'

  $ aslref empty-slice.asl
  '000'
  ASL Dynamic error: Cannot extract from bitvector of length 0 slice 4+:-1.
  [1]

  $ aslref bad-slices.asl
  ASL Dynamic error: Cannot extract from bitvector of length 0 slice 4+:-23.
  [1]

  $ aslref bad-shift.asl
  '00000'

  $ aslref unreachable.asl
  File unreachable.asl, line 3, characters 2 to 17:
  ASL Dynamic error: Unreachable reached.
  [1]

  $ aslref assign-to-global-immutable.asl
  File assign-to-global-immutable.asl, line 5, characters 2 to 21:
  ASL Typing error: cannot assign to immutable storage "my_immutable_global".
  [1]

  $ aslref equality.asl
  $ aslref bad-equality.asl
  File bad-equality.asl, line 3, characters 8 to 23:
  ASL Typing error: Illegal application of operator == on types
    (integer {1}, integer {2}) and (integer {1}, integer {2}).
  [1]

  $ aslref setter_without_getter.asl
  File setter_without_getter.asl, line 1, character 0 to line 4, character 3:
  ASL Typing error: setter "f" does not have a corresponding getter of
    signature integer -> integer.
  [1]

  $ aslref tuple_items.asl
  $ aslref cases_where.asl
  $ aslref duplicated-otherwise.asl
  File duplicated-otherwise.asl, line 7, characters 8 to 12:
  ASL Error: Cannot parse.
  [1]
  $ aslref duplicate_expr_record.asl
  File duplicate_expr_record.asl, line 5, characters 12 to 27:
  ASL Typing error: cannot declare already declared element "h".
  [1]

  $ aslref rdiv_checks.asl
  File rdiv_checks.asl, line 3, characters 12 to 25:
  ASL Typing error: Illegal application of operator / on types real and string.
  [1]

  $ aslref record-getfields.asl

  $ aslref integer-accessed-bitvector.asl
  File integer-accessed-bitvector.asl, line 4, characters 2 to 6:
  ASL Typing error: a subtype of bits(-) was expected, provided integer.
  [1]

Arrays indexed by enumerations
  $ aslref enum-array.asl
  [0, 0, 0]

  $ aslref array-lca.asl
  $ aslref array-index-error.asl
  ASL Execution error: Mismatch type:
    value 14 does not belong to type integer {0..4}.
  [1]

Parameters bugs:
  $ aslref bug1.asl
  File bug1.asl, line 5, characters 21 to 29:
  ASL Typing error: constrained integer expected, provided integer.
  [1]
  $ aslref bug2.asl
  ASL Typing error: constrained integer expected, provided integer.
  [1]
  $ aslref bug3.asl
  File bug3.asl, line 4, characters 10 to 18:
  ASL Typing error: constrained integer expected, provided integer.
  [1]
  $ aslref bug4.asl
  File bug4.asl, line 5, characters 11 to 31:
  ASL Typing error: Illegal application of operator OR on types bits(3)
    and bits(4).
  [1]
  $ aslref arg-as-param-call.asl
  File arg-as-param-call.asl, line 8, characters 4 to 21:
  ASL Typing error: a subtype of bits(10) was expected, provided bits(4).
  [1]
  $ aslref typed-param-call.asl
  File typed-param-call.asl, line 8, characters 4 to 15:
  ASL Typing error: a subtype of integer {5..10} was expected,
    provided integer {2}.
  [1]
  $ aslref typed-arg-as-param-call.asl
  File typed-arg-as-param-call.asl, line 8, characters 4 to 18:
  ASL Typing error: a subtype of integer {5..10} was expected,
    provided integer {2}.
  [1]
  $ aslref --no-exec defining_param.asl
  $ aslref rename-returned-tuples.asl

Required tests:
  $ aslref anonymous-types-example-success.asl
  $ aslref array-with-enums.asl
  $ aslref array.asl
  $ aslref -0 assign-v0.asl
  $ aslref -0 asl0-patterns.asl
  File asl0-patterns.asl, line 7, characters 25 to 29:
  ASL Error: Cannot parse.
  [1]
  $ aslref assign1.asl
  $ aslref big-ints.asl
  $ aslref bitfields.asl
  $ aslref bitvectors.asl
  $ aslref case.asl
  $ aslref concat-empty.asl
  File concat-empty.asl, line 3, characters 46 to 47:
  ASL Error: Cannot parse.
  [1]
  $ aslref concat01.asl
  $ aslref concat02.asl
  $ aslref concat03.asl
  $ aslref constrained-integer-types-example.asl
  $ aslref constrained-types-example.asl
  $ aslref division.asl
  $ aslref exceptions.asl
  $ aslref func1.asl
  $ aslref func2.asl
  $ aslref func3.asl
  $ aslref func4.asl
  $ aslref func5.asl
  $ aslref func6.asl
  $ aslref func7.asl
  $ aslref global_vars.asl
  $ aslref global_vars-02.asl
  $ aslref lexpr-concat.asl
  $ aslref --no-exec lexpr-concat-2.asl
  $ aslref masks.asl
  $ aslref more-assignments-examples.asl
  $ aslref more-invocation-examples.asl
  $ aslref named-types-example.asl
  $ aslref nested-bitfields.asl
  $ aslref operator_precedence.asl
  $ aslref pass.asl
  $ aslref patterns.asl
  $ aslref pattern-string.asl
  $ aslref records-2.asl
  $ aslref records.asl
  $ aslref static.asl
  $ aslref stdlib.asl
  $ aslref subtypes-example.asl
  $ aslref subtypes-with.asl
  $ aslref tuples.asl
  $ aslref declaration-primitive-local.asl
  $ aslref --no-type-check -0 typing-assign-v0.asl

  $ aslref undeclared-variable.asl
  File undeclared-variable.asl, line 3, characters 2 to 5:
  ASL Error: Undefined identifier: 'bar'
  [1]

Base values
  $ aslref base_values.asl
  File base_values.asl, line 5, characters 2 to 28:
  ASL Typing error: base value of type integer {N..M, 42} cannot be statically
    determined since it consists of N.
  [1]

  $ aslref base_values_empty.asl
  File base_values_empty.asl, line 3, characters 2 to 24:
  ASL Typing error: base value of type integer {N..M} cannot be statically
    determined since it consists of N.
  [1]

Empty getters/setters
  $ aslref empty-getter-called-with-slices.asl
  File empty-getter-called-with-slices.asl, line 8, characters 10 to 14:
  ASL Static Error: cannot slice with empty slicing operator. This might also
    be due to an incorrect getter/setter invocation.
  [1]
  $ aslref empty-getter-called-with-slices-2.asl
  File empty-getter-called-with-slices-2.asl, line 8, characters 10 to 14:
  ASL Typing error: boolean does not subtype any of: integer, bits(-).
  [1]
  $ aslref nonempty-getter-called-without-slices.asl
  File nonempty-getter-called-without-slices.asl, line 8, characters 10 to 12:
  ASL Error: Undefined identifier: 'f1'
  [1]
  $ aslref empty-setter-nonempty-getter.asl
  File empty-setter-nonempty-getter.asl, line 6, character 0 to line 9,
    character 3:
  ASL Typing error: setter "f1" does not have a corresponding getter of
    signature  -> integer.
  [1]
  $ aslref nonempty-setter-empty-getter.asl
  File nonempty-setter-empty-getter.asl, line 6, character 0 to line 9,
    character 3:
  ASL Typing error: setter "f1" does not have a corresponding getter of
    signature  -> integer.
  [1]
  $ aslref empty-setter-called-with-slices.asl
  File empty-setter-called-with-slices.asl, line 13, characters 2 to 6:
  ASL Static Error: cannot slice with empty slicing operator. This might also
    be due to an incorrect getter/setter invocation.
  [1]
  $ aslref nonempty-setter-called-without-slices.asl
  File nonempty-setter-called-without-slices.asl, line 13, characters 2 to 4:
  ASL Error: Undefined identifier: 'f1'
  [1]
  $ aslref setter_subfield.asl
  $ aslref setter_sub_tuple.asl
  File setter_sub_tuple.asl, line 21, characters 15 to 16:
  ASL Typing error: a subtype of integer {0} was expected, provided integer.
  [1]
  $ aslref setter_sub_tuple_02.asl
  $ aslref setter_subslice.asl
  $ aslref getter_subfield.asl
  $ aslref getter_sub_tuple.asl
  $ aslref getter_subslice.asl
  $ aslref getter_subfields.asl
  $ aslref setter_bitfields.asl
  $ aslref pstate-exp.asl --type-check-warn
  File pstate-exp.asl, line 60, characters 10 to 11:
  ASL Static Error: Unsupported expression n.
  File pstate-exp.asl, line 60, characters 10 to 11:
  ASL Static Error: Unsupported expression n.
  File pstate-exp.asl, line 62, characters 12 to 13:
  ASL Static Error: Unsupported expression n.
  File pstate-exp.asl, line 62, characters 12 to 13:
  ASL Static Error: Unsupported expression n.
  File pstate-exp.asl, line 78, characters 10 to 11:
  ASL Static Error: Unsupported expression n.
  File pstate-exp.asl, line 78, characters 10 to 11:
  ASL Static Error: Unsupported expression n.
  File pstate-exp.asl, line 80, characters 12 to 13:
  ASL Static Error: Unsupported expression n.
  File pstate-exp.asl, line 80, characters 12 to 13:
  ASL Static Error: Unsupported expression n.
  File pstate-exp.asl, line 96, characters 10 to 11:
  ASL Static Error: Unsupported expression n.
  File pstate-exp.asl, line 96, characters 10 to 11:
  ASL Static Error: Unsupported expression n.
  File pstate-exp.asl, line 98, characters 12 to 13:
  ASL Static Error: Unsupported expression n.
  File pstate-exp.asl, line 98, characters 12 to 13:
  ASL Static Error: Unsupported expression n.

  $ aslref bad-pattern.asl
  File bad-pattern.asl, line 4, characters 7 to 12:
  ASL Typing error: Erroneous pattern '101' for expression of type integer {3}.
  [1]
