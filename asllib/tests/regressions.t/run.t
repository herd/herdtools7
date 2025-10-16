Hello world should work:

  $ aslref hello_world.asl
  Hello, world!

Type-checking errors:

  $ aslref subtype-satisfaction-arrray-illegal.asl
  File subtype-satisfaction-arrray-illegal.asl, line 4, characters 0 to 38:
  type o of array[[10]] of n subtypes m;
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: a subtype of m was expected, provided array [[10]] of n.
  [1]

  $ aslref anonymous-types-example.asl
  File anonymous-types-example.asl, line 21, characters 2 to 6:
    pair = (1, dataT2);
    ^^^^
  ASL Type error: a subtype of pairT was expected, provided (integer {1}, T2).
  [1]

  $ aslref duplicate_function_args.asl
  File duplicate_function_args.asl, line 1, character 0 to line 4, character 4:
  func foo(i: integer, i: integer)
  begin
    pass;
  end;
  ASL Type error: cannot declare already declared element "i".
  [1]

  $ aslref duplicate_record_fields.asl
  File duplicate_record_fields.asl, line 1, character 0 to line 5, character 2:
  type MyRecord of record {
    i: integer,
    j: boolean,
    i: integer
  };
  ASL Type error: cannot declare already declared element "i".
  [1]

  $ aslref duplicate_enumeration_items.asl
  File duplicate_enumeration_items.asl, line 1, characters 0 to 34:
  type t of enumeration { i, j, i };
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: cannot declare already declared element "i".
  [1]

  $ aslref constant-zeros.asl

Bad types:
  $ aslref overlapping-slices.asl
  File overlapping-slices.asl, line 1, character 0 to line 4, character 2:
  type t of bits(64) {
    [23: 0] a,
    [10: 0, 3+: 2] b,
  };
  ASL Static error: overlapping slices 0+:11, 3+:2.
  [1]

  $ aslref bad-inclusion-in-symbolic-type.asl
  File bad-inclusion-in-symbolic-type.asl, line 2, characters 0 to 26:
  var ah: integer{2..A} = 1;
  ^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: a subtype of integer {2..A} was expected,
    provided integer {1}.
  [1]

Global ignored:
  $ cat >global_ignored.asl <<EOF
  > var - = 3 / 0;
  > func main () => integer
  > begin return 0; end;
  > EOF

  $ aslref global_ignored.asl
  File global_ignored.asl, line 1, characters 4 to 5:
  var - = 3 / 0;
      ^
  ASL Grammar error: Cannot parse. A global declaration must declare a name.
  [1]

  $ aslref shadow-banning-bug.asl
  File shadow-banning-bug.asl, line 5, characters 4 to 16:
      var g = 0.0;
      ^^^^^^^^^^^^
  ASL Type error: cannot declare already declared element "g".
  [1]

Constrained-type satisfaction:
  $ cat >type-sat1.asl <<EOF
  > func illegal_f1()
  > begin
  >   var x: integer { 8, 16 };
  >   var y: integer { 8, 16, 32};
  >   x = y; // illegal as domain of x is not a subset of domain of y
  > end;
  > EOF

  $ aslref type-sat1.asl
  File type-sat1.asl, line 5, characters 2 to 3:
    x = y; // illegal as domain of x is not a subset of domain of y
    ^
  ASL Type error: a subtype of integer {8, 16} was expected,
    provided integer {8, 16, 32}.
  [1]

  $ cat >type-sat2.asl <<EOF
  > func illegal_f2()
  > begin
  >   var x: integer { 8 , 16 };
  >   var y: integer;
  >   x = y; // illegal
  > end;
  > EOF

  $ aslref type-sat2.asl
  File type-sat2.asl, line 5, characters 2 to 3:
    x = y; // illegal
    ^
  ASL Type error: a subtype of integer {8, 16} was expected, provided integer.
  [1]

  $ aslref type_satisfaction_illegal_f3.asl
  File type_satisfaction_illegal_f3.asl, line 9, characters 4 to 17:
      invoke_me(x); // illegal as domains doesn't match
      ^^^^^^^^^^^^^
  ASL Type error: a subtype of integer {8, 16} was expected, provided integer.
  [1]

  $ aslref type_satisfaction_illegal_f4.asl
  File type_satisfaction_illegal_f4.asl, line 9, characters 4 to 17:
      invoke_me(x);
      ^^^^^^^^^^^^^
  ASL Type error: a subtype of integer {8, 16} was expected,
    provided integer {8..64}.
  [1]

  $ cat >type-sat3.asl <<EOF
  > func illegal_f5 {N} (b: bits(N))
  > begin
  >   // N is under-constrained integer
  >   var x: integer { 2, 4} = N;
  >    return;
  > end;
  > EOF

  $ aslref type-sat3.asl
  File type-sat3.asl, line 4, characters 2 to 29:
    var x: integer { 2, 4} = N;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: a subtype of integer {2, 4} was expected,
    provided integer {N}.
  [1]

  $ cat >type-sat4.asl <<EOF
  > func invokeMe_2 {N} (b: bits(N))
  > begin
  >   // N is under-constrained integer
  >   var x: integer { 2, 4} = N;
  >   return;
  > end;
  > EOF

  $ aslref type-sat4.asl
  File type-sat4.asl, line 4, characters 2 to 29:
    var x: integer { 2, 4} = N;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: a subtype of integer {2, 4} was expected,
    provided integer {N}.
  [1]

Runtime checks:
  $ cat >runtime-type-sat1.asl <<EOF
  > func main () => integer
  > begin
  >   let x: integer {1} = 2 as integer {1};
  >   return 0;
  > end;
  > EOF

  $ aslref runtime-type-sat1.asl
  File runtime-type-sat1.asl, line 3, characters 23 to 24:
    let x: integer {1} = 2 as integer {1};
                         ^
  ASL Dynamic error: Mismatch type:
    value 2 does not belong to type integer {1}.
  [1]

  $ cat >runtime-type-sat2.asl <<EOF
  > func test(size: integer {3, 4}) begin
  >   let x = Zeros{4} as bits(size);
  > end;
  > func main () => integer begin
  >   test(4);
  >   test(3);
  >   return 0;
  > end;
  > EOF

  $ aslref runtime-type-sat2.asl
  File runtime-type-sat2.asl, line 2, characters 10 to 18:
    let x = Zeros{4} as bits(size);
            ^^^^^^^^
  ASL Dynamic error: Mismatch type:
    value 0x0 does not belong to type bits(size).
  [1]

  $ aslref under-constrained-used.asl

Parameterized integers:
  $ aslref bad-underconstrained-call.asl
  File bad-underconstrained-call.asl, line 9, characters 9 to 26:
    return GetBitAt{M}(x, M);
           ^^^^^^^^^^^^^^^^^
  ASL Type error: a subtype of integer {0..(M - 1)} was expected,
    provided integer {M}.
  [1]
  $ aslref bad-underconstrained-call-02.asl
  File bad-underconstrained-call-02.asl, line 8, characters 2 to 15:
    foo{M}(x, 3);
    ^^^^^^^^^^^^^
  ASL Type error: a subtype of integer {M} was expected, provided integer {3}.
  [1]
  $ aslref bad-underconstrained-call-03.asl
  File bad-underconstrained-call-03.asl, line 8, characters 2 to 19:
    foo{M}(x, M + 1);
    ^^^^^^^^^^^^^^^^^
  ASL Type error: a subtype of integer {M} was expected,
    provided integer {(M + 1)}.
  [1]
  $ aslref bad-underconstrained-ctc.asl
  File bad-underconstrained-ctc.asl, line 3, characters 12 to 13:
    return x[(N as integer {N - 1})];
              ^
  ASL Dynamic error: Mismatch type:
    value 4 does not belong to type integer {(N - 1)}.
  [1]
  $ aslref bad-underconstrained-return.asl
  File bad-underconstrained-return.asl, line 3, characters 2 to 15:
    return N + 1;
    ^^^^^^^^^^^^^
  ASL Type error: a subtype of integer {0..N} was expected,
    provided integer {(N + 1)}.
  [1]
  $ aslref bad-underconstrained-return-02.asl
  File bad-underconstrained-return-02.asl, line 3, characters 2 to 11:
    return 5;
    ^^^^^^^^^
  ASL Type error: a subtype of integer {0..N} was expected,
    provided integer {5}.
  [1]

  $ aslref named-types-in-slices.asl
  0xff

  $ aslref empty-slice.asl
  0x0
  ASL Dynamic error: Cannot extract from bitvector of length 0 slice 4+:-1.
  [1]

  $ aslref bad-slices.asl
  ASL Dynamic error: Cannot extract from bitvector of length 0 slice 4+:-23.
  [1]

  $ aslref bad-shift.asl
  0x00

  $ aslref unreachable.asl
  File unreachable.asl, line 3, characters 2 to 14:
    unreachable;
    ^^^^^^^^^^^^
  ASL Dynamic error: unreachable reached.
  [1]

  $ aslref assign-to-global-immutable.asl
  File assign-to-global-immutable.asl, line 5, characters 2 to 21:
    my_immutable_global = 4;
    ^^^^^^^^^^^^^^^^^^^
  ASL Type error: cannot assign to immutable storage "my_immutable_global".
  [1]

  $ aslref equality.asl
  $ aslref bad-equality.asl
  File bad-equality.asl, line 3, characters 10 to 25:
    println (1, 2) == (1,2);
            ^^^^^^^^^^^^^^^
  ASL Type error: Illegal application of operator == on types
    (integer {1}, integer {2}) and (integer {1}, integer {2}).
  [1]

  $ aslref setter_without_getter.asl
  File setter_without_getter.asl, line 6, characters 0 to 3:
  end;
  ^^^
  ASL Grammar error: Cannot parse.
  [1]

  $ aslref getter_without_setter.asl
  File getter_without_setter.asl, line 6, characters 0 to 3:
  end;
  ^^^
  ASL Grammar error: Cannot parse.
  [1]

  $ aslref tuple_items.asl
  $ aslref cases_where.asl
  $ aslref duplicated-otherwise.asl
  File duplicated-otherwise.asl, line 7, characters 8 to 12:
          when 0.0 => println "2.0";
          ^^^^
  ASL Grammar error: Cannot parse.
  [1]
  $ aslref duplicate_expr_record.asl
  File duplicate_expr_record.asl, line 5, characters 12 to 27:
      var x = A{h = 5, h = 9};
              ^^^^^^^^^^^^^^^
  ASL Type error: cannot declare already declared element "h".
  [1]

  $ aslref same-precedence.asl
  File same-precedence.asl, line 6, characters 10 to 15:
    let x = a + b - c;
            ^^^^^
  ASL Grammar error: Cannot parse. Operators `-` and `+` have the same
    precedence - parenthesise to disambiguate.
  [1]

  $ aslref same-precedence2.asl
  File same-precedence2.asl, line 6, characters 10 to 17:
    let d = a ==> b <=> c;
            ^^^^^^^
  ASL Grammar error: Cannot parse. Operators `<=>` and `==>` have the same
    precedence - parenthesise to disambiguate.
  [1]

  $ aslref rdiv_checks.asl
  File rdiv_checks.asl, line 3, characters 12 to 25:
      var x = 5.3 / "hello";
              ^^^^^^^^^^^^^
  ASL Type error: Illegal application of operator / on types real and string.
  [1]

  $ aslref record-getfields.asl

  $ aslref integer-accessed-bitvector.asl
  File integer-accessed-bitvector.asl, line 4, characters 2 to 3:
    x[0] = '1';
    ^
  ASL Type error: a subtype of bits(-) was expected, provided integer.
  [1]

  $ aslref slice-width-shorthand.asl

Arrays indexed by enumerations
  $ aslref enum-array.asl

  $ aslref array-lca.asl
  $ aslref array-index-error.asl
  ASL Dynamic error: Mismatch type:
    value 14 does not belong to type integer {0..4}.
  [1]

Parameters bugs:
  $ aslref bug1.asl
  File bug1.asl, line 5, characters 21 to 29:
    let foo: bits(x) = Zeros{y};
                       ^^^^^^^^
  ASL Type error: constrained integer expected, provided integer.
  [1]
  $ aslref bug2.asl
  File bug2.asl, line 5, characters 10 to 17:
    let t = y[x: 0];
            ^^^^^^^
  ASL Type error: constrained integer expected, provided integer.
  [1]
  $ aslref bug3.asl
  File bug3.asl, line 4, characters 10 to 18:
    let t = Zeros{x};
            ^^^^^^^^
  ASL Type error: constrained integer expected, provided integer.
  [1]
  $ aslref bug4.asl
  File bug4.asl, line 5, characters 11 to 31:
    let pb = Zeros{a} OR Zeros{b};
             ^^^^^^^^^^^^^^^^^^^^
  ASL Type error: Illegal application of operator OR on types bits(3)
    and bits(4).
  [1]
  $ aslref arg-as-param-call.asl
  File arg-as-param-call.asl, line 8, characters 4 to 21:
      test{10}('1111');
      ^^^^^^^^^^^^^^^^^
  ASL Type error: a subtype of bits(10) was expected, provided bits(4).
  [1]
  $ aslref typed-param-call.asl
  File typed-param-call.asl, line 8, characters 4 to 18:
      test{2}('11');
      ^^^^^^^^^^^^^^
  ASL Type error: a subtype of integer {5..10} was expected,
    provided integer {2}.
  [1]
  $ aslref typed-arg-as-param-call.asl
  File typed-arg-as-param-call.asl, line 8, characters 4 to 18:
      test{2}('11');
      ^^^^^^^^^^^^^^
  ASL Type error: a subtype of integer {5..10} was expected,
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
      if x[0+:4] IN '10x1' then // invalid
                           ^^^^
  ASL Grammar error: Cannot parse.
  [1]
  $ aslref -0 unreachable-v0.asl
  $ aslref assign1.asl
  $ aslref big-ints.asl
  $ aslref bitfields.asl
  $ aslref bitvectors.asl
  $ aslref case.asl
  $ aslref concat-empty.asl
  File concat-empty.asl, line 3, characters 45 to 46:
    let empty_concatenation_should_not_parse = [];
                                               ^
  ASL Grammar error: Cannot parse.
  [1]
  $ aslref concat01.asl
  $ aslref concat02.asl
  $ aslref concat03.asl
  $ aslref constrained-integer-types-example.asl
  $ aslref constrained-types-example.asl
  $ aslref division.asl
  $ aslref exceptions.asl
  File exceptions.asl, line 73, characters 32 to 37:
          when COUNTING => assert FALSE;
                                  ^^^^^
  ASL Dynamic error: Assertion failed: FALSE.
  [1]
  $ aslref func1.asl
  $ aslref func2.asl
  $ aslref func3.asl
  $ aslref func4.asl
  $ aslref func5.asl
  $ aslref func6.asl
  $ aslref func7.asl
  $ aslref global_vars.asl
  $ aslref global_vars-02.asl
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
  $ aslref subtypes-example.asl
  $ aslref subtypes-with.asl
  $ aslref tuples.asl
  $ aslref declaration-primitive-local.asl
  $ aslref --no-type-check -0 typing-assign-v0.asl
  $ aslref constant-functions.asl
  $ aslref alt-mask-syntax.asl
  $ aslref subprogram-global-name-clash.asl
  $ aslref subprogram-local-name-clash.asl
  $ aslref string_concat.asl
  $ aslref approx-expr-binop.asl
  $ aslref asciistr.asl
  $ aslref asl1-calls-asl0-accessor.asl -0 asl0-accessor.asl
  $ aslref --no-exec accessor-overloading-1.asl
  $ aslref accessor-overloading-2.asl
  nullary setter
  unary getter
  unary getter
  unary getter
  unary setter
  $ aslref empty-function.asl
  File empty-function.asl, line 3, characters 0 to 3:
  end;
  ^^^
  ASL Grammar error: Cannot parse.
  [1]

  $ aslref --no-type-check throw-local-env.asl
  File throw-local-env.asl, line 10, characters 13 to 14:
        assert y == 5; // y should not be found in dynamic environment here
               ^
  ASL Dynamic error: Undefined identifier: 'y'
  [1]

  $ aslref undeclared-variable.asl
  File undeclared-variable.asl, line 3, characters 2 to 5:
    bar = (32 - 46) * 0;
    ^^^
  ASL Static error: Undefined identifier: 'bar'
  [1]

  $ aslref --gnu-errors gnu-errors.asl
  aslref: gnu-errors.asl:1:0: ASL Warning: the recursive function fact has no recursive limit annotation.
  aslref: :0:-1: ASL Dynamic error: Mismatch type: value 11 does not belong to type integer {0..9}.
  [1]

  $ aslref
  No files supplied! Run `aslref --help` for information on usage.
  [1]

  $ aslref no-main.asl
  ASL Dynamic error: no entrypoint supplied. Have you defined `func main() =>
    integer`, or did you mean to pass `--no-exec`?
  [1]

  $ aslref main-wrong-type.asl
  ASL Dynamic error: no entrypoint supplied. Have you defined `func main() =>
    integer`, or did you mean to pass `--no-exec`?
  [1]

  $ aslref overloaded-main.asl
  correct main executed

  $ aslref -0 asl0-error-handling-time.asl
  File asl0-error-handling-time.asl, line 2, characters 8 to 9:
    if reg[1] == '1' then 62 else 63 +: 1 field,
          ^
  ASL Static error: Undefined identifier: 'reg'
  [1]

Base values
  $ aslref base_values.asl
  File base_values.asl, line 5, characters 2 to 28:
    var x: integer {N..M, 42};
    ^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: base value of type integer {42, N..M} cannot be symbolically
    reduced since it consists of N.
  [1]

  $ aslref base_values_empty.asl
  File base_values_empty.asl, line 3, characters 2 to 24:
    var x: integer {N..M};
    ^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: base value of type integer {N..M} cannot be symbolically
    reduced since it consists of N.
  [1]

  $ aslref base_values_tuple.asl
  $ aslref base_values_bvs.asl

Getters/setters
  $ aslref nonempty-getter-called-without-slices.asl
  File nonempty-getter-called-without-slices.asl, line 14, characters 10 to 12:
    let x = f1;
            ^^
  ASL Static error: Undefined identifier: 'f1'
  [1]
  $ aslref nonempty-setter-called-without-slices.asl
  File nonempty-setter-called-without-slices.asl, line 14, characters 2 to 4:
    f1 = 4;
    ^^
  ASL Static error: Undefined identifier: 'f1'
  [1]
  $ aslref setter_subfield.asl
  $ aslref setter_subslice.asl
  $ aslref getter_subfield.asl
  $ aslref getter_sub_tuple.asl
  $ aslref getter_subslice.asl
  $ aslref getter_subfields.asl

  $ aslref bad-pattern.asl
  File bad-pattern.asl, line 4, characters 7 to 12:
    when '101' => println ("Cannot happen");
         ^^^^^
  ASL Type error: Erroneous pattern '101' for expression of type integer {3}.
  [1]
  $ aslref pattern-masks-no-braces.asl
  File pattern-masks-no-braces.asl, line 4, characters 19 to 24:
    assert ('111' IN '1xx') == TRUE;
                     ^^^^^
  ASL Grammar error: Cannot parse.
  [1]

ASLRef Field getter extension
  $ aslref --v0-use-field-getter-extension setter_bitfields.asl
  $ aslref --v0-use-field-getter-extension pstate-exp.asl
  $ aslref --no-exec atc-in-types.asl
  $ aslref single-slice.asl

Inherit integer constraints on left-hand sides
  $ aslref inherit-integer-constraints.asl
  $ aslref inherit-integer-constraints-bad-basic.asl
  File inherit-integer-constraints-bad-basic.asl, line 4, characters 2 to 11:
    return x;
    ^^^^^^^^^
  ASL Type error: a subtype of integer {43} was expected,
    provided integer {42}.
  [1]

  $ aslref inherit-integer-constraints-bad-tuple.asl
  File inherit-integer-constraints-bad-tuple.asl, line 4, characters 2 to 28:
    return (y.item0, y.item2);
    ^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: a subtype of (integer {42}, integer {0}) was expected,
    provided (integer {42}, integer {43}).
  [1]

  $ aslref inherit-integer-constraints-bad-type.asl
  File inherit-integer-constraints-bad-type.asl, line 1, character 0 to line 4,
    character 2:
  type badtype of record {
      a : integer{},
      c : integer
  };
  ASL Type error: a pending constrained integer is illegal here.
  [1]

Left-hand sides
  $ aslref lhs-tuple-fields.asl
  $ aslref lhs-tuple-fields-same-field.asl
  File lhs-tuple-fields-same-field.asl, line 8, characters 2 to 4:
    bv.(fld, -, fld) = ('11', TRUE, '11');
    ^^
  ASL Grammar error: multiple writes to "bv.fld".
  [1]
  $ aslref lhs-tuple-same-var.asl
  $ aslref lhs-expressivity.asl

Outdated syntax
  $ aslref noreturn.asl
  File noreturn.asl, line 31, character 0 to line 34, character 4:
  noreturn func rec_noreturning()
  begin
      rec_noreturning();
  end;
  ASL Warning: the recursive function rec_noreturning has no recursive limit
  annotation.
  $ aslref noreturn_function.asl
  File noreturn_function.asl, line 2, characters 26 to 28:
  noreturn func returning() => integer
                            ^^
  ASL Grammar error: Cannot parse.
  [1]

Bounds checks
  $ aslref bounds-checks-read-bitvector-1.asl
  ASL Dynamic error: Cannot extract from bitvector of length 0 slice -1+:1.
  [1]
  $ aslref bounds-checks-read-bitvector-2.asl
  ASL Dynamic error: Mismatch type:
    value 4 does not belong to type integer {0..3}.
  [1]
  $ aslref bounds-checks-write-bitvector-1.asl
  ASL Dynamic error: Cannot extract from bitvector of length 0 slice -1+:1.
  [1]
  $ aslref bounds-checks-write-bitvector-2.asl
  ASL Dynamic error: Mismatch type:
    value 5 does not belong to type integer {0..3}.
  [1]
  $ aslref bounds-checks-read-array-1.asl
  ASL Dynamic error: Mismatch type:
    value -1 does not belong to type integer {0..3}.
  [1]
  $ aslref bounds-checks-read-array-2.asl
  ASL Dynamic error: Mismatch type:
    value 4 does not belong to type integer {0..3}.
  [1]
  $ aslref bounds-checks-write-array-1.asl
  ASL Dynamic error: Mismatch type:
    value -1 does not belong to type integer {0..3}.
  [1]
  $ aslref bounds-checks-write-array-2.asl
  ASL Dynamic error: Mismatch type:
    value 4 does not belong to type integer {0..3}.
  [1]
  $ aslref bounds-checks-read-zero-width-slice.asl
  ASL Dynamic error: Mismatch type:
    value 100 does not belong to type integer {0..3}.
  [1]
  $ aslref bounds-checks-write-zero-width-slice.asl
  ASL Dynamic error: Mismatch type:
    value 100 does not belong to type integer {0..3}.
  [1]
