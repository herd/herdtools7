Hello world should work:

  $ aslref hello_world.asl
  Hello, world!

ASL Typing Tests:
  $ aslref TypingRule.SubtypeSatisfaction1.asl
  $ aslref --no-exec TypingRule.SubtypeSatisfaction2.asl
  $ aslref TypingRule.SubtypeSatisfaction3.asl
  File TypingRule.SubtypeSatisfaction3.asl, line 9, characters 4 to 45:
      var dogLegs : AnimalLegs = myCircleSides; // illegal: unrelated types
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: a subtype of AnimalLegs was expected, provided ShapeSides.
  [1]
  $ aslref TypingRule.SubtypeSatisfaction.bad1.asl
  File TypingRule.SubtypeSatisfaction.bad1.asl, line 8, characters 0 to 31:
  var x : integer{Int12} = Int12;
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: a subtype of integer {2} was expected,
    provided integer {1..2}.
  [1]
  $ aslref TypingRule.SubtypeSatisfaction.bad2.asl
  File TypingRule.SubtypeSatisfaction.bad2.asl, line 7, characters 4 to 13:
      return N;
      ^^^^^^^^^
  ASL Type error: a subtype of integer {N} was expected,
    provided integer {2, 4}.
  [1]
  $ aslref TypingRule.TypeSatisfaction1.asl
  $ aslref TypingRule.TypeSatisfaction2.asl
  $ aslref TypingRule.TypeSatisfaction3.asl
  File TypingRule.TypeSatisfaction3.asl, line 14, characters 2 to 6:
    pair = (1, dataT2);
    ^^^^
  ASL Type error: a subtype of pairT was expected, provided (integer {1}, T2).
  [1]
  $ aslref TypingRule.TypeSatisfaction.bad1.asl
  File TypingRule.TypeSatisfaction.bad1.asl, line 3, characters 4 to 25:
      var a: integer{0..N};
      ^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: base value of type integer {0..N} cannot be statically
    determined since it consists of N.
  [1]
  $ aslref --no-exec TypingRule.TypeClashes.asl
  $ aslref --no-exec TypingRule.TypeClashes.bad.asl
  File TypingRule.TypeClashes.bad.asl, line 3, characters 0 to 55:
  func structured_procedure(r: SuperRec) begin pass; end;
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: cannot declare already declared element
    "structured_procedure".
  [1]
  $ aslref TypingRule.LowestCommonAncestor.asl
  $ aslref --no-exec TypingRule.LowestCommonAncestor2.asl
  $ aslref TypingRule.FindNamedLCA.asl
  $ aslref TypingRule.ApplyUnopType.asl
  $ aslref TypingRule.EConcatUnresolvableToInteger.asl
  File TypingRule.EConcatUnresolvableToInteger.asl, line 12, character 4 to
    line 14, character 8:
      while ret < LIMIT1 do
          ret = ret + ret * 2;
      end;
  ASL Warning: Loop does not have a limit.
  $ aslref TypingRule.ApplyBinopTypes.asl
  $ aslref --no-exec TypingRule.ApplyBinopTypes.constraints.asl
  File TypingRule.ApplyBinopTypes.constraints.asl, line 22, characters 51 to 78:
      var a_div : integer{A, (A DIV 2), (A DIV 3)} = a DIV (1 as integer{-5..3});
                                                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Warning: Removing some values that would fail with op DIV from constraint set
  {-5..3} gave {1..3}. Continuing with this constraint set.
  File TypingRule.ApplyBinopTypes.constraints.asl, line 26, characters 39 to 65:
      var a_mod_0_to_3 : integer{0..2} = a MOD (1 as integer{0..3});
                                         ^^^^^^^^^^^^^^^^^^^^^^^^^^
  Warning: Removing some values that would fail with op MOD from constraint set
  {0..3} gave {1..3}. Continuing with this constraint set.
  File TypingRule.ApplyBinopTypes.constraints.asl, line 42, characters 31 to 82:
      var y : integer{0..2^14} = (1 as integer{0..2^14}) DIV (2 as integer{0..2^14});
                                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Warning: Removing some values that would fail with op DIV from constraint set
  {0..16384} gave {1..16384}. Continuing with this constraint set.
  $ aslref --no-exec TypingRule.ApplyBinopTypes.constraints2.asl
  File TypingRule.ApplyBinopTypes.constraints2.asl, line 6, characters 12 to 19:
      let z = x DIV y;
              ^^^^^^^
  Warning: Removing some values that would fail with op DIV from constraint set
  {-1..1} gave {1..1}. Continuing with this constraint set.
  $ aslref TypingRule.LDDiscard.asl
  File TypingRule.LDDiscard.asl, line 4, characters 6 to 7:
    let - = 42;
        ^
  ASL Grammar error: Obsolete syntax: Discarded storage declaration.
  [1]
  $ aslref TypingRule.LDVar.asl
  $ aslref TypingRule.LDTyped.asl
  $ aslref TypingRule.LDTuple.asl
  $ aslref TypingRule.Lit.asl
  $ aslref TypingRule.CheckCommonBitfieldsAlign.Error.asl
  File TypingRule.CheckCommonBitfieldsAlign.Error.asl, line 1, character 20 to
    line 6, character 1:
  type Nested_Type of bits(2) {
      [1:0] sub {
          [0,1] common
      },
      [1:0] common
  };
  ASL Type error:
    bitfields `sub.common` and `common` are in the same scope but define different slices of the containing bitvector type: [0, 1] and [1:0], respectively.
  [1]

ASL Typing Tests / annotating types:
  $ aslref TypingRule.TReal.asl
  $ aslref TypingRule.TBool.asl
  $ aslref TypingRule.TNamed.asl
  $ aslref TypingRule.TNamed.bad1.asl
  File TypingRule.TNamed.bad1.asl, line 11, characters 4 to 13:
      foo(x.f); // Illegal: x.f is of type TypeB which does not type-satisfy TypeA.
      ^^^^^^^^^
  ASL Type error: a subtype of TypeA was expected, provided TypeB.
  [1]
  $ aslref TypingRule.TNamed.bad2.asl
  File TypingRule.TNamed.bad2.asl, line 12, characters 4 to 13:
      foo(y.f); // illegal: y.f is of type TypeB which does not type-satisfy TypeA.
      ^^^^^^^^^
  ASL Type error: a subtype of TypeA was expected, provided TypeB.
  [1]
  $ aslref TypingRule.TIntUnconstrained.asl
  $ aslref TypingRule.TIntWellConstrained.asl
  $ aslref TypingRule.TIntParameterized.asl
  $ aslref TypingRule.InheritIntegerConstraints.asl
  $ aslref TypingRule.InheritIntegerConstraints.unconstrained.bad.asl
  File TypingRule.InheritIntegerConstraints.unconstrained.bad.asl, line 5,
    characters 4 to 26:
      var g : integer{} = a;
      ^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: constrained integer expected, provided integer.
  [1]

  $ aslref --no-exec TypingRule.TInt.config_pending_constrained.bad.asl
  File TypingRule.TInt.config_pending_constrained.bad.asl, line 1,
    characters 0 to 26:
  config x : integer{} =  1;
  ^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: a pending constrained integer is illegal here.
  [1]

  $ aslref TypingRule.TInt.rhs_pending_constrained.bad.asl
  File TypingRule.TInt.rhs_pending_constrained.bad.asl, line 5,
    characters 28 to 42:
      var x : integer{1..2} = 3 as integer{};
                              ^^^^^^^^^^^^^^
  ASL Type error: a pending constrained integer is illegal here.
  [1]

  $ aslref TypingRule.AnnotateConstraint.asl
  $ aslref TypingRule.AnnotateConstraint.bad.asl
  File TypingRule.AnnotateConstraint.bad.asl, line 4, characters 17 to 18:
    let t: integer{x..x+1} = 2; // illegal as 'x' is not symbolically evaluable.
                   ^
  ASL Type error: expected a symbolically evaluable expression/subprogram.
  [1]

  $ aslref TypingRule.TBits.asl
  $ aslref TypingRule.TBits.bad.asl
  File TypingRule.TBits.bad.asl, line 3, characters 16 to 17:
      var R: bits(I); // Illegal since I is unconstrained.
                  ^
  ASL Type error: constrained integer expected, provided integer.
  [1]
  $ aslref TypingRule.TTuple.asl
  $ aslref TypingRule.TArray.asl
  $ aslref TypingRule.TArray.bad.asl
  File TypingRule.TArray.bad.asl, line 9, characters 32 to 58:
      var illegal_array1: array [[non_symbolically_evaluable]] of integer;
                                  ^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: expected a symbolically evaluable expression/subprogram.
  [1]
  $ aslref TypingRule.TArray.bad2.asl
  File TypingRule.TArray.bad2.asl, line 5, characters 4 to 61:
      var illegal_array2: array [[non_constrained]] of integer;
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: constrained integer expected, provided integer.
  [1]
  $ aslref TypingRule.AnnotateSymbolicallyEvaluableExpr.asl
  $ aslref --no-exec TypingRule.TEnumDecl.asl
  $ aslref --no-exec TypingRule.TEnumDecl.subtypes.asl
  $ aslref --no-exec TypingRule.TEnumDecl.bad.asl
  File TypingRule.TEnumDecl.bad.asl, line 1, characters 0 to 19:
  constant GREEN = 1;
  ^^^^^^^^^^^^^^^^^^^
  ASL Type error: cannot declare already declared element "GREEN".
  [1]
  $ aslref --no-exec TypingRule.TEnumDecl.bad2.asl
  File TypingRule.TEnumDecl.bad2.asl, line 4, characters 0 to 67:
  type SubEnumIllegal1 of enumeration {LOW, HIGH} subtypes SuperEnum;
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: cannot declare already declared element "LOW".
  [1]
  $ aslref --no-exec TypingRule.TEnumDecl.bad3.asl
  File TypingRule.TEnumDecl.bad3.asl, line 5, characters 0 to 69:
  type SubEnumIllegal2 of enumeration {TOP, BOTTOM} subtypes SuperEnum;
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: a subtype of SuperEnum was expected,
    provided enumeration {TOP, BOTTOM}.
  [1]
  $ aslref --no-exec TypingRule.TEnumDecl.bad4.asl
  File TypingRule.TEnumDecl.bad4.asl, line 1, characters 0 to 49:
  type Color of enumeration { GREEN, ORANGE, RED };
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: cannot declare already declared element "RED".
  [1]
  $ aslref --no-exec TypingRule.Subtype.asl
  $ aslref --no-exec TypingRule.GetVariableEnum.asl
  $ aslref TypingRule.TRecordDecl.asl
  $ aslref TypingRule.TRecordDecl.bad.asl
  File TypingRule.TRecordDecl.bad.asl, line 1, characters 0 to 58:
  type MyRecord of record {v: integer, b: boolean, v: real};
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: cannot declare already declared element "v".
  [1]
  $ aslref TypingRule.TExceptionDecl.asl
  $ aslref TypingRule.TCollection.asl
  $ aslref TypingRule.TNonDecl.asl
  File TypingRule.TNonDecl.asl, line 1, characters 5 to 6:
  func (x: record { a: integer, b: boolean }) => integer
       ^
  ASL Error: Cannot parse.
  [1]
  $ aslref TypingRule.TBitField.asl
  $ aslref --no-exec TypingRule.AnnotateFuncSig.asl
  $ aslref --no-exec TypingRule.AnnotateFuncSig.bad.asl
  File TypingRule.AnnotateFuncSig.bad.asl, line 4, characters 60 to 63:
  func signature_example(bv: bits(8)) => bits(16) recurselimit(W)
                                                              ^^^
  ASL Type error: expected a symbolically evaluable expression/subprogram.
  [1]
  $ aslref --no-exec TypingRule.ExtractParameters-bad1.asl
  File TypingRule.ExtractParameters-bad1.asl, line 3, characters 15 to 36:
      arg0: bits(N as integer{8,16,32}),
                 ^^^^^^^^^^^^^^^^^^^^^
  ASL Static Error: Unsupported expression N as integer {8, 16, 32}.
  [1]
  $ aslref TypingRule.BuiltinAggregateTypes.asl
  $ aslref --no-exec TypingRule.BuiltinExceptionType.asl
  $ aslref TypingRule.BuiltinSingularTypes.asl
  $ aslref TypingRule.EnumerationType.asl
  $ aslref TypingRule.TString.asl
  $ aslref TypingRule.ConstraintMod.bad.asl
  File TypingRule.ConstraintMod.bad.asl, line 9, characters 4 to 5:
      z = 3; // Illegal: the type inferred for z is integer{0..2}
      ^
  ASL Type error: a subtype of integer {0..2} was expected,
    provided integer {3}.
  [1]
  $ aslref --no-exec TypingRule.CheckConstrainedInteger.asl

  $ aslref TypingRule.BaseValue.asl
  global_base = 0, unconstrained_integer_base = 0, constrained_integer_base = -3
  bool_base = FALSE, real_base = 0, string_base = , enumeration_base = RED
  bits_base = 0x00
  tuple_base = (0, -3, RED)
  record_base      = {data=0x00, time=0, flag=FALSE}
  record_base_init = {data=0x00, time=0, flag=FALSE}
  exception_base = {msg=}
  integer_array_base = [[0, 0, 0, 0]]
  enumeration_array_base = [[RED=0, GREEN=0, BLUE=0]]
  $ aslref --no-exec TypingRule.BaseValue.parameterized.asl
  $ aslref TypingRule.BaseValue.bad_negative_width.asl
  File TypingRule.BaseValue.bad_negative_width.asl, line 1, characters 0 to 24:
  var bits_base: bits(-3);
  ^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: base value of empty type bits((- 3)).
  [1]
  $ aslref TypingRule.BaseValue.bad_empty.asl
  File TypingRule.BaseValue.bad_empty.asl, line 1, characters 0 to 22:
  var x : integer{5..0};
  ^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: base value of empty type integer {5..0}.
  [1]

  $ aslref TypingRule.UnopLiterals.asl
  negate_int: -10 = -10
  negate_int: -0x0 = 0
  negate_int: -0xf = -15
  negate_rel: -2.3 = -23/10
  not_bool: !TRUE = FALSE
  not_bits: NOT '' = 0x
  not_bits: NOT '11 01' = 0x2

  $ aslref TypingRule.BinopLiterals.boolean.asl
  and_bool: TRUE && TRUE = TRUE
  and_bool: TRUE && FALSE = FALSE
  or_bool: TRUE || FALSE = TRUE
  or_bool: FALSE || FALSE = FALSE
  eq_bool: FALSE == FALSE = TRUE
  eq_bool: TRUE == TRUE = TRUE
  eq_bool: FALSE == TRUE = FALSE
  ne_bool: FALSE != FALSE = FALSE
  ne_bool: TRUE != TRUE = FALSE
  ne_bool: FALSE != TRUE = TRUE
  implies_bool: FALSE ==> FALSE = TRUE
  implies_bool: FALSE ==> TRUE = TRUE
  implies_bool: TRUE ==> TRUE = TRUE
  implies_bool: TRUE ==> FALSE = FALSE
  equiv_bool: FALSE <=> FALSE = TRUE
  equiv_bool: TRUE <=> TRUE = TRUE
  equiv_bool: FALSE <=> TRUE = FALSE

  $ aslref TypingRule.BinopLiterals.integer-arithmetic.asl
  add_int: 10 + 20 = 30
  sub_int: 10 - 20 = -10
  mul_int: 10 * 20 = 200
  div_int: 20 DIV 10 = 2
  fdiv_int: 20 DIVRM 3 = 6
  fdiv_int: -20 DIVRM 3 = -7
  frem_int: 20 MOD 3 = 2
  frem_int: -20 MOD 3 = 1
  exp_int: 2 ^ 10 = 1024
  exp_int: -2 ^ 10 = 1024
  exp_int: -2 ^ 11 = -2048
  exp_int: 0 ^ 0 = 1
  exp_int: -2 ^ 0 = 1
  shiftleft_int: 1 << 10 = 1024
  shiftleft_int: 1 << 0 = 1
  shiftleft_int: -1 << 10 = -1024
  shiftright_int: 1 >> 10 = 0
  shiftright_int: 16 >> 2 = 4
  shiftright_int: -16 >> 2 = -4
  shiftright_int: 1 >> 0 = 1
  shiftright_int: -1 >> 10 = -1

  $ aslref TypingRule.BinopLiterals.integer-relational.asl
  eq_int: 5 == 10 = FALSE
  ne_int: 5 != 10 = TRUE
  le_int: 10 <= 10 = TRUE
  lt_int: 10 < 10 = TRUE
  lt_int: 5 < 10 = TRUE
  gt_int: 10 > 10 = FALSE
  gt_int: 11 > 10 = TRUE
  ge_int: 11 >= 10 = TRUE
  ge_int: 6 >= 10 = FALSE

  $ aslref TypingRule.BinopLiterals.real.asl
  mul_int_real: 10 * 0.5 = 5
  mul_real_int: 0.5 * 10 = 5
  add_real: 10.0 + 0.5 = 21/2
  sub_real: 10.0 - 0.5 = 19/2
  mul_real: 10.0 * 0.5 = 5
  exp_real: 10.0 ^ 2 = 100
  div_real: 10.0 / 0.5 = 20
  eq_real: 10.0 == 0.5 = FALSE
  ne_real: 10.0 != 0.5 = TRUE
  le_real: 10.0 <= 0.5 = FALSE
  lt_real: 10.0 < 0.5 = FALSE
  gt_real: 10.0 > 0.5 = TRUE
  ge_real: 10.0 >= 0.5 = TRUE

  $ aslref TypingRule.BinopLiterals.bits.asl
  add_bits: '010' + '011' = 0x5
  add_bits: '10' + '11' = 0x1
  add_bits: '010' + 3 = 0x5
  add_bits: '10' + 3 = 0x1
  sub_bits: '100' - '010' = 0x2
  sub_bits: '100' - '111' = 0x5
  sub_bits: '100' - 7 = 0x5
  sub_bits: '100' - 8 = 0x4
  and_bits: '100' AND '111' = 0x4
  or_bits: '100' OR '110' = 0x6
  xor_bits: '100' XOR '110' = 0x2
  eq_bits: '100' == '110' = FALSE
  ne_bits: '100' != '110' = TRUE
  concat_bits: '100' :: '110' = 0x26
  concat_bits: '100' :: '' = 0x4

  $ aslref TypingRule.BinopLiterals.strings_labels.asl
  eq_string: "hello" == "world" = FALSE
  eq_string: "hello" == "hello" = TRUE
  ne_string: "hello" != "world" = TRUE
  eq_enum: RED == RED = TRUE
  eq_enum: RED == GREEN = FALSE
  eq_enum: RED != RED = FALSE
  eq_enum: RED != GREEN = TRUE
  concat_string: 0 :: '1' :: 2.0 :: TRUE :: "foo" :: RED = 00x12TRUEfooRED

  $ aslref TypingRule.EVar.asl
  $ aslref TypingRule.EVar.undefined.asl
  File TypingRule.EVar.undefined.asl, line 3, characters 12 to 13:
      var x = t;
              ^
  ASL Error: Undefined identifier: 't'
  [1]

  $ aslref TypingRule.EGetRecordField.asl
  $ aslref TypingRule.EGetBadRecordField.asl
  File TypingRule.EGetBadRecordField.asl, line 7, characters 10 to 36:
    var x = my_record.undeclared_field;
            ^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Error: There is no field 'undeclared_field' on type MyRecordType.
  [1]
  $ aslref TypingRule.EGetBitfield.asl
  $ aslref TypingRule.EGetBadBitField.asl
  File TypingRule.EGetBadBitField.asl, line 7, characters 12 to 33:
      var x = p.undeclared_bitfield;
              ^^^^^^^^^^^^^^^^^^^^^
  ASL Error: There is no field 'undeclared_bitfield' on type Packet.
  [1]
  $ aslref TypingRule.EGetBadField.asl
  File TypingRule.EGetBadField.asl, line 6, characters 12 to 15:
      var x = a.f;
              ^^^
  ASL Error: There is no field 'f' on type array [[5]] of integer.
  [1]
  $ aslref TypingRule.EGetFields.asl
  $ aslref --no-exec TypingRule.ATC.asl
  $ aslref --no-exec TypingRule.ATC2.asl
  $ aslref --no-exec TypingRule.ATC3.asl
  $ aslref --no-exec TypingRule.ATC4.asl
  $ aslref --no-exec TypingRule.ATC.bad.asl
  File TypingRule.ATC.bad.asl, line 4, characters 21 to 33:
      let B: integer = A as integer; // Illegal: bit cannot be an integer.
                       ^^^^^^^^^^^^
  ASL Type error: cannot perform Asserted Type Conversion on bits(1) by
    integer.
  [1]
  $ aslref --no-exec TypingRule.CheckATC.asl
  File TypingRule.CheckATC.asl, line 8, characters 12 to 32:
      var a = 3.0 as integer{1, 2};
              ^^^^^^^^^^^^^^^^^^^^
  ASL Type error: cannot perform Asserted Type Conversion on real by
    integer {1, 2}.
  [1]
  $ aslref --no-exec TypingRule.IsGlobalUndefined.asl
  $ aslref TypingRule.EArbitrary.asl
  $ aslref TypingRule.StaticEval.asl
  $ aslref TypingRule.StaticEval.bad.asl
  File TypingRule.StaticEval.bad.asl, line 3, characters 5 to 20: Division will
  result in empty constraint set, so will always fail.
  File TypingRule.StaticEval.bad.asl, line 3, characters 5 to 20:
      [WORD_SIZE DIV 3 - 1:WORD_SIZE DIV 2] upper,
       ^^^^^^^^^^^^^^^
  ASL Type error: Illegal application of operator DIV on types integer {64}
    and integer {3}.
  [1]
  $ aslref TypingRule.Catcher.asl
  ExceptionType2 : x=2, g= 1
  $ aslref TypingRule.FindCatcher.None.asl
  ExceptionType2 : x=2, g= 1
  $ aslref TypingRule.LEDiscard.asl
  $ aslref TypingRule.LEVar.asl
  $ aslref TypingRule.LEVar.undefined.asl
  File TypingRule.LEVar.undefined.asl, line 3, characters 4 to 5:
      x = 42;
      ^
  ASL Error: Undefined identifier: 'x'
  [1]
  $ aslref TypingRule.LESetBadField.asl
  File TypingRule.LESetBadField.asl, line 6, characters 4 to 5:
      x.RED = 42;
      ^
  ASL Type error: array [[Color]] of integer does not subtype any of: bits(-),
    record {  }, exception {  }, collection {  }.
  [1]
  $ aslref TypingRule.LESetBadField.asl
  File TypingRule.LESetBadField.asl, line 6, characters 4 to 5:
      x.RED = 42;
      ^
  ASL Type error: array [[Color]] of integer does not subtype any of: bits(-),
    record {  }, exception {  }, collection {  }.
  [1]
  $ aslref TypingRule.LESetStructuredField.asl
  $ aslref TypingRule.LESetField.asl
  $ aslref TypingRule.LESetFields.asl
  $ aslref TypingRule.LESlice.bad.asl
  File TypingRule.LESlice.bad.asl, line 4, characters 3 to 11:
    x[3:0, 3] = '0 0000';
     ^^^^^^^^
  ASL Static error: overlapping slices 0+:4, 3+:1.
  [1]
  $ aslref --no-exec TypingRule.DeclareGlobalStorage.asl
  $ aslref --no-exec TypingRule.SCond.asl
  $ aslref TypingRule.SDecl.asl
  $ aslref TypingRule.SDecl.bad1.asl
  File TypingRule.SDecl.bad1.asl, line 4, characters 4 to 12:
      constant c3 = 5;
      ^^^^^^^^
  ASL Grammar error: Obsolete syntax: Local constant declaration.
  [1]
  $ aslref TypingRule.SDecl.bad2.asl
  File TypingRule.SDecl.bad2.asl, line 4, characters 18 to 19:
      let y: integer;
                    ^
  ASL Error: Cannot parse.
  [1]
  $ aslref TypingRule.SAssert.bad.asl
  File TypingRule.SAssert.bad.asl, line 11, characters 10 to 23:
      assert(increment()); // Illegal, since increment is not readonly.
            ^^^^^^^^^^^^^
  ASL Type error: expected a readonly expression/subprogram.
  [1]
  $ aslref TypingRule.SWhile.asl
  File TypingRule.SWhile.asl, line 23, character 4 to line 29, character 8:
      while i < 20 do
          assert i < 20;
          if x[i] == '1' then
              ones = ones + 1;
          end;
          i = i + 1;
      end;
  ASL Warning: Loop does not have a limit.
  20
  20
  $ aslref TypingRule.SWhile.bad_limit.asl
  File TypingRule.SWhile.bad_limit.asl, line 8, characters 26 to 33:
      while i < N looplimit i_limit do
                            ^^^^^^^
  ASL Type error: expected a symbolically evaluable expression/subprogram.
  [1]
  $ aslref TypingRule.SFor.bad1.asl
  File TypingRule.SFor.bad1.asl, line 5, character 4 to line 7, character 8:
      for i = 0 to 4 do
          pass;
      end;
  ASL Type error: cannot declare already declared element "i".
  [1]
  $ aslref TypingRule.SFor.bad2.asl
  File TypingRule.SFor.bad2.asl, line 5, characters 8 to 9:
          i = i + 1;
          ^
  ASL Type error: cannot assign to immutable storage "i".
  [1]
  $ aslref TypingRule.SFor.bad3.asl
  File TypingRule.SFor.bad3.asl, line 7, characters 4 to 5:
      j = 0; // Illegal: 'j' is in scope only in the loop body.
      ^
  ASL Error: Undefined identifier: 'j'
  [1]
  $ aslref TypingRule.SFor.bad4.asl
  File TypingRule.SFor.bad4.asl, line 11, characters 17 to 30:
      for j = 0 to upper_bound() do
                   ^^^^^^^^^^^^^
  ASL Type error: expected a readonly expression/subprogram.
  [1]
  $ aslref TypingRule.SReturn.bad.asl
  File TypingRule.SReturn.bad.asl, line 3, characters 4 to 13:
      return 0;
      ^^^^^^^^^
  ASL Type error: cannot return something from a procedure.
  [1]
  $ aslref --no-exec TypingRule.SPragma.asl
  File TypingRule.SPragma.asl, line 3, characters 4 to 39:
      pragma implementation_hidden x + 1;
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Warning: pragma implementation_hidden will be ignored.
  $ aslref --no-exec TypingRule.BitfieldSliceToPositions.asl
  $ aslref TypingRule.DisjointSlicesToPositions.bad.asl
  File TypingRule.DisjointSlicesToPositions.bad.asl, line 1, character 0 to
    line 6, character 2:
  var myData: bits(16) {
      [4] flag,
      // Illegal: slices declared for the same bitfield must not overlap
      [3:0, 5:3] data,
      [3*:4] value
  };
  ASL Static error: overlapping slices 0+:4, 3+:3.
  [1]
  $ aslref --no-exec TypingRule.CheckPositionsInWidth.bad.asl
  File TypingRule.CheckPositionsInWidth.bad.asl, line 1, character 0 to line 5,
    character 2:
  var myData: bits(16) {
      [4] flag,
      [3:0, 5+:3] data,
      [3*:5] value // Illegal: position 19 exceeds 15
  };
  ASL Static error: Cannot extract from bitvector of length 16 slice (3 * 5)+:5.
  [1]

  $ aslref TypingRule.CheckNoPrecisionLoss.asl
  File TypingRule.CheckNoPrecisionLoss.asl, line 3, characters 8 to 13:
  var b = a * a;
          ^^^^^
  Exploding sets for the binary operation * could result in a constraint set
  bigger than 2^17 with constraints 1..1024 and 1..1024. Continuing with the
  non-expanded constraints.
  File TypingRule.CheckNoPrecisionLoss.asl, line 3, characters 0 to 14:
  var b = a * a;
  ^^^^^^^^^^^^^^
  ASL Type error: type used to define storage item is the result of precision
    loss.
  [1]
  $ aslref TypingRule.PrecisionJoin.asl
  File TypingRule.PrecisionJoin.asl, line 3, characters 9 to 14:
  var b = (a * a) + 2;
           ^^^^^
  Exploding sets for the binary operation * could result in a constraint set
  bigger than 2^17 with constraints 1..1024 and 1..1024. Continuing with the
  non-expanded constraints.
  File TypingRule.PrecisionJoin.asl, line 3, characters 0 to 20:
  var b = (a * a) + 2;
  ^^^^^^^^^^^^^^^^^^^^
  ASL Type error: type used to define storage item is the result of precision
    loss.
  [1]

  $ aslref TypingRule.PSingle.asl
  $ aslref TypingRule.PSingle.bad.asl
  File TypingRule.PSingle.bad.asl, line 4, characters 11 to 30:
      assert '101' IN { '1100' };
             ^^^^^^^^^^^^^^^^^^^
  ASL Type error: cannot find a common ancestor to those two types bits(3) and
    bits(4).
  [1]
  $ aslref TypingRule.PRange.asl
  $ aslref TypingRule.PRange.bad.asl
  File TypingRule.PRange.bad.asl, line 4, characters 11 to 32:
      assert 42.4 IN { -1.8..143 };
             ^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: Erroneous pattern (- (9.0 / 5.0)) .. 143 for expression of
    type real.
  [1]
  $ aslref TypingRule.PLeq.asl
  $ aslref TypingRule.PLeq.bad.asl
  File TypingRule.PLeq.bad.asl, line 4, characters 12 to 28:
       assert 3 IN { <= 42.0 };
              ^^^^^^^^^^^^^^^^
  ASL Type error: Erroneous pattern <= (42.0 / 1.0) for expression of type
    integer {3}.
  [1]
  $ aslref TypingRule.PGeq.asl
  $ aslref TypingRule.PGeq.bad.asl
  File TypingRule.PGeq.bad.asl, line 4, characters 11 to 27:
      assert 42 IN { >= 3.0 };
             ^^^^^^^^^^^^^^^^
  ASL Type error: Erroneous pattern >= (3.0 / 1.0) for expression of type
    integer {42}.
  [1]
  $ aslref TypingRule.PTuple.bad.asl
  File TypingRule.PTuple.bad.asl, line 8, characters 11 to 45:
      assert (3, '101010') IN { ('xx1010', 5) };
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: a subtype of bits(-) was expected, provided integer {3}.
  [1]
  $ aslref TypingRule.PMask.asl
  $ aslref TypingRule.PMask.bad.asl
  File TypingRule.PMask.bad.asl, line 5, characters 11 to 34:
      assert '101010' IN {'xx10101'};
             ^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: a subtype of bits(7) was expected, provided bits(6).
  [1]
  $ aslref TypingRule.PAny.asl
  $ aslref TypingRule.PAny.bad.asl
  File TypingRule.PAny.bad.asl, line 5, characters 11 to 29:
      assert TRUE IN {FALSE, 5};
             ^^^^^^^^^^^^^^^^^^
  ASL Type error: Erroneous pattern 5 for expression of type boolean.
  [1]

  $ aslref TypingRule.CheckIsNotCollection.asl
  File TypingRule.CheckIsNotCollection.asl, line 3, characters 12 to 22:
    var test: collection {
              ^^^^^^^^^^
  ASL Error: Cannot parse.
  [1]
  $ aslref TypingRule.LESetCollectionFields.asl
  $ aslref TypingRule.TypecheckDecl.asl
  0x0000000000000000
  0xffffffffffffffff
  $ aslref TypingRule.Subprogram.asl
  0x04
  $ aslref --no-exec TypingRule.CheckControlFlow.noreturn.asl
  $ aslref --no-exec TypingRule.CheckControlFlow.asl
  $ aslref TypingRule.CheckControlFlow.bad2.asl
  File TypingRule.CheckControlFlow.bad2.asl, line 6, character 4 to line 16,
    character 8:
      if v != Zeros{N} then
          if flag then
              return Ones{N} XOR v;
          end;
      else
          if flag then
              return v;
          else
              throw invalid_state{-};
          end;
      end;
  ASL Type error:
    not all control flow paths of the function "incorrect_terminating_path" are
    guaranteed to either return, raise an exception, or invoke unreachable.
  [1]
  $ aslref TypingRule.CheckControlFlow.bad3.asl
  ASL Type error: the function "returning" is qualified with noreturn but may
    return on some control flow path.
  [1]
  $ aslref TypingRule.ApproxStmt.asl
  File TypingRule.ApproxStmt.asl, line 8, characters 4 to 30:
      pragma require_positive x;
      ^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Warning: pragma require_positive will be ignored.
  $ aslref TypingRule.ApproxStmt.bad1.asl
  File TypingRule.ApproxStmt.bad1.asl, line 8, character 4 to line 10,
    character 8:
      while (TRUE) do
          pass;
      end;
  ASL Warning: Loop does not have a limit.
  File TypingRule.ApproxStmt.bad1.asl, line 8, character 4 to line 10,
    character 8:
      while (TRUE) do
          pass;
      end;
  ASL Type error: not all control flow paths of the function "loop_forever" are
    guaranteed to either return, raise an exception, or invoke unreachable.
  [1]
  $ aslref --no-exec TypingRule.DeclareType.asl
  $ aslref TypingRule.AnnotateExtraFields.bad.asl
  File TypingRule.AnnotateExtraFields.bad.asl, line 1, characters 15 to 39:
  type SubRecord subtypes Record with {-};
                 ^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Error: Undefined identifier: 'Record'
  [1]
  $ aslref --no-exec TypingRule.DeclaredType.asl
  $ aslref --no-exec TypingRule.DeclaredType.bad.asl
  File TypingRule.DeclaredType.bad.asl, line 3, characters 12 to 23:
      var x = 20 as MyInt;
              ^^^^^^^^^^^
  ASL Error: Undefined identifier: 'MyInt'
  [1]
  $ aslref --no-exec TypingRule.DeclareConst.asl
  $ aslref --no-exec TypingRule.DeclareGlobalStorage.config.asl
  $ aslref --no-exec TypingRule.DeclareGlobalStorage.bad1.asl
  File TypingRule.DeclareGlobalStorage.bad1.asl, line 3, characters 0 to 29:
  config c : integer{1..5} = x;
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: expected a pure expression/subprogram.
  [1]
  $ aslref --no-exec TypingRule.DeclareGlobalStorage.bad2.asl
  File TypingRule.DeclareGlobalStorage.bad2.asl, line 3, characters 0 to 29:
  config c : integer{1..x} = 2;
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: expected a pure expression/subprogram.
  [1]
  $ aslref --no-exec TypingRule.DeclareGlobalStorage.bad3.asl
  File TypingRule.DeclareGlobalStorage.bad3.asl, line 2, characters 37 to 38:
  config uninitialized_config : integer;
                                       ^
  ASL Error: Cannot parse.
  [1]
  $ aslref --no-exec TypingRule.DeclareGlobalStorage.non_config.asl
  $ aslref --no-exec TypingRule.UpdateGlobalStorage.constant.asl
  $ aslref --no-exec TypingRule.UpdateGlobalStorage.config.asl
  $ aslref --no-exec TypingRule.UpdateGlobalStorage.config.bad.asl
  File TypingRule.UpdateGlobalStorage.config.bad.asl, line 3,
    characters 0 to 39:
  config d: MyException = MyException{-};
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: expected singular type, found MyException.
  [1]
  $ aslref --no-exec TypingRule.DefDecl.asl
  $ aslref --no-exec TypingRule.UseDecl.asl
  $ aslref --no-exec TypingRule.DefEnumLabels.asl
  $ aslref --no-exec TypingRule.UseTy.asl
  $ aslref --no-exec TypingRule.UseExpr.asl
  $ aslref --no-exec TypingRule.UseLexpr.asl
  $ aslref --no-exec TypingRule.UsePattern.asl
  $ aslref --no-exec TypingRule.UseSlice.asl
  $ aslref --no-exec TypingRule.UseBitfield.asl
  $ aslref --no-exec TypingRule.UseConstraint.asl
  $ aslref --no-exec TypingRule.UseStmt.asl
  $ aslref --no-exec TypingRule.DeclDependencies.asl
  $ aslref --no-exec TypingRule.CheckGlobalPragma.asl
  File TypingRule.CheckGlobalPragma.asl, line 2, characters 0 to 32:
  pragma good_pragma 1, (2==3), x;
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Warning: pragma good_pragma will be ignored.
  $ aslref --no-exec TypingRule.CheckGlobalPragma.bad.asl
  File TypingRule.CheckGlobalPragma.bad.asl, line 2, characters 0 to 33:
  pragma bad_pragma 1, (2==3.0), x;
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Warning: pragma bad_pragma will be ignored.
  File TypingRule.CheckGlobalPragma.bad.asl, line 2, characters 22 to 28:
  pragma bad_pragma 1, (2==3.0), x;
                        ^^^^^^
  ASL Type error: Illegal application of operator == on types integer {2}
    and real.
  [1]
  $ aslref --no-exec TypingRule.AddSubprogramDecls.asl
  $ aslref --no-exec TypingRule.TypeCheckAST.asl
  File TypingRule.TypeCheckAST.asl, line 8, characters 0 to 15:
  pragma pragma1;
  ^^^^^^^^^^^^^^^
  ASL Warning: pragma pragma1 will be ignored.
  $ aslref --no-exec TypingRule.TypeCheckMutuallyRec.bad.asl
  File TypingRule.TypeCheckMutuallyRec.bad.asl, line 1, characters 0 to 15:
  var g = foo(5);
  ^^^^^^^^^^^^^^^
  ASL Type error: multiple recursive declarations: "foo", "g".
  [1]
  $ aslref --no-exec TypingRule.TypeCheckMutuallyRec.bad2.asl
  File TypingRule.TypeCheckMutuallyRec.bad2.asl, line 2, characters 0 to 19:
  type other of base;
  ^^^^^^^^^^^^^^^^^^^
  ASL Type error: multiple recursive declarations: "other", "base".
  [1]
  $ aslref --no-exec TypingRule.DeclareSubprograms.asl
  $ aslref --no-exec TypingRule.SubprogramForName.asl
  $ aslref --no-exec TypingRule.InsertStdlibParam.asl
  $ aslref TypingRule.SubprogramForName.asl
  $ aslref TypingRule.SubprogramForName.bad.undefined.asl
  File TypingRule.SubprogramForName.bad.undefined.asl, line 3,
    characters 8 to 17:
      - = add_10(5);
          ^^^^^^^^^
  ASL Error: Undefined identifier: 'add_10'
  [1]
  $ aslref TypingRule.SubprogramForName.bad.no_candidates.asl
  File TypingRule.SubprogramForName.bad.no_candidates.asl, line 8,
    characters 8 to 19:
      - = add_10(5.0);
          ^^^^^^^^^^^
  ASL Type error: No subprogram declaration matches the invocation:
    add_10(real).
  [1]
  $ aslref TypingRule.ExpressionList.asl
  $ aslref TypingRule.RenameTyEqs.asl
  $ aslref TypingRule.CheckParamsTypeSat.asl
  $ aslref --no-exec TypingRule.ParametersOfTy.asl
  $ aslref --no-exec TypingRule.ParametersOfExpr.asl
  $ aslref --no-exec TypingRule.ParametersOfExpr.bad.asl
  File TypingRule.ParametersOfExpr.bad.asl, line 4, characters 15 to 27:
      z: integer{(D, E).item0}) => // Illegal expression in argument type
                 ^^^^^^^^^^^^
  ASL Static Error: Unsupported expression (D, E).item0.
  [1]
  $ aslref --no-exec TypingRule.FuncSigTypes.asl
  $ aslref --no-exec TypingRule.SubprogramTypesClash.asl
  $ aslref TypingRule.SubprogramTypesClash.bad1.asl
  File TypingRule.SubprogramTypesClash.bad1.asl, line 4, characters 8 to 22:
          return '1000';
          ^^^^^^^^^^^^^^
  ASL Type error: cannot declare already declared element "X".
  [1]
  $ aslref TypingRule.SubprogramTypesClash.bad2.asl
  File TypingRule.SubprogramTypesClash.bad2.asl, line 1, characters 0 to 40:
  func X() => integer begin return 0; end;
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: cannot declare already declared element "X".
  [1]
  $ aslref TypingRule.SubprogramClash.bad1.asl
  File TypingRule.SubprogramClash.bad1.asl, line 1, character 0 to line 4,
    character 4:
  pure func X(a: integer) => integer
  begin
    return 0;
  end;
  ASL Type error: cannot declare already declared element "X".
  [1]
  $ aslref --no-exec TypingRule.CheckParamDecls.asl
  $ aslref TypingRule.CheckParamDecls.bad.asl
  File TypingRule.CheckParamDecls.bad.asl, line 3, character 0 to line 9,
    character 4:
  func parameter_lists{A, B, C, D}(
      x: integer{A..B},
      y: bits(C)) =>
      bits(D)
  begin
      return Ones{D};
  end;
  ASL Type error: incorrect parameter declaration for "parameter_lists",
    expected {D, A, B, C} but {A, B, C, D} provided
  [1]
  $ aslref TypingRule.AnnotateReturnType.bad.asl
  File TypingRule.AnnotateReturnType.bad.asl, line 3, characters 24 to 34:
  func returns_value() => collection { foo: bits(32)};
                          ^^^^^^^^^^
  ASL Error: Cannot parse.
  [1]
  $ aslref --no-exec TypingRule.AnnotateOneParam.asl
  $ aslref TypingRule.AnnotateOneParam.bad1.asl
  File TypingRule.AnnotateOneParam.bad1.asl, line 4, characters 0 to 50:
  func parameterized{A}(x: bits(A)) begin pass; end;
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: cannot declare already declared element "A".
  [1]
  $ aslref --no-exec TypingRule.AnnotateOneArg.asl
  $ aslref TypingRule.AnnotateOneArg.bad1.asl
  File TypingRule.AnnotateOneArg.bad1.asl, line 4, character 0 to line 9,
    character 4:
  func arguments(b: boolean, i: integer, r: real)
  begin
      - = b;
      - = i;
      - = r;
  end;
  ASL Type error: cannot declare already declared element "b".
  [1]
  $ aslref TypingRule.AnnotateOneArg.bad2.asl
  File TypingRule.AnnotateOneArg.bad2.asl, line 2, characters 18 to 28:
  func arguments(b: collection {a: bits(7)})
                    ^^^^^^^^^^
  ASL Error: Cannot parse.
  [1]
  $ aslref TypingRule.AnnotateRetTy.asl
  $ aslref TypingRule.AnnotateRetTy.bad.asl
  File TypingRule.AnnotateRetTy.bad.asl, line 15, characters 4 to 17:
      flip{64}(bv); // Illegal: the returned value must be consumed.
      ^^^^^^^^^^^^^
  ASL Error: Mismatched use of return value from call to 'flip'.
  [1]
  $ aslref --no-exec TypingRule.AnnotateCall.asl
  $ aslref --no-exec TypingRule.AnnotateCall2.asl
  $ aslref TypingRule.AnnotateCall.bad.asl
  File TypingRule.AnnotateCall.bad.asl, line 6, characters 4 to 21:
      f{wid}(bus, bus); // Illegal
      ^^^^^^^^^^^^^^^^^
  ASL Type error: a subtype of integer {2, 4} was expected,
    provided integer {2, 4, 8}.
  [1]
  $ aslref TypingRule.AnnotateCall.bad2.asl
  File TypingRule.AnnotateCall.bad2.asl, line 13, characters 8 to 33:
      - = parameterized_func{arg}();
          ^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: constrained integer expected, provided integer.
  [1]
  $ aslref TypingRule.AnnotateCall.bad3.asl
  File TypingRule.AnnotateCall.bad3.asl, line 9, characters 11 to 32:
      return constrained_func{N}();
             ^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: a subtype of integer {1, 2, 3} was expected,
    provided integer {2, 3, 4}.
  [1]
  $ aslref TypingRule.AnnotateCall.bad4.asl
  File TypingRule.AnnotateCall.bad4.asl, line 9, characters 11 to 32:
      return constrained_func{N}(); // requires an asserting type conversion
             ^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: a subtype of integer {1, 2, 3} was expected,
    provided integer {N}.
  [1]
  $ aslref TypingRule.AnnotateCallActualsTyped.bad1.asl
  File TypingRule.AnnotateCallActualsTyped.bad1.asl, line 11,
    characters 8 to 32:
      - = xor_extend{64}(bv1, bv2); // Illegal: missing parameter for `M`.
          ^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Static Error: Arity error while calling 'xor_extend':
    2 parameters expected and 1 provided
  [1]
  $ aslref TypingRule.AnnotateCallActualsTyped.bad2.asl
  File TypingRule.AnnotateCallActualsTyped.bad2.asl, line 13,
    characters 8 to 31:
      - = xor_extend{64, 32}(bv1);
          ^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: No subprogram declaration matches the invocation:
    xor_extend(bits(64)).
  [1]
  $ aslref TypingRule.AnnotateCallActualsTyped.bad3.asl
  File TypingRule.AnnotateCallActualsTyped.bad3.asl, line 14,
    characters 8 to 24:
      - = plus{64}(bv1, w);
          ^^^^^^^^^^^^^^^^
  ASL Type error: a subtype of integer {0..64} was expected,
    provided integer {0..128}.
  [1]
  $ aslref TypingRule.AnnotateCallActualsTyped.bad4.asl
  File TypingRule.AnnotateCallActualsTyped.bad4.asl, line 23,
    characters 14 to 27:
      var arg = ones{myWid}();
                ^^^^^^^^^^^^^
  ASL Type error: constrained integer expected, provided integer.
  [1]
  $ aslref TypingRule.SubstExpr.asl
  $ aslref --no-exec TypingRule.CheckSymbolicallyEvaluable.asl
  $ aslref --no-exec TypingRule.CheckSymbolicallyEvaluable.bad.asl
  File TypingRule.CheckSymbolicallyEvaluable.bad.asl, line 10,
    characters 5 to 28:
      [symbolic_throwing{4}(4)] data
       ^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: expected a readonly expression/subprogram.
  [1]
  $ aslref TypingRule.EvalSliceExpr.asl
  $ aslref --no-exec TypingRule.SideEffectsLDK.asl
  $ aslref --no-exec TypingRule.SideEffectsGDK.asl
  $ aslref --no-exec TypingRule.SideEffectIsPure.asl
  $ aslref TypingRule.CheckSymbolicallyEvaluable.asl
  $ aslref TypingRule.SESIsReadonly.asl
  $ aslref TypingRule.SESIsReadonly.bad1.asl
  File TypingRule.SESIsReadonly.bad1.asl, line 17, characters 11 to 37:
      assert y > write_side_effecting();
             ^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: expected a readonly expression/subprogram.
  [1]
  $ aslref TypingRule.SESIsReadonly.bad2.asl
  File TypingRule.SESIsReadonly.bad2.asl, line 16, characters 17 to 39:
      for i = x to write_side_effecting() do
                   ^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: expected a readonly expression/subprogram.
  [1]
  $ aslref TypingRule.SESIsPure.asl
  $ aslref TypingRule.SESIsPure.bad.asl
  File TypingRule.SESIsPure.bad.asl, line 4, characters 18 to 23:
  type Data of bits(g * 2) {
                    ^^^^^
  ASL Type error: expected a pure expression/subprogram.
  [1]
  $ aslref --no-exec TypingRule.SESForSubprogram.asl
  $ aslref --no-exec TypingRule.SESIsSymbolicallyEvaluable.asl
  $ aslref TypingRule.SliceEqual.asl
  $ aslref TypingRule.SlicesEqual.asl
  $ aslref TypingRule.BitwidthEqual.asl
  $ aslref TypingRule.RenameSubprograms.asl
  File TypingRule.RenameSubprograms.asl, line 3, characters 2 to 11:
    return 1;
    ^^^^^^^^^
  ASL Type error: a subtype of boolean was expected, provided integer {1}.
  [1]
  $ aslref TypingRule.ApproxConstraint.asl
  $ aslref TypingRule.BitFieldEqual.asl
  $ aslref TypingRule.BitFieldEqual.bad1.asl
  File TypingRule.BitFieldEqual.bad1.asl, line 4, characters 4 to 71:
      var x : bits(64) { [1] data } = Zeros{64} as bits(64) { [2] data };
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: a subtype of bits (64) { [1+:1] data } was expected,
    provided bits (64) { [2+:1] data }.
  [1]
  $ aslref TypingRule.BitFieldEqual.bad2.asl
  File TypingRule.BitFieldEqual.bad2.asl, line 5, character 4 to line 6,
    character 43:
      var x : bits(64) { [16+:16] data { [0] lsb } } =  Zeros{64} as
              bits(64) { [31:16] data {  } };
  ASL Type error: a subtype of bits (64) { [16+:16] data { [0+:1] lsb } }
    was expected, provided bits (64) { [16+:16] data {  } }.
  [1]
  $ aslref --no-exec TypingRule.BitFieldsEqual.asl
  $ aslref TypingRule.BitFieldsEqual.bad.asl
  File TypingRule.BitFieldsEqual.bad.asl, line 6, characters 0 to 76:
  implementation func Foo(bv : bits(64) { [1] msb, [0] lsb }) begin pass; end;
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: no `impdef` for `implementation` function.
  [1]
  $ aslref --no-exec TypingRule.TypeEqual.asl
  $ aslref TypingRule.ExprEqual.asl
  $ aslref TypingRule.ArrayLengthEqual.bad.asl
  File TypingRule.ArrayLengthEqual.bad.asl, line 9, characters 4 to 5:
      x = y;
      ^
  ASL Type error: a subtype of array [[3]] of integer was expected,
    provided array [[Color]] of integer.
  [1]
  $ aslref TypingRule.ReduceConstraint.asl
  File TypingRule.ReduceConstraint.asl, line 6, characters 4 to 65:
      var x : integer{3 * w, 0..5 * z - z - 2 * z,  w + z} = w + z;
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: a subtype of integer {0..(2 * z), (3 * w), (z + w)}
    was expected, provided integer {0..2000}.
  [1]
  $ aslref TypingRule.ConstraintEqual.asl
  $ aslref TypingRule.ConstraintsEqual.asl
  $ aslref --no-exec TypingRule.ToIR.asl
  $ aslref --no-exec TypingRule.Normalize.asl
  $ aslref --no-exec TypingRule.UnitaryMonomialsToExpr.asl
  $ aslref --no-exec TypingRule.ConstraintPow.asl
  $ aslref --no-exec TypingRule.ConstraintMod.asl
  $ aslref --no-exec TypingRule.PossibleExtremitiesLeft.asl
  $ aslref --no-exec TypingRule.PossibleExtremitiesRight.asl
  $ aslref --no-exec TypingRule.ApplyBinopExtremities.asl
  $ aslref --no-exec TypingRule.ApproxBottomTop.asl
  File TypingRule.ApproxBottomTop.asl, line 5, characters 4 to 30:
      var x : integer{a..b} = a;
      ^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: a subtype of integer {a..b} was expected,
    provided integer {1..10}.
  [1]
  $ aslref TypingRule.SymdomOfWidthExpr.asl
  $ aslref TypingRule.AddImmutableExpr.asl
  $ aslref TypingRule.ShouldRememberImmutableExpression.bad.asl
  File TypingRule.ShouldRememberImmutableExpression.bad.asl, line 9,
    characters 4 to 33:
      let t: integer{x..x + 1} = 2;
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: a subtype of integer {x..(x + 1)} was expected,
    provided integer {2}.
  [1]
  $ aslref --no-exec TypingRule.AddGlobalImmutableExpr.asl
  $ aslref --no-exec TypingRule.AddGlobalConstant.asl
  $ aslref TypingRule.LookupConstant.asl
  $ aslref --no-exec TypingRule.AddLocal.asl
  $ aslref --no-exec TypingRule.CheckVarNotInGEnv.asl
  $ aslref --no-exec TypingRule.CheckVarNotInGEnv.bad.asl
  File TypingRule.CheckVarNotInGEnv.bad.asl, line 1, characters 0 to 17:
  var RED: integer;
  ^^^^^^^^^^^^^^^^^
  ASL Type error: cannot declare already declared element "RED".
  [1]
  $ aslref --no-exec TypingRule.CheckVarNotInEnv.bad1.asl
  File TypingRule.CheckVarNotInEnv.bad1.asl, line 1, characters 0 to 53:
  func foo{A}(bv: bits(A), A: integer) begin pass; end;
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: cannot declare already declared element "A".
  [1]
  $ aslref --no-exec TypingRule.CheckVarNotInEnv.bad2.asl
  File TypingRule.CheckVarNotInEnv.bad2.asl, line 3, characters 14 to 15:
      var x, y, y: integer;
                ^
  ASL Type error: cannot declare already declared element "y".
  [1]
  $ aslref TypingRule.CheckBitsEqualWidth.asl
  $ aslref TypingRule.CheckBitsEqualWidth.bad.asl
  File TypingRule.CheckBitsEqualWidth.bad.asl, line 9, characters 15 to 21:
          return x == y; // Illegal: M and N are not necessarily equal.
                 ^^^^^^
  ASL Type error: Illegal application of operator == on types bits(M)
    and bits(N).
  [1]
  $ aslref TypingRule.CheckBitsEqualWidth.bad2.asl
  File TypingRule.CheckBitsEqualWidth.bad2.asl, line 10, characters 11 to 23:
      cond = bit1 == bit2; // Illegal
             ^^^^^^^^^^^^
  ASL Type error: Illegal application of operator == on types bits(int1)
    and bits(int2).
  [1]
  $ aslref --no-exec TypingRule.GetWellConstrainedStructure.asl
  $ aslref --no-exec TypingRule.MemBfs.asl
  $ aslref --no-exec TypingRule.MemBfs.bad.asl
  File TypingRule.MemBfs.bad.asl, line 9, characters 4 to 44:
      var x : bits(8) {[0] flag, [1] lsb} = y;
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: a subtype of bits (8) { [0+:1] flag, [1+:1] lsb }
    was expected, provided FlaggedPacket.
  [1]
  $ aslref --no-exec TypingRule.IntervalTooLarge.asl
  $ aslref --no-exec TypingRule.IntervalTooLarge.bad.asl
  File TypingRule.IntervalTooLarge.bad.asl, line 9, characters 23 to 28:
      var z: integer{} = b * 2;
                         ^^^^^
  Interval too large: [ 0 .. 16385 ]. Keeping it as an interval.
  File TypingRule.IntervalTooLarge.bad.asl, line 9, characters 4 to 29:
      var z: integer{} = b * 2;
      ^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: type used to define storage item is the result of precision
    loss.
  [1]
  $ aslref --no-exec TypingRule.ExplodeIntervals.bad.asl
  File TypingRule.ExplodeIntervals.bad.asl, line 10, characters 23 to 28:
      var z: integer{} = b * 2;
                         ^^^^^
  Interval too large: [ 0 .. 16385 ]. Keeping it as an interval.
  File TypingRule.ExplodeIntervals.bad.asl, line 10, characters 4 to 29:
      var z: integer{} = b * 2;
      ^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: type used to define storage item is the result of precision
    loss.
  [1]
  $ aslref --no-exec TypingRule.FilterReduceConstraintDiv.asl
  $ aslref TypingRule.ReduceToZOpt.asl
  $ aslref --no-exec TypingRule.RefineConstraintBySign.asl
  File TypingRule.RefineConstraintBySign.asl, line 14, characters 23 to 30:
      var z: integer{} = A DIV y;
                         ^^^^^^^
  Warning: Removing some values that would fail with op DIV from constraint set
  {-4..-3, -1..2, -1..B, 0, 1, 3..4, A..B, B, B..-1} gave
  {1..2, 1..B, 1, 3..4, A..B, B, B..-1}. Continuing with this constraint set.
  File TypingRule.RefineConstraintBySign.asl, line 28, characters 8 to 15:
      } = A DIV y;
          ^^^^^^^
  Warning: Removing some values that would fail with op DIV from constraint set
  {-4..-3, -1..2, -1..B, 0, 1, 3..4, A..B, B, B..-1} gave
  {1..2, 1..B, 1, 3..4, A..B, B, B..-1}. Continuing with this constraint set.
  $ aslref --no-exec TypingRule.EBinop.asl
  $ aslref --no-exec TypingRule.EBinop2.asl
  $ aslref TypingRule.EBinop.bad.asl
  File TypingRule.EBinop.bad.asl, line 3, characters 10 to 19:
    let x = 3 DIV 0.0;
            ^^^^^^^^^
  ASL Type error: Illegal application of operator DIV on types integer {3}
    and real.
  [1]
  $ aslref --no-exec TypingRule.ECond.asl
  $ aslref TypingRule.ESlice.bad1.asl
  File TypingRule.ESlice.bad1.asl, line 16, characters 4 to 7:
      dst = src[w:1];
      ^^^
  ASL Type error: a subtype of bits((k - 1)) was expected,
    provided bits(offset).
  [1]
  $ aslref TypingRule.ESlice.bad2.asl
  File TypingRule.ESlice.bad2.asl, line 9, characters 4 to 7:
      dst = src[w:1];
      ^^^
  ASL Type error: a subtype of bits((k - 1)) was expected,
    provided bits(offset).
  [1]
  $ aslref --no-exec TypingRule.Structure.asl
  $ aslref --no-exec TypingRule.AnonymousType.asl
  $ aslref TypingRule.AnnotateSlices.bad-impure.asl
  File TypingRule.AnnotateSlices.bad-impure.asl, line 12, characters 12 to 28:
    let y = x[side_effecting()];
              ^^^^^^^^^^^^^^^^
  ASL Type error: expected a readonly expression/subprogram.
  [1]
  $ aslref --no-exec TypingRule.AddNewFunc.bad1.asl
  File TypingRule.AddNewFunc.bad1.asl, line 8, character 0 to line 11,
    character 4:
  func f(x: shape)
  begin
      pass;
  end;
  ASL Type error: cannot declare already declared element "f".
  [1]
  $ aslref --no-exec TypingRule.AddNewFunc.bad2.asl
  File TypingRule.AddNewFunc.bad2.asl, line 22, characters 4 to 20:
      g(myShape, 0.1); // illegal
      ^^^^^^^^^^^^^^^^
  ASL Type error: a subtype of square was expected, provided shape.
  [1]
  $ aslref --no-exec TypingRule.AddNewFunc.bad3.asl
  File TypingRule.AddNewFunc.bad3.asl, line 8, character 0 to line 11,
    character 4:
  func h(x: shape, y: square)
  begin
      pass;
  end;
  ASL Type error: cannot declare already declared element "h".
  [1]
