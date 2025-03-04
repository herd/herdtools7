Hello world should work:

  $ aslref hello_world.asl
  Hello, world!

ASL Typing Tests:
  $ aslref TypingRule.SubtypeSatisfaction1.asl
  $ aslref --no-exec TypingRule.SubtypeSatisfaction2.asl
  $ aslref TypingRule.SubtypeSatisfaction3.asl
  File TypingRule.SubtypeSatisfaction3.asl, line 9, characters 4 to 45:
  ASL Typing error: a subtype of AnimalLegs was expected, provided ShapeSides.
  [1]
  $ aslref TypingRule.SubtypeSatisfaction.bad1.asl
  File TypingRule.SubtypeSatisfaction.bad1.asl, line 8, characters 0 to 31:
  ASL Typing error: a subtype of integer {2} was expected,
    provided integer {1..2}.
  [1]
  $ aslref TypingRule.SubtypeSatisfaction.bad2.asl
  File TypingRule.SubtypeSatisfaction.bad2.asl, line 7, characters 4 to 13:
  ASL Typing error: a subtype of integer {N} was expected,
    provided integer {2, 4}.
  [1]
  $ aslref TypingRule.TypeSatisfaction1.asl
  $ aslref TypingRule.TypeSatisfaction2.asl
  $ aslref TypingRule.TypeSatisfaction3.asl
  File TypingRule.TypeSatisfaction3.asl, line 14, characters 2 to 6:
  ASL Typing error: a subtype of pairT was expected,
    provided (integer {1}, T2).
  [1]
  $ aslref --no-exec TypingRule.TypeClashes.asl
  $ aslref TypingRule.LowestCommonAncestor.asl
  $ aslref TypingRule.FindNamedLCA.asl
  $ aslref TypingRule.ApplyUnopType.asl
//  $ aslref TypingRule.EConcatUnresolvableToInteger.asl
  $ aslref TypingRule.ApplyBinopTypes.asl
  $ aslref TypingRule.ApplyBinopTypes.constraints.asl
  File TypingRule.ApplyBinopTypes.constraints.asl, line 22, characters 51 to 78:
  Warning: Removing some values that would fail with op DIV from constraint set
  {-5..3} gave {1..3}. Continuing with this constraint set.
  File TypingRule.ApplyBinopTypes.constraints.asl, line 26, characters 39 to 65:
  Warning: Removing some values that would fail with op MOD from constraint set
  {0..3} gave {1..3}. Continuing with this constraint set.
  File TypingRule.ApplyBinopTypes.constraints.asl, line 42, characters 31 to 82:
  Warning: Removing some values that would fail with op DIV from constraint set
  {0..16384} gave {1..16384}. Continuing with this constraint set.
  ASL Error: Undefined identifier: 'main'
  [1]
  $ aslref TypingRule.LDDiscard.asl
  $ aslref TypingRule.LDVar.asl
  $ aslref TypingRule.LDTyped.asl
  $ aslref TypingRule.LDTuple.asl
  $ aslref TypingRule.Lit.asl
  $ aslref TypingRule.CheckCommonBitfieldsAlign.Error.asl
  File TypingRule.CheckCommonBitfieldsAlign.Error.asl, line 1, character 20 to
    line 6, character 1:
  ASL Typing error:
    bitfields `sub.common` and `common` are in the same scope but define different slices of the containing bitvector type: [0, 1] and [1:0], respectively.
  [1]

ASL Typing Tests / annotating types:
  $ aslref TypingRule.TReal.asl
  $ aslref TypingRule.TBool.asl
  $ aslref TypingRule.TNamed.asl
  $ aslref TypingRule.TIntUnconstrained.asl
  $ aslref TypingRule.TIntWellConstrained.asl
  $ aslref TypingRule.TIntParameterized.asl
  $ aslref TypingRule.InheritIntegerConstraints.asl
  $ aslref TypingRule.InheritIntegerConstraints.unconstrained.bad.asl
  File TypingRule.InheritIntegerConstraints.unconstrained.bad.asl, line 5,
    characters 4 to 27:
  ASL Typing error: constrained integer expected, provided integer.
  [1]

  $ aslref --no-exec TypingRule.TInt.config_pending_constrained.bad.asl
  File TypingRule.TInt.config_pending_constrained.bad.asl, line 1,
    characters 0 to 27:
  ASL Typing error: a pending constrained integer is illegal here.
  [1]

  $ aslref TypingRule.TInt.rhs_pending_constrained.bad.asl
  File TypingRule.TInt.rhs_pending_constrained.bad.asl, line 5,
    characters 28 to 43:
  ASL Typing error: a pending constrained integer is illegal here.
  [1]

  $ aslref TypingRule.AnnotateConstraint.asl
  $ aslref TypingRule.AnnotateConstraint.bad.asl
  File TypingRule.AnnotateConstraint.bad.asl, line 4, characters 17 to 18:
  ASL Typing error: a pure expression was expected, found x, which produces the
    following side-effects: [ReadsLocal "x"].
  [1]

  $ aslref TypingRule.TBits.asl
  $ aslref TypingRule.TTuple.asl
  $ aslref TypingRule.TArray.asl
  $ aslref TypingRule.TArray.bad.asl
  File TypingRule.TArray.bad.asl, line 9, characters 31 to 57:
  ASL Typing error: a pure expression was expected,
    found non_symbolically_evaluable, which produces the following
    side-effects: [ReadsLocal "non_symbolically_evaluable"].
  [1]
  $ aslref TypingRule.AnnotateSymbolicallyEvaluableExpr.asl
  $ aslref --no-exec TypingRule.TEnumDecl.asl
  $ aslref --no-exec TypingRule.TEnumDecl.bad.asl
  File TypingRule.TEnumDecl.bad.asl, line 2, characters 0 to 49:
  ASL Typing error: cannot declare already declared element "RED".
  [1]
  $ aslref --no-exec TypingRule.Subtype.asl
  $ aslref --no-exec TypingRule.GetVariableEnum.asl
  $ aslref TypingRule.TRecordDecl.asl
  $ aslref TypingRule.TExceptionDecl.asl
  $ aslref TypingRule.TNonDecl.asl
  File TypingRule.TNonDecl.asl, line 1, characters 5 to 6:
  ASL Error: Cannot parse.
  [1]
  $ aslref TypingRule.TBitField.asl
  $ aslref --no-exec TypingRule.AnnotateFuncSig.asl
  $ aslref TypingRule.BuiltinAggregateTypes.asl
  $ aslref --no-exec TypingRule.BuiltinExceptionType.asl
  $ aslref TypingRule.BuiltinSingularTypes.asl
  $ aslref TypingRule.EnumerationType.asl
  $ aslref TypingRule.TString.asl
  $ aslref TypingRule.ConstraintMod.bad.asl
  File TypingRule.ConstraintMod.bad.asl, line 9, characters 4 to 5:
  ASL Typing error: a subtype of integer {0..2} was expected,
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
  $ aslref TypingRule.BaseValue.bad_parameterized.asl
  File TypingRule.BaseValue.bad_parameterized.asl, line 4, characters 4 to 39:
  ASL Typing error: base value of type bits(N) cannot be statically determined
    since it consists of N.
  [1]
  $ aslref TypingRule.BaseValue.bad_negative_width.asl
  File TypingRule.BaseValue.bad_negative_width.asl, line 1, characters 0 to 24:
  ASL Typing error: base value of empty type bits((- 3)).
  [1]
  $ aslref TypingRule.BaseValue.bad_empty.asl
  File TypingRule.BaseValue.bad_empty.asl, line 1, characters 0 to 22:
  ASL Typing error: base value of empty type integer {5..0}.
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
  implies_bool: FALSE --> FALSE = TRUE
  implies_bool: FALSE --> TRUE = TRUE
  implies_bool: TRUE --> TRUE = TRUE
  implies_bool: TRUE --> FALSE = FALSE
  equiv_bool: FALSE <-> FALSE = TRUE
  equiv_bool: TRUE <-> TRUE = TRUE
  equiv_bool: FALSE <-> TRUE = FALSE

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

  $ aslref TypingRule.EVar.asl
  $ aslref TypingRule.EVar.undefined.asl
  File TypingRule.EVar.undefined.asl, line 3, characters 12 to 13:
  ASL Error: Undefined identifier: 't'
  [1]

  $ aslref TypingRule.EGetRecordField.asl
  $ aslref TypingRule.EGetBadRecordField.asl
  File TypingRule.EGetBadRecordField.asl, line 7, characters 10 to 36:
  ASL Error: There is no field 'undeclared_field' on type MyRecordType.
  [1]
  $ aslref TypingRule.EGetBitfield.asl
  $ aslref TypingRule.EGetBadBitField.asl
  File TypingRule.EGetBadBitField.asl, line 7, characters 12 to 33:
  ASL Error: There is no field 'undeclared_bitfield' on type Packet.
  [1]
  $ aslref TypingRule.EGetBadField.asl
  File TypingRule.EGetBadField.asl, line 6, characters 12 to 15:
  ASL Error: There is no field 'f' on type array [[5]] of integer.
  [1]
  $ aslref TypingRule.EGetFields.asl
  $ aslref --no-exec TypingRule.ATC.asl
  $ aslref --no-exec TypingRule.CheckATC.asl
  File TypingRule.CheckATC.asl, line 8, characters 12 to 32:
  ASL Typing error: cannot perform Asserted Type Conversion on real by
    integer {1, 2}.
  [1]
  $ aslref --no-exec TypingRule.IsGlobalUndefined.asl
  $ aslref TypingRule.EArbitrary.asl
  $ aslref TypingRule.StaticEval.asl
  $ aslref TypingRule.StaticEval.bad.asl
  File TypingRule.StaticEval.bad.asl, line 3, characters 5 to 20: Division will
  result in empty constraint set, so will always fail.
  File TypingRule.StaticEval.bad.asl, line 3, characters 5 to 20:
  ASL Typing error: Illegal application of operator DIV on types integer {64}
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
  ASL Error: Undefined identifier: 'x'
  [1]
  $ aslref TypingRule.LESetBadField.asl
  File TypingRule.LESetBadField.asl, line 6, characters 4 to 5:
  ASL Typing error: integer {42} does not subtype any of: bits(-), record {  },
    exception {  }.
  [1]
  $ aslref TypingRule.LESetBadField.asl
  File TypingRule.LESetBadField.asl, line 6, characters 4 to 5:
  ASL Typing error: integer {42} does not subtype any of: bits(-), record {  },
    exception {  }.
  [1]
  $ aslref TypingRule.LESetStructuredField.asl
  $ aslref TypingRule.LESetField.asl
  $ aslref TypingRule.LESetFields.asl
  $ aslref TypingRule.LESlice.asl
  $ aslref TypingRule.LESlice.bad.asl
  File TypingRule.LESlice.bad.asl, line 4, characters 3 to 11:
  ASL Static error: overlapping slices 0+:4, 3+:1.
  [1]
  $ aslref --no-exec TypingRule.DeclareGlobalStorage.asl
  $ aslref TypingRule.SDecl.asl
  $ aslref TypingRule.SAssert.bad.asl
  File TypingRule.SAssert.bad.asl, line 11, characters 10 to 23:
  ASL Typing error: a pure expression was expected, found (increment()), which
    produces the following side-effects: [WritesGlobal "g", ReadsGlobal "g"].
  [1]
  $ aslref TypingRule.SWhile.asl
  File TypingRule.SWhile.asl, line 23, character 4 to line 29, character 8:
  ASL Warning: Loop does not have a limit.
  20
  20
  $ aslref TypingRule.SWhile.bad_limit.asl
  File TypingRule.SWhile.bad_limit.asl, line 8, characters 26 to 33:
  ASL Typing error: a pure expression was expected, found i_limit, which
    produces the following side-effects: [ReadsLocal "i_limit"].
  [1]
  $ aslref TypingRule.SFor.bad1.asl
  File TypingRule.SFor.bad1.asl, line 5, character 4 to line 7, character 8:
  ASL Typing error: cannot declare already declared element "i".
  [1]
  $ aslref TypingRule.SFor.bad2.asl
  File TypingRule.SFor.bad2.asl, line 5, characters 8 to 9:
  ASL Typing error: cannot assign to immutable storage "i".
  [1]
  $ aslref TypingRule.SFor.bad3.asl
  File TypingRule.SFor.bad3.asl, line 7, characters 4 to 5:
  ASL Error: Undefined identifier: 'j'
  [1]
  $ aslref TypingRule.SFor.bad4.asl
  File TypingRule.SFor.bad4.asl, line 11, characters 17 to 30:
  ASL Typing error: a pure expression was expected, found upper_bound(), which
    produces the following side-effects: [WritesGlobal "g"].
  [1]
  $ aslref TypingRule.SReturn.bad.asl
  File TypingRule.SReturn.bad.asl, line 3, characters 4 to 13:
  ASL Typing error: cannot return something from a procedure.
  [1]
  $ aslref --no-exec TypingRule.SPragma.asl
  File TypingRule.SPragma.asl, line 3, characters 4 to 39:
  ASL Warning: pragma implementation_hidden will be ignored.
  $ aslref --no-exec TypingRule.BitfieldSliceToPositions.asl
  $ aslref TypingRule.DisjointSlicesToPositions.bad.asl
  File TypingRule.DisjointSlicesToPositions.bad.asl, line 1, character 0 to
    line 6, character 2:
  ASL Static error: overlapping slices 0+:4, 3+:3.
  [1]
  $ aslref --no-exec TypingRule.CheckPositionsInWidth.bad.asl
  File TypingRule.CheckPositionsInWidth.bad.asl, line 1, character 0 to line 5,
    character 2:
  ASL Static error: Cannot extract from bitvector of length 16 slice (3 * 5)+:5.
  [1]

  $ aslref TypingRule.CheckNoPrecisionLoss.asl
  File TypingRule.CheckNoPrecisionLoss.asl, line 3, characters 8 to 13:
  Exploding sets for the binary operation * could result in a constraint set
  bigger than 2^17 with constraints 1..1024 and 1..1024. Continuing with the
  non-expanded constraints.
  File TypingRule.CheckNoPrecisionLoss.asl, line 3, characters 0 to 14:
  ASL Typing error: type used to define storage item is the result of precision
    loss.
  [1]
  $ aslref TypingRule.PrecisionJoin.asl
  File TypingRule.PrecisionJoin.asl, line 3, characters 9 to 14:
  Exploding sets for the binary operation * could result in a constraint set
  bigger than 2^17 with constraints 1..1024 and 1..1024. Continuing with the
  non-expanded constraints.
  File TypingRule.PrecisionJoin.asl, line 3, characters 0 to 20:
  ASL Typing error: type used to define storage item is the result of precision
    loss.
  [1]

  $ aslref TypingRule.PSingle.asl
  $ aslref TypingRule.PSingle.bad.asl
  File TypingRule.PSingle.bad.asl, line 4, characters 11 to 30:
  ASL Typing error: cannot find a common ancestor to those two types bits(3)
    and bits(4).
  [1]
  $ aslref TypingRule.PRange.asl
  $ aslref TypingRule.PRange.bad.asl
  File TypingRule.PRange.bad.asl, line 4, characters 11 to 32:
  ASL Typing error: Erroneous pattern (- (9.0 / 5.0)) .. 143 for expression of
    type real.
  [1]
  $ aslref TypingRule.PLeq.asl
  $ aslref TypingRule.PLeq.bad.asl
  File TypingRule.PLeq.bad.asl, line 4, characters 12 to 28:
  ASL Typing error: Erroneous pattern <= (42.0 / 1.0) for expression of type
    integer {3}.
  [1]
  $ aslref TypingRule.PGeq.asl
  $ aslref TypingRule.PGeq.bad.asl
  File TypingRule.PGeq.bad.asl, line 4, characters 11 to 27:
  ASL Typing error: Erroneous pattern >= (3.0 / 1.0) for expression of type
    integer {42}.
  [1]
  $ aslref TypingRule.PTuple.bad.asl
  File TypingRule.PTuple.bad.asl, line 8, characters 11 to 45:
  ASL Typing error: a subtype of bits(-) was expected, provided integer {3}.
  [1]
  $ aslref TypingRule.PMask.asl
  $ aslref TypingRule.PMask.bad.asl
  File TypingRule.PMask.bad.asl, line 5, characters 11 to 34:
  ASL Typing error: a subtype of bits(7) was expected, provided bits(6).
  [1]
  $ aslref TypingRule.PAny.asl
  $ aslref TypingRule.PAny.bad.asl
  File TypingRule.PAny.bad.asl, line 5, characters 11 to 29:
  ASL Typing error: Erroneous pattern 5 for expression of type boolean.
  [1]

  $ aslref TypingRule.CheckIsNotCollection.asl
  File TypingRule.CheckIsNotCollection.asl, line 8, characters 2 to 25:
  ASL typing error: unexpected collection.
  [1]
