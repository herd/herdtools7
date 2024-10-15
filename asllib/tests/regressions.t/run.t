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
  ASL Static error: overlapping slices 0+:11, 3+:2.
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
  File stdlib.asl, line 106, characters 13 to 26:
  Warning: Removing some values that would fail with op DIV from constraint set
  {(-500 DIV 5)..(-500 DIV 1), (-495 DIV 5)..(-495 DIV 1),
   (-490 DIV 5)..(-490 DIV 1), (-485 DIV 5)..(-485 DIV 1),
   (-480 DIV 5)..(-480 DIV 1), (-475 DIV 5)..(-475 DIV 1),
   (-470 DIV 5)..(-470 DIV 1), (-465 DIV 5)..(-465 DIV 1),
   (-460 DIV 5)..(-460 DIV 1), (-455 DIV 5)..(-455 DIV 1),
   (-450 DIV 5)..(-450 DIV 1), (-445 DIV 5)..(-445 DIV 1),
   (-440 DIV 5)..(-440 DIV 1), (-435 DIV 5)..(-435 DIV 1),
   (-430 DIV 5)..(-430 DIV 1), (-425 DIV 5)..(-425 DIV 1),
   (-420 DIV 5)..(-420 DIV 1), (-415 DIV 5)..(-415 DIV 1),
   (-410 DIV 5)..(-410 DIV 1), (-405 DIV 5)..(-405 DIV 1),
   (-400 DIV 5)..(-400 DIV 1), (-392 DIV 5)..(-392 DIV 1),
   (-390 DIV 5)..(-390 DIV 1), (-388 DIV 5)..(-388 DIV 1),
   (-380 DIV 5)..(-380 DIV 1), (-372 DIV 5)..(-372 DIV 1),
   (-370 DIV 5)..(-370 DIV 1), (-368 DIV 5)..(-368 DIV 1),
   (-360 DIV 5)..(-360 DIV 1), (-352 DIV 5)..(-352 DIV 1),
   (-350 DIV 5)..(-350 DIV 1), (-348 DIV 5)..(-348 DIV 1),
   (-340 DIV 5)..(-340 DIV 1), (-332 DIV 5)..(-332 DIV 1),
   (-330 DIV 5)..(-330 DIV 1), (-328 DIV 5)..(-328 DIV 1),
   (-320 DIV 5)..(-320 DIV 1), (-312 DIV 5)..(-312 DIV 1),
   (-310 DIV 5)..(-310 DIV 1), (-308 DIV 5)..(-308 DIV 1),
   (-300 DIV 5)..(-300 DIV 1), (-288 DIV 5)..(-288 DIV 1),
   (-282 DIV 5)..(-282 DIV 1), (-270 DIV 5)..(-270 DIV 1),
   (-258 DIV 5)..(-258 DIV 1), (-252 DIV 5)..(-252 DIV 1),
   (-240 DIV 5)..(-240 DIV 1), (-228 DIV 5)..(-228 DIV 1),
   (-222 DIV 5)..(-222 DIV 1), (-210 DIV 5)..(-210 DIV 1),
   (-198 DIV 5)..(-198 DIV 1), (-192 DIV 5)..(-192 DIV 1),
   (-180 DIV 5)..(-180 DIV 1), (-168 DIV 5)..(-168 DIV 1),
   (-162 DIV 5)..(-162 DIV 1), (-150 DIV 5)..(-150 DIV 1),
   (-138 DIV 5)..(-138 DIV 1), (-132 DIV 5)..(-132 DIV 1),
   (-120 DIV 5)..(-120 DIV 1), (-108 DIV 5)..(-108 DIV 1),
   (-102 DIV 5)..(-102 DIV 1), (102 DIV 5)..(102 DIV 1),
   (108 DIV 5)..(108 DIV 1), (120 DIV 5)..(120 DIV 1),
   (132 DIV 5)..(132 DIV 1), (138 DIV 5)..(138 DIV 1),
   (150 DIV 5)..(150 DIV 1), (162 DIV 5)..(162 DIV 1),
   (168 DIV 5)..(168 DIV 1), (180 DIV 5)..(180 DIV 1),
   (192 DIV 5)..(192 DIV 1), (198 DIV 5)..(198 DIV 1),
   (210 DIV 5)..(210 DIV 1), (222 DIV 5)..(222 DIV 1),
   (228 DIV 5)..(228 DIV 1), (240 DIV 5)..(240 DIV 1),
   (252 DIV 5)..(252 DIV 1), (258 DIV 5)..(258 DIV 1),
   (270 DIV 5)..(270 DIV 1), (282 DIV 5)..(282 DIV 1),
   (288 DIV 5)..(288 DIV 1), (300 DIV 5)..(300 DIV 1),
   (308 DIV 5)..(308 DIV 1), (310 DIV 5)..(310 DIV 1),
   (312 DIV 5)..(312 DIV 1), (320 DIV 5)..(320 DIV 1),
   (328 DIV 5)..(328 DIV 1), (330 DIV 5)..(330 DIV 1),
   (332 DIV 5)..(332 DIV 1), (340 DIV 5)..(340 DIV 1),
   (348 DIV 5)..(348 DIV 1), (350 DIV 5)..(350 DIV 1),
   (352 DIV 5)..(352 DIV 1), (360 DIV 5)..(360 DIV 1),
   (368 DIV 5)..(368 DIV 1), (370 DIV 5)..(370 DIV 1),
   (372 DIV 5)..(372 DIV 1), (380 DIV 5)..(380 DIV 1),
   (388 DIV 5)..(388 DIV 1), (390 DIV 5)..(390 DIV 1),
   (392 DIV 5)..(392 DIV 1), (400 DIV 5)..(400 DIV 1),
   (405 DIV 5)..(405 DIV 1), (410 DIV 5)..(410 DIV 1),
   (415 DIV 5)..(415 DIV 1), (420 DIV 5)..(420 DIV 1),
   (425 DIV 5)..(425 DIV 1), (430 DIV 5)..(430 DIV 1),
   (435 DIV 5)..(435 DIV 1), (440 DIV 5)..(440 DIV 1),
   (445 DIV 5)..(445 DIV 1), (450 DIV 5)..(450 DIV 1),
   (455 DIV 5)..(455 DIV 1), (460 DIV 5)..(460 DIV 1),
   (465 DIV 5)..(465 DIV 1), (470 DIV 5)..(470 DIV 1),
   (475 DIV 5)..(475 DIV 1), (480 DIV 5)..(480 DIV 1),
   (485 DIV 5)..(485 DIV 1), (490 DIV 5)..(490 DIV 1),
   (495 DIV 5)..(495 DIV 1), (500 DIV 5)..(500 DIV 1),
   (-396 DIV 5)..(-395 DIV 1), (-385 DIV 5)..(-384 DIV 1),
   (-376 DIV 5)..(-375 DIV 1), (-365 DIV 5)..(-364 DIV 1),
   (-356 DIV 5)..(-355 DIV 1), (-345 DIV 5)..(-344 DIV 1),
   (-336 DIV 5)..(-335 DIV 1), (-325 DIV 5)..(-324 DIV 1),
   (-316 DIV 5)..(-315 DIV 1), (-305 DIV 5)..(-304 DIV 1),
   (-297 DIV 5)..(-294 DIV 1), (-292 DIV 5)..(-290 DIV 1),
   (-285 DIV 5)..(-284 DIV 1), (-280 DIV 5)..(-279 DIV 1),
   (-276 DIV 5)..(-275 DIV 1), (-273 DIV 5)..(-272 DIV 1),
   (-268 DIV 5)..(-267 DIV 1), (-265 DIV 5)..(-264 DIV 1),
   (-261 DIV 5)..(-260 DIV 1), (-256 DIV 5)..(-255 DIV 1),
   (-250 DIV 5)..(-248 DIV 1), (-246 DIV 5)..(-243 DIV 1),
   (-237 DIV 5)..(-234 DIV 1), (-232 DIV 5)..(-230 DIV 1),
   (-225 DIV 5)..(-224 DIV 1), (-220 DIV 5)..(-219 DIV 1),
   (-216 DIV 5)..(-215 DIV 1), (-213 DIV 5)..(-212 DIV 1),
   (-208 DIV 5)..(-207 DIV 1), (-205 DIV 5)..(-204 DIV 1),
   (-201 DIV 5)..(-200 DIV 1), (-196 DIV 5)..(-194 DIV 1),
   (-190 DIV 5)..(-188 DIV 1), (-186 DIV 5)..(-182 DIV 1),
   (-178 DIV 5)..(-174 DIV 1), (-172 DIV 5)..(-170 DIV 1),
   (-166 DIV 5)..(-164 DIV 1), (-160 DIV 5)..(-158 DIV 1),
   (-156 DIV 5)..(-152 DIV 1), (-148 DIV 5)..(-144 DIV 1),
   (-142 DIV 5)..(-140 DIV 1), (-136 DIV 5)..(-134 DIV 1),
   (-130 DIV 5)..(-128 DIV 1), (-126 DIV 5)..(-122 DIV 1),
   (-118 DIV 5)..(-114 DIV 1), (-112 DIV 5)..(-110 DIV 1),
   (-106 DIV 5)..(-104 DIV 1), (-100 DIV 5)..(100 DIV 1),
   (104 DIV 5)..(106 DIV 1), (110 DIV 5)..(112 DIV 1),
   (114 DIV 5)..(118 DIV 1), (122 DIV 5)..(126 DIV 1),
   (128 DIV 5)..(130 DIV 1), (134 DIV 5)..(136 DIV 1),
   (140 DIV 5)..(142 DIV 1), (144 DIV 5)..(148 DIV 1),
   (152 DIV 5)..(156 DIV 1), (158 DIV 5)..(160 DIV 1),
   (164 DIV 5)..(166 DIV 1), (170 DIV 5)..(172 DIV 1),
   (174 DIV 5)..(178 DIV 1), (182 DIV 5)..(186 DIV 1),
   (188 DIV 5)..(190 DIV 1), (194 DIV 5)..(196 DIV 1),
   (200 DIV 5)..(201 DIV 1), (204 DIV 5)..(205 DIV 1),
   (207 DIV 5)..(208 DIV 1), (212 DIV 5)..(213 DIV 1),
   (215 DIV 5)..(216 DIV 1), (219 DIV 5)..(220 DIV 1),
   (224 DIV 5)..(225 DIV 1), (230 DIV 5)..(232 DIV 1),
   (234 DIV 5)..(237 DIV 1), (243 DIV 5)..(246 DIV 1),
   (248 DIV 5)..(250 DIV 1), (255 DIV 5)..(256 DIV 1),
   (260 DIV 5)..(261 DIV 1), (264 DIV 5)..(265 DIV 1),
   (267 DIV 5)..(268 DIV 1), (272 DIV 5)..(273 DIV 1),
   (275 DIV 5)..(276 DIV 1), (279 DIV 5)..(280 DIV 1),
   (284 DIV 5)..(285 DIV 1), (290 DIV 5)..(292 DIV 1),
   (294 DIV 5)..(297 DIV 1), (304 DIV 5)..(305 DIV 1),
   (315 DIV 5)..(316 DIV 1), (324 DIV 5)..(325 DIV 1),
   (335 DIV 5)..(336 DIV 1), (344 DIV 5)..(345 DIV 1),
   (355 DIV 5)..(356 DIV 1), (364 DIV 5)..(365 DIV 1),
   (375 DIV 5)..(376 DIV 1), (384 DIV 5)..(385 DIV 1), (395 DIV 5)..(396 DIV 1)}
  gave
  {21..102, 22..108, 24..120, 27..132, 28..138, 30..150, 33..162, 34..168,
   36..180, 39..192, 40..198, 42..210, 45..222, 46..228, 48..240, 51..252,
   52..258, 54..270, 57..282, 58..288, 60..300, 62..308, 62..310, 63..312,
   64..320, 66..328, 66..330, 67..332, 68..340, 70..348, 70..350, 71..352,
   72..360, 74..368, 74..370, 75..372, 76..380, 78..388, 78..390, 79..392,
   80..400, 81..405, 82..410, 83..415, 84..420, 85..425, 86..430, 87..435,
   88..440, 89..445, 90..450, 91..455, 92..460, 93..465, 94..470, 95..475,
   96..480, 97..485, 98..490, 99..495, 100..500, -20..100, 21..106, 22..112,
   23..118, 25..126, 26..130, 27..136, 28..142, 29..148, 31..156, 32..160,
   33..166, 34..172, 35..178, 37..186, 38..190, 39..196, 40..201, 41..205,
   42..208, 43..213, 43..216, 44..220, 45..225, 46..232, 47..237, 49..246,
   50..250, 51..256, 52..261, 53..265, 54..268, 55..273, 55..276, 56..280,
   57..285, 58..292, 59..297, 61..305, 63..316, 65..325, 67..336, 69..345,
   71..356, 73..365, 75..376, 77..385, 79..396}.
  Continuing with this constraint set.
  File stdlib.asl, line 107, characters 39 to 46:
  Warning: Removing some values that would fail with op DIV from constraint set
  {(-100 DIV 5)..(100 DIV 1)} gave {-20..100}. Continuing with this constraint
  set.
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

  $ aslref bad-pattern.asl
  File bad-pattern.asl, line 4, characters 7 to 12:
  ASL Typing error: Erroneous pattern '101' for expression of type integer {3}.
  [1]
