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
  {-100..-500, -99..-495, -98..-490, -97..-485, -96..-480, -95..-475,
   -94..-470, -93..-465, -92..-460, -91..-455, -90..-450, -89..-445, -88..-440,
   -87..-435, -86..-430, -85..-425, -84..-420, -83..-415, -82..-410, -81..-405,
   -80..-400, -79..-395, -78..-390, -77..-385, -76..-380, -75..-375, -74..-370,
   -73..-365, -72..-360, -71..-355, -70..-350, -69..-345, -68..-340, -67..-335,
   -66..-330, -65..-325, -64..-320, -63..-315, -62..-310, -61..-305, -60..-300,
   -59..-295, -58..-290, -57..-285, -56..-280, -55..-275, -54..-270, -53..-265,
   -52..-260, -51..-255, -50..-250, -49..-245, -48..-240, -47..-235, -46..-230,
   -45..-225, -44..-220, -43..-215, -42..-210, -41..-205, -40..-200, -39..-195,
   -38..-190, -37..-185, -36..-180, -35..-175, -34..-170, -33..-165, -32..-160,
   -31..-155, -30..-150, -29..-145, -28..-140, -27..-135, -26..-130, -25..-125,
   -24..-120, -23..-115, -22..-110, -21..-105, -20..-100, -19..-95, -18..-90,
   -17..-85, -16..-80, -15..-75, -14..-70, -13..-65, -12..-60, -11..-55,
   -10..-50, -9..-45, -8..-40, -7..-35, -6..-30, -5..-25, -4..-20, -3..-15,
   -2..-10, -1..-5, 0..0, 1..5, 2..10, 3..15, 4..20, 5..25, 6..30, 7..35,
   8..40, 9..45, 10..50, 11..55, 12..60, 13..65, 14..70, 15..75, 16..80,
   17..85, 18..90, 19..95, 20..100, 21..105, 22..110, 23..115, 24..120,
   25..125, 26..130, 27..135, 28..140, 29..145, 30..150, 31..155, 32..160,
   33..165, 34..170, 35..175, 36..180, 37..185, 38..190, 39..195, 40..200,
   41..205, 42..210, 43..215, 44..220, 45..225, 46..230, 47..235, 48..240,
   49..245, 50..250, 51..255, 52..260, 53..265, 54..270, 55..275, 56..280,
   57..285, 58..290, 59..295, 60..300, 61..305, 62..310, 63..315, 64..320,
   65..325, 66..330, 67..335, 68..340, 69..345, 70..350, 71..355, 72..360,
   73..365, 74..370, 75..375, 76..380, 77..385, 78..390, 79..395, 80..400,
   81..405, 82..410, 83..415, 84..420, 85..425, 86..430, 87..435, 88..440,
   89..445, 90..450, 91..455, 92..460, 93..465, 94..470, 95..475, 96..480,
   97..485, 98..490, 99..495, 100..500, (-396 DIV 5)..-396, (-392 DIV 5)..-392,
   (-388 DIV 5)..-388, (-384 DIV 5)..-384, (-376 DIV 5)..-376,
   (-372 DIV 5)..-372, (-368 DIV 5)..-368, (-364 DIV 5)..-364,
   (-356 DIV 5)..-356, (-352 DIV 5)..-352, (-348 DIV 5)..-348,
   (-344 DIV 5)..-344, (-336 DIV 5)..-336, (-332 DIV 5)..-332,
   (-328 DIV 5)..-328, (-324 DIV 5)..-324, (-316 DIV 5)..-316,
   (-312 DIV 5)..-312, (-308 DIV 5)..-308, (-304 DIV 5)..-304,
   (-297 DIV 5)..-297, (-296 DIV 5)..-296, (-294 DIV 5)..-294,
   (-292 DIV 5)..-292, (-291 DIV 5)..-291, (-288 DIV 5)..-288,
   (-284 DIV 5)..-284, (-282 DIV 5)..-282, (-279 DIV 5)..-279,
   (-276 DIV 5)..-276, (-273 DIV 5)..-273, (-272 DIV 5)..-272,
   (-268 DIV 5)..-268, (-267 DIV 5)..-267, (-264 DIV 5)..-264,
   (-261 DIV 5)..-261, (-258 DIV 5)..-258, (-256 DIV 5)..-256,
   (-252 DIV 5)..-252, (-249 DIV 5)..-249, (-248 DIV 5)..-248,
   (-246 DIV 5)..-246, (-244 DIV 5)..-244, (-243 DIV 5)..-243,
   (-237 DIV 5)..-237, (-236 DIV 5)..-236, (-234 DIV 5)..-234,
   (-232 DIV 5)..-232, (-231 DIV 5)..-231, (-228 DIV 5)..-228,
   (-224 DIV 5)..-224, (-222 DIV 5)..-222, (-219 DIV 5)..-219,
   (-216 DIV 5)..-216, (-213 DIV 5)..-213, (-212 DIV 5)..-212,
   (-208 DIV 5)..-208, (-207 DIV 5)..-207, (-204 DIV 5)..-204,
   (-201 DIV 5)..-201, (-198 DIV 5)..-198, (-196 DIV 5)..-196,
   (-194 DIV 5)..-194, (-192 DIV 5)..-192, (-189 DIV 5)..-189,
   (-188 DIV 5)..-188, (-186 DIV 5)..-186, (-184 DIV 5)..-184,
   (-183 DIV 5)..-183, (-182 DIV 5)..-182, (-178 DIV 5)..-178,
   (-177 DIV 5)..-177, (-176 DIV 5)..-176, (-174 DIV 5)..-174,
   (-172 DIV 5)..-172, (-171 DIV 5)..-171, (-168 DIV 5)..-168,
   (-166 DIV 5)..-166, (-164 DIV 5)..-164, (-162 DIV 5)..-162,
   (-159 DIV 5)..-159, (-158 DIV 5)..-158, (-156 DIV 5)..-156,
   (-154 DIV 5)..-154, (-153 DIV 5)..-153, (-152 DIV 5)..-152,
   (-148 DIV 5)..-148, (-147 DIV 5)..-147, (-146 DIV 5)..-146,
   (-144 DIV 5)..-144, (-142 DIV 5)..-142, (-141 DIV 5)..-141,
   (-138 DIV 5)..-138, (-136 DIV 5)..-136, (-134 DIV 5)..-134,
   (-132 DIV 5)..-132, (-129 DIV 5)..-129, (-128 DIV 5)..-128,
   (-126 DIV 5)..-126, (-124 DIV 5)..-124, (-123 DIV 5)..-123,
   (-122 DIV 5)..-122, (-118 DIV 5)..-118, (-117 DIV 5)..-117,
   (-116 DIV 5)..-116, (-114 DIV 5)..-114, (-112 DIV 5)..-112,
   (-111 DIV 5)..-111, (-108 DIV 5)..-108, (-106 DIV 5)..-106,
   (-104 DIV 5)..-104, (-102 DIV 5)..-102, (-99 DIV 5)..-99, (-98 DIV 5)..-98,
   (-97 DIV 5)..-97, (-96 DIV 5)..-96, (-94 DIV 5)..-94, (-93 DIV 5)..-93,
   (-92 DIV 5)..-92, (-91 DIV 5)..-91, (-89 DIV 5)..-89, (-88 DIV 5)..-88,
   (-87 DIV 5)..-87, (-86 DIV 5)..-86, (-84 DIV 5)..-84, (-83 DIV 5)..-83,
   (-82 DIV 5)..-82, (-81 DIV 5)..-81, (-79 DIV 5)..-79, (-78 DIV 5)..-78,
   (-77 DIV 5)..-77, (-76 DIV 5)..-76, (-74 DIV 5)..-74, (-73 DIV 5)..-73,
   (-72 DIV 5)..-72, (-71 DIV 5)..-71, (-69 DIV 5)..-69, (-68 DIV 5)..-68,
   (-67 DIV 5)..-67, (-66 DIV 5)..-66, (-64 DIV 5)..-64, (-63 DIV 5)..-63,
   (-62 DIV 5)..-62, (-61 DIV 5)..-61, (-59 DIV 5)..-59, (-58 DIV 5)..-58,
   (-57 DIV 5)..-57, (-56 DIV 5)..-56, (-54 DIV 5)..-54, (-53 DIV 5)..-53,
   (-52 DIV 5)..-52, (-51 DIV 5)..-51, (-49 DIV 5)..-49, (-48 DIV 5)..-48,
   (-47 DIV 5)..-47, (-46 DIV 5)..-46, (-44 DIV 5)..-44, (-43 DIV 5)..-43,
   (-42 DIV 5)..-42, (-41 DIV 5)..-41, (-39 DIV 5)..-39, (-38 DIV 5)..-38,
   (-37 DIV 5)..-37, (-36 DIV 5)..-36, (-34 DIV 5)..-34, (-33 DIV 5)..-33,
   (-32 DIV 5)..-32, (-31 DIV 5)..-31, (-29 DIV 5)..-29, (-28 DIV 5)..-28,
   (-27 DIV 5)..-27, (-26 DIV 5)..-26, (-24 DIV 5)..-24, (-23 DIV 5)..-23,
   (-22 DIV 5)..-22, (-21 DIV 5)..-21, (-19 DIV 5)..-19, (-18 DIV 5)..-18,
   (-17 DIV 5)..-17, (-16 DIV 5)..-16, (-14 DIV 5)..-14, (-13 DIV 5)..-13,
   (-12 DIV 5)..-12, (-11 DIV 5)..-11, (-9 DIV 5)..-9, (-8 DIV 5)..-8,
   (-7 DIV 5)..-7, (-6 DIV 5)..-6, (-4 DIV 5)..-4, (-3 DIV 5)..-3,
   (-2 DIV 5)..-2, (-1 DIV 5)..-1, (1 DIV 5)..1, (2 DIV 5)..2, (3 DIV 5)..3,
   (4 DIV 5)..4, (6 DIV 5)..6, (7 DIV 5)..7, (8 DIV 5)..8, (9 DIV 5)..9,
   (11 DIV 5)..11, (12 DIV 5)..12, (13 DIV 5)..13, (14 DIV 5)..14,
   (16 DIV 5)..16, (17 DIV 5)..17, (18 DIV 5)..18, (19 DIV 5)..19,
   (21 DIV 5)..21, (22 DIV 5)..22, (23 DIV 5)..23, (24 DIV 5)..24,
   (26 DIV 5)..26, (27 DIV 5)..27, (28 DIV 5)..28, (29 DIV 5)..29,
   (31 DIV 5)..31, (32 DIV 5)..32, (33 DIV 5)..33, (34 DIV 5)..34,
   (36 DIV 5)..36, (37 DIV 5)..37, (38 DIV 5)..38, (39 DIV 5)..39,
   (41 DIV 5)..41, (42 DIV 5)..42, (43 DIV 5)..43, (44 DIV 5)..44,
   (46 DIV 5)..46, (47 DIV 5)..47, (48 DIV 5)..48, (49 DIV 5)..49,
   (51 DIV 5)..51, (52 DIV 5)..52, (53 DIV 5)..53, (54 DIV 5)..54,
   (56 DIV 5)..56, (57 DIV 5)..57, (58 DIV 5)..58, (59 DIV 5)..59,
   (61 DIV 5)..61, (62 DIV 5)..62, (63 DIV 5)..63, (64 DIV 5)..64,
   (66 DIV 5)..66, (67 DIV 5)..67, (68 DIV 5)..68, (69 DIV 5)..69,
   (71 DIV 5)..71, (72 DIV 5)..72, (73 DIV 5)..73, (74 DIV 5)..74,
   (76 DIV 5)..76, (77 DIV 5)..77, (78 DIV 5)..78, (79 DIV 5)..79,
   (81 DIV 5)..81, (82 DIV 5)..82, (83 DIV 5)..83, (84 DIV 5)..84,
   (86 DIV 5)..86, (87 DIV 5)..87, (88 DIV 5)..88, (89 DIV 5)..89,
   (91 DIV 5)..91, (92 DIV 5)..92, (93 DIV 5)..93, (94 DIV 5)..94,
   (96 DIV 5)..96, (97 DIV 5)..97, (98 DIV 5)..98, (99 DIV 5)..99,
   (102 DIV 5)..102, (104 DIV 5)..104, (106 DIV 5)..106, (108 DIV 5)..108,
   (111 DIV 5)..111, (112 DIV 5)..112, (114 DIV 5)..114, (116 DIV 5)..116,
   (117 DIV 5)..117, (118 DIV 5)..118, (122 DIV 5)..122, (123 DIV 5)..123,
   (124 DIV 5)..124, (126 DIV 5)..126, (128 DIV 5)..128, (129 DIV 5)..129,
   (132 DIV 5)..132, (134 DIV 5)..134, (136 DIV 5)..136, (138 DIV 5)..138,
   (141 DIV 5)..141, (142 DIV 5)..142, (144 DIV 5)..144, (146 DIV 5)..146,
   (147 DIV 5)..147, (148 DIV 5)..148, (152 DIV 5)..152, (153 DIV 5)..153,
   (154 DIV 5)..154, (156 DIV 5)..156, (158 DIV 5)..158, (159 DIV 5)..159,
   (162 DIV 5)..162, (164 DIV 5)..164, (166 DIV 5)..166, (168 DIV 5)..168,
   (171 DIV 5)..171, (172 DIV 5)..172, (174 DIV 5)..174, (176 DIV 5)..176,
   (177 DIV 5)..177, (178 DIV 5)..178, (182 DIV 5)..182, (183 DIV 5)..183,
   (184 DIV 5)..184, (186 DIV 5)..186, (188 DIV 5)..188, (189 DIV 5)..189,
   (192 DIV 5)..192, (194 DIV 5)..194, (196 DIV 5)..196, (198 DIV 5)..198,
   (201 DIV 5)..201, (204 DIV 5)..204, (207 DIV 5)..207, (208 DIV 5)..208,
   (212 DIV 5)..212, (213 DIV 5)..213, (216 DIV 5)..216, (219 DIV 5)..219,
   (222 DIV 5)..222, (224 DIV 5)..224, (228 DIV 5)..228, (231 DIV 5)..231,
   (232 DIV 5)..232, (234 DIV 5)..234, (236 DIV 5)..236, (237 DIV 5)..237,
   (243 DIV 5)..243, (244 DIV 5)..244, (246 DIV 5)..246, (248 DIV 5)..248,
   (249 DIV 5)..249, (252 DIV 5)..252, (256 DIV 5)..256, (258 DIV 5)..258,
   (261 DIV 5)..261, (264 DIV 5)..264, (267 DIV 5)..267, (268 DIV 5)..268,
   (272 DIV 5)..272, (273 DIV 5)..273, (276 DIV 5)..276, (279 DIV 5)..279,
   (282 DIV 5)..282, (284 DIV 5)..284, (288 DIV 5)..288, (291 DIV 5)..291,
   (292 DIV 5)..292, (294 DIV 5)..294, (296 DIV 5)..296, (297 DIV 5)..297,
   (304 DIV 5)..304, (308 DIV 5)..308, (312 DIV 5)..312, (316 DIV 5)..316,
   (324 DIV 5)..324, (328 DIV 5)..328, (332 DIV 5)..332, (336 DIV 5)..336,
   (344 DIV 5)..344, (348 DIV 5)..348, (352 DIV 5)..352, (356 DIV 5)..356,
   (364 DIV 5)..364, (368 DIV 5)..368, (372 DIV 5)..372, (376 DIV 5)..376,
   (384 DIV 5)..384, (388 DIV 5)..388, (392 DIV 5)..392, (396 DIV 5)..396}
  gave
  {-100..-500, -99..-495, -98..-490, -97..-485, -96..-480, -95..-475,
   -94..-470, -93..-465, -92..-460, -91..-455, -90..-450, -89..-445, -88..-440,
   -87..-435, -86..-430, -85..-425, -84..-420, -83..-415, -82..-410, -81..-405,
   -80..-400, -79..-395, -78..-390, -77..-385, -76..-380, -75..-375, -74..-370,
   -73..-365, -72..-360, -71..-355, -70..-350, -69..-345, -68..-340, -67..-335,
   -66..-330, -65..-325, -64..-320, -63..-315, -62..-310, -61..-305, -60..-300,
   -59..-295, -58..-290, -57..-285, -56..-280, -55..-275, -54..-270, -53..-265,
   -52..-260, -51..-255, -50..-250, -49..-245, -48..-240, -47..-235, -46..-230,
   -45..-225, -44..-220, -43..-215, -42..-210, -41..-205, -40..-200, -39..-195,
   -38..-190, -37..-185, -36..-180, -35..-175, -34..-170, -33..-165, -32..-160,
   -31..-155, -30..-150, -29..-145, -28..-140, -27..-135, -26..-130, -25..-125,
   -24..-120, -23..-115, -22..-110, -21..-105, -20..-100, -19..-95, -18..-90,
   -17..-85, -16..-80, -15..-75, -14..-70, -13..-65, -12..-60, -11..-55,
   -10..-50, -9..-45, -8..-40, -7..-35, -6..-30, -5..-25, -4..-20, -3..-15,
   -2..-10, -1..-5, 0..0, 1..5, 2..10, 3..15, 4..20, 5..25, 6..30, 7..35,
   8..40, 9..45, 10..50, 11..55, 12..60, 13..65, 14..70, 15..75, 16..80,
   17..85, 18..90, 19..95, 20..100, 21..105, 22..110, 23..115, 24..120,
   25..125, 26..130, 27..135, 28..140, 29..145, 30..150, 31..155, 32..160,
   33..165, 34..170, 35..175, 36..180, 37..185, 38..190, 39..195, 40..200,
   41..205, 42..210, 43..215, 44..220, 45..225, 46..230, 47..235, 48..240,
   49..245, 50..250, 51..255, 52..260, 53..265, 54..270, 55..275, 56..280,
   57..285, 58..290, 59..295, 60..300, 61..305, 62..310, 63..315, 64..320,
   65..325, 66..330, 67..335, 68..340, 69..345, 70..350, 71..355, 72..360,
   73..365, 74..370, 75..375, 76..380, 77..385, 78..390, 79..395, 80..400,
   81..405, 82..410, 83..415, 84..420, 85..425, 86..430, 87..435, 88..440,
   89..445, 90..450, 91..455, 92..460, 93..465, 94..470, 95..475, 96..480,
   97..485, 98..490, 99..495, 100..500, -79..-396, -78..-392, -77..-388,
   -76..-384, -75..-376, -74..-372, -73..-368, -72..-364, -71..-356, -70..-352,
   -69..-348, -68..-344, -67..-336, -66..-332, -65..-328, -64..-324, -63..-316,
   -62..-312, -61..-308, -60..-304, -59..-297, -59..-296, -58..-294, -58..-292,
   -58..-291, -57..-288, -56..-284, -56..-282, -55..-279, -55..-276, -54..-273,
   -54..-272, -53..-268, -53..-267, -52..-264, -52..-261, -51..-258, -51..-256,
   -50..-252, -49..-249, -49..-248, -49..-246, -48..-244, -48..-243, -47..-237,
   -47..-236, -46..-234, -46..-232, -46..-231, -45..-228, -44..-224, -44..-222,
   -43..-219, -43..-216, -42..-213, -42..-212, -41..-208, -41..-207, -40..-204,
   -40..-201, -39..-198, -39..-196, -38..-194, -38..-192, -37..-189, -37..-188,
   -37..-186, -36..-184, -36..-183, -36..-182, -35..-178, -35..-177, -35..-176,
   -34..-174, -34..-172, -34..-171, -33..-168, -33..-166, -32..-164, -32..-162,
   -31..-159, -31..-158, -31..-156, -30..-154, -30..-153, -30..-152, -29..-148,
   -29..-147, -29..-146, -28..-144, -28..-142, -28..-141, -27..-138, -27..-136,
   -26..-134, -26..-132, -25..-129, -25..-128, -25..-126, -24..-124, -24..-123,
   -24..-122, -23..-118, -23..-117, -23..-116, -22..-114, -22..-112, -22..-111,
   -21..-108, -21..-106, -20..-104, -20..-102, -19..-99, -19..-98, -19..-97,
   -19..-96, -18..-94, -18..-93, -18..-92, -18..-91, -17..-89, -17..-88,
   -17..-87, -17..-86, -16..-84, -16..-83, -16..-82, -16..-81, -15..-79,
   -15..-78, -15..-77, -15..-76, -14..-74, -14..-73, -14..-72, -14..-71,
   -13..-69, -13..-68, -13..-67, -13..-66, -12..-64, -12..-63, -12..-62,
   -12..-61, -11..-59, -11..-58, -11..-57, -11..-56, -10..-54, -10..-53,
   -10..-52, -10..-51, -9..-49, -9..-48, -9..-47, -9..-46, -8..-44, -8..-43,
   -8..-42, -8..-41, -7..-39, -7..-38, -7..-37, -7..-36, -6..-34, -6..-33,
   -6..-32, -6..-31, -5..-29, -5..-28, -5..-27, -5..-26, -4..-24, -4..-23,
   -4..-22, -4..-21, -3..-19, -3..-18, -3..-17, -3..-16, -2..-14, -2..-13,
   -2..-12, -2..-11, -1..-9, -1..-8, -1..-7, -1..-6, 0..-4, 0..-3, 0..-2,
   0..-1, 1..1, 1..2, 1..3, 1..4, 2..6, 2..7, 2..8, 2..9, 3..11, 3..12, 
   3..13, 3..14, 4..16, 4..17, 4..18, 4..19, 5..21, 5..22, 5..23, 5..24, 
   6..26, 6..27, 6..28, 6..29, 7..31, 7..32, 7..33, 7..34, 8..36, 8..37, 
   8..38, 8..39, 9..41, 9..42, 9..43, 9..44, 10..46, 10..47, 10..48, 10..49,
   11..51, 11..52, 11..53, 11..54, 12..56, 12..57, 12..58, 12..59, 13..61,
   13..62, 13..63, 13..64, 14..66, 14..67, 14..68, 14..69, 15..71, 15..72,
   15..73, 15..74, 16..76, 16..77, 16..78, 16..79, 17..81, 17..82, 17..83,
   17..84, 18..86, 18..87, 18..88, 18..89, 19..91, 19..92, 19..93, 19..94,
   20..96, 20..97, 20..98, 20..99, 21..102, 21..104, 22..106, 22..108, 
   23..111, 23..112, 23..114, 24..116, 24..117, 24..118, 25..122, 25..123,
   25..124, 26..126, 26..128, 26..129, 27..132, 27..134, 28..136, 28..138,
   29..141, 29..142, 29..144, 30..146, 30..147, 30..148, 31..152, 31..153,
   31..154, 32..156, 32..158, 32..159, 33..162, 33..164, 34..166, 34..168,
   35..171, 35..172, 35..174, 36..176, 36..177, 36..178, 37..182, 37..183,
   37..184, 38..186, 38..188, 38..189, 39..192, 39..194, 40..196, 40..198,
   41..201, 41..204, 42..207, 42..208, 43..212, 43..213, 44..216, 44..219,
   45..222, 45..224, 46..228, 47..231, 47..232, 47..234, 48..236, 48..237,
   49..243, 49..244, 50..246, 50..248, 50..249, 51..252, 52..256, 52..258,
   53..261, 53..264, 54..267, 54..268, 55..272, 55..273, 56..276, 56..279,
   57..282, 57..284, 58..288, 59..291, 59..292, 59..294, 60..296, 60..297,
   61..304, 62..308, 63..312, 64..316, 65..324, 66..328, 67..332, 68..336,
   69..344, 70..348, 71..352, 72..356, 73..364, 74..368, 75..372, 76..376,
   77..384, 78..388, 79..392, 80..396}.
  Continuing with this constraint set.
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
