Hello world should work:

  $ aslseq hello_world.asl
  Hello, world!

Type-checking errors:

  $ aslseq subtype-satisfaction-arrray-illegal.asl
  File subtype-satisfaction-arrray-illegal.asl, line 4, characters 0 to 36:
  ASL Typing error: a subtype of m was expected, provided array [10] of n.
  [1]

  $ aslseq anonymous-types-example.asl
  File anonymous-types-example.asl, line 21, characters 2 to 6:
  ASL Typing error: a subtype of pairT was expected,
    provided (integer {1}, T2).
  [1]

  $ aslseq duplicate_function_args.asl
  File duplicate_function_args.asl, line 1, character 0 to line 4, character 3:
  ASL Typing error: cannot declare already declared element "i".
  [1]

  $ aslseq duplicate_record_fields.asl
  File duplicate_record_fields.asl, line 1, character 0 to line 5, character 2:
  ASL Typing error: cannot declare already declared element "i".
  [1]

  $ aslseq duplicate_enumeration_items.asl
  File duplicate_enumeration_items.asl, line 1, characters 0 to 34:
  ASL Typing error: cannot declare already declared element "i".
  [1]

Bad types:
  $ aslseq overlapping-slices.asl
  File overlapping-slices.asl, line 1, character 0 to line 4, character 2:
  ASL Typing error: overlapping slices 10:0, 3+:2.
  [1]

Global ignored:
  $ cat >global_ignored.asl <<EOF
  > var - = 3 / 0;
  > func main () => integer
  > begin return 0; end
  > EOF

  $ aslseq global_ignored.asl
  File global_ignored.asl, line 1, characters 8 to 13:
  ASL Typing error: Illegal application of operator / on types integer {3}
    and integer {0}
  [1]

Constrained-type satisfaction:
  $ cat >type-sat.asl <<EOF
  > func illegal_f1()
  > begin
  >   var x: integer { 8, 16 };
  >   var y: integer { 8, 16, 32};
  >   x = y; // illegal as domain of x is not a subset of domain of y
  > end
  > EOF

  $ aslseq type-sat.asl
  File type-sat.asl, line 5, characters 2 to 3:
  ASL Typing error: a subtype of integer {8, 16} was expected,
    provided integer {8, 16, 32}.
  [1]

  $ cat >type-sat.asl <<EOF
  > func illegal_f2()
  > begin
  >   var x: integer { 8 , 16 };
  >   var y: integer;
  >   x = y; // illegal
  > end
  > EOF

  $ aslseq type-sat.asl
  File type-sat.asl, line 5, characters 2 to 3:
  ASL Typing error: a subtype of integer {8, 16} was expected,
    provided integer.
  [1]

  $ aslseq type_satisfaction_illegal_f3.asl
  File type_satisfaction_illegal_f3.asl, line 9, characters 4 to 17:
  ASL Typing error: a subtype of integer {8, 16} was expected,
    provided integer.
  [1]

  $ aslseq type_satisfaction_illegal_f4.asl
  File type_satisfaction_illegal_f4.asl, line 9, characters 4 to 17:
  ASL Typing error: a subtype of integer {8, 16} was expected,
    provided integer {8..64}.
  [1]

  $ cat >type-sat.asl <<EOF
  > func illegal_f5(N: integer, b: bits(N))
  > begin
  >   // N is under-constrained integer
  >   var x: integer { 2, 4} = N;
  >    return;
  > end

  $ aslseq type-sat.asl
  File type-sat.asl, line 4, characters 2 to 29:
  ASL Typing error: a subtype of integer {2, 4} was expected,
    provided integer {}.
  [1]

  $ cat >type-sat.asl <<EOF
  > func invokeMe_2(N: integer, b: bits(N))
  > begin
  >   // N is under-constrained integer
  >   var x: integer { 2, 4} = N;
  >   return;
  > end

  $ aslseq type-sat.asl
  File type-sat.asl, line 4, characters 2 to 29:
  ASL Typing error: a subtype of integer {2, 4} was expected,
    provided integer {}.
  [1]

Runtime checks:
  $ cat >runtime-type-sat.asl <<EOF
  > func main () => integer
  > begin
  >   let x: integer {1} = 2 as integer {1};
  >   return 0;
  > end

  $ aslseq runtime-type-sat.asl
  File runtime-type-sat.asl, line 3, characters 23 to 24:
  ASL Execution error: Mismatch type:
    value 2 does not belong to type integer {1}.
  [1]

