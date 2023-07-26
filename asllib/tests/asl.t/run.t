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
  File duplicate_function_args.asl, line 1, character 0 to line  4, character 3:
  ASL Typing error: cannot declare already declared element "i".
  [1]

  $ aslseq duplicate_record_fields.asl
  File duplicate_record_fields.asl, line 1, character 0 to line  5, character 2:
  ASL Typing error: cannot declare already declared element "i".
  [1]

  $ aslseq duplicate_enumeration_items.asl
  File duplicate_enumeration_items.asl, line 1, characters 0 to 34:
  ASL Typing error: cannot declare already declared element "i".
  [1]
