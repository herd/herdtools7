  $ aslref on-arbitrary.asl
  File on-arbitrary.asl, line 8, characters 12 to 35:
  ASL typing error: unexpected collection.
  [1]
  $ aslref on-local-func-arg.asl
  File on-local-func-arg.asl, line 6, character 0 to line 11, character 4:
  ASL typing error: unexpected collection.
  [1]
  $ aslref on-local-var.asl
  File on-local-var.asl, line 8, characters 2 to 24:
  ASL typing error: unexpected collection.
  [1]
  $ aslref with-non-bitvector-arg.asl
  File with-non-bitvector-arg.asl, line 1, character 0 to line 4, character 2:
  ASL Static Error: Unsupported type collection {
                                       field1: bits(1),
                                       field2: integer
                                     }.
  [1]
  $ aslref on-function-return-type.asl
  File on-function-return-type.asl, line 8, character 0 to line 11, character 4:
  ASL typing error: unexpected collection.
  [1]

