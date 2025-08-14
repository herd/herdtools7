  $ aslref on-arbitrary.asl
  File on-arbitrary.asl, line 3, characters 23 to 33:
    var col = ARBITRARY: collection {
                         ^^^^^^^^^^
  ASL Grammar error: Cannot parse.
  [1]
  $ aslref on-local-func-arg.asl
  File on-local-func-arg.asl, line 6, characters 15 to 25:
  func foo (col: collection {
                 ^^^^^^^^^^
  ASL Grammar error: Cannot parse.
  [1]
  $ aslref on-local-var.asl
  File on-local-var.asl, line 8, characters 2 to 25:
    var col = MyCollection;
    ^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: unexpected collection.
  [1]
  $ aslref with-non-bitvector-arg.asl
  File with-non-bitvector-arg.asl, line 1, character 0 to line 4, character 2:
  var MyCollection : collection {
    field1: bits(1),
    field2: integer,
  };
  ASL Static error:
    Unsupported type collection { field1: bits(1), field2: integer }.
  [1]
  $ aslref on-function-return-type.asl
  File on-function-return-type.asl, line 6, characters 15 to 25:
  func foo () => collection {
                 ^^^^^^^^^^
  ASL Grammar error: Cannot parse.
  [1]

  $ aslref on-local-tuple.asl
  File on-local-tuple.asl, line 8, characters 2 to 33:
    var col2 = (my_collection, 32);
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: unexpected collection.
  [1]

  $ aslref on-global-var.asl
  File on-global-var.asl, line 6, characters 0 to 33:
  var MyCollection2 = MyCollection;
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: unexpected collection.
  [1]

  $ aslref on-type-declaration.asl
  File on-type-declaration.asl, line 1, characters 21 to 31:
  type MyCollection of collection {
                       ^^^^^^^^^^
  ASL Grammar error: Cannot parse.
  [1]
