  $ aslref on-arbitrary.asl
  File on-arbitrary.asl, line 3, characters 23 to 33:
    var col = ARBITRARY: collection {
                         ^^^^^^^^^^
  ASL Grammar error (BE_PE): Cannot parse.
  [1]
  $ aslref non-variable-base.asl
  File non-variable-base.asl, line 5, characters 10 to 41:
    let x = (if TRUE then C1 else C2).field;
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error (TE_UT):
    collection fields can only be accessed through a variable;
    provided base: if TRUE then C1 else C2.
  [1]
  $ aslref on-local-func-arg.asl
  File on-local-func-arg.asl, line 6, characters 15 to 25:
  func foo (col: collection {
                 ^^^^^^^^^^
  ASL Grammar error (BE_PE): Cannot parse.
  [1]
  $ aslref on-local-var.asl
  File on-local-var.asl, line 8, characters 2 to 25:
    var col = MyCollection;
    ^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error (TE_UT): unexpected collection.
  [1]
  $ aslref with-non-bitvector-arg.asl
  File with-non-bitvector-arg.asl, line 3, characters 10 to 17:
    field2: integer,
            ^^^^^^^
  ASL Type error (TE_UT): a bitvector type was expected, provided integer.
  [1]
  $ aslref on-function-return-type.asl
  File on-function-return-type.asl, line 6, characters 15 to 25:
  func foo () => collection {
                 ^^^^^^^^^^
  ASL Grammar error (BE_PE): Cannot parse.
  [1]

  $ aslref on-local-tuple.asl
  File on-local-tuple.asl, line 8, characters 2 to 33:
    var col2 = (my_collection, 32);
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error (TE_UT): unexpected collection.
  [1]

  $ aslref on-global-var.asl
  File on-global-var.asl, line 6, characters 0 to 33:
  var MyCollection2 = MyCollection;
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error (TE_UT): unexpected collection.
  [1]

  $ aslref on-type-declaration.asl
  File on-type-declaration.asl, line 1, characters 21 to 31:
  type MyCollection of collection {
                       ^^^^^^^^^^
  ASL Grammar error (BE_PE): Cannot parse.
  [1]
