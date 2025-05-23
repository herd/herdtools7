Simple pragma examples
  $ aslref pragma.asl
  File pragma.asl, line 3, characters 4 to 25:
      pragma p1 "with arg";
      ^^^^^^^^^^^^^^^^^^^^^
  ASL Warning: pragma p1 will be ignored.
  File pragma.asl, line 4, characters 4 to 14:
      pragma p2;
      ^^^^^^^^^^
  ASL Warning: pragma p2 will be ignored.
  File pragma.asl, line 5, characters 4 to 32:
      pragma p3 "multi arg", TRUE;
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Warning: pragma p3 will be ignored.
  File pragma.asl, line 11, characters 0 to 30:
  pragma p3 TRUE, 2 + 2, test();
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Warning: pragma p3 will be ignored.
  File pragma.asl, line 10, characters 0 to 15:
  pragma p2 TRUE;
  ^^^^^^^^^^^^^^^
  ASL Warning: pragma p2 will be ignored.
  File pragma.asl, line 9, characters 0 to 10:
  pragma p1;
  ^^^^^^^^^^
  ASL Warning: pragma p1 will be ignored.

  $ aslref pragma_invalid.asl
  File pragma_invalid.asl, line 1, characters 0 to 19:
  pragma p1 2 + TRUE;
  ^^^^^^^^^^^^^^^^^^^
  ASL Warning: pragma p1 will be ignored.
  File pragma_invalid.asl, line 1, characters 10 to 18:
  pragma p1 2 + TRUE;
            ^^^^^^^^
  ASL Type error: Illegal application of operator + on types integer {2}
    and boolean.
  [1]

  $ aslref pragma_invalid_stmt.asl
  File pragma_invalid_stmt.asl, line 3, characters 4 to 27:
      pragma fail "test" + 1;
      ^^^^^^^^^^^^^^^^^^^^^^^
  ASL Warning: pragma fail will be ignored.
  File pragma_invalid_stmt.asl, line 3, characters 21 to 26:
      pragma fail "test" + 1;
                       ^^^^^
  ASL Type error: Illegal application of operator + on types string
    and integer {1}.
  [1]
