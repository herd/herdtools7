Simple pragma examples
  $ aslref pragma.asl
  File pragma.asl, line 3, characters 4 to 25:
  ASL Warning: pragma p1 will be ignored.
  File pragma.asl, line 4, characters 4 to 14:
  ASL Warning: pragma p2 will be ignored.
  File pragma.asl, line 5, characters 4 to 32:
  ASL Warning: pragma p3 will be ignored.
  File pragma.asl, line 11, characters 0 to 30:
  ASL Warning: pragma p3 will be ignored.
  File pragma.asl, line 10, characters 0 to 15:
  ASL Warning: pragma p2 will be ignored.
  File pragma.asl, line 9, characters 0 to 10:
  ASL Warning: pragma p1 will be ignored.

  $ aslref pragma_invalid.asl
  File pragma_invalid.asl, line 1, characters 0 to 19:
  ASL Warning: pragma p1 will be ignored.
  File pragma_invalid.asl, line 1, characters 10 to 18:
  ASL Typing error: Illegal application of operator + on types integer {2}
    and boolean.
  [1]

  $ aslref pragma_invalid_stmt.asl
  File pragma_invalid_stmt.asl, line 3, characters 4 to 27:
  ASL Warning: pragma fail will be ignored.
  File pragma_invalid_stmt.asl, line 3, characters 21 to 26:
  ASL Typing error: Illegal application of operator + on types string
    and integer {1}.
  [1]
