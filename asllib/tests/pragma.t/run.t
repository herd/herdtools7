Simple pragma examples
  $ aslref pragma.asl
  $ aslref pragma_invalid.asl
  File pragma_invalid.asl, line 1, characters 10 to 18:
  ASL Typing error: Illegal application of operator + on types integer {2}
    and boolean.
  [1]

  $ aslref pragma_invalid_stmt.asl
  File pragma_invalid_stmt.asl, line 3, characters 21 to 26:
  ASL Typing error: Illegal application of operator + on types string
    and integer {1}.
  [1]
