  $ aslref no-return.asl
  File no-return.asl, line 2, character 5:
  ASL Typing error: function "main" does not return anything.
  [1]

  $ aslref with-return.asl

  $ aslref always-throw.asl

  $ aslref inherited-always-throw.asl
  File inherited-always-throw.asl, line 10, characters 2 to 27:
  ASL Typing error: function "inherited_always_throws" does not return
    anything.
  [1]

  $ aslref if-return.asl
  File if-return.asl, line 3, characters 2 to 30:
  ASL Typing error: function "sign" does not return anything.
  [1]

  $ aslref if-return-return.asl

  $ aslref if-return-throw.asl

  $ aslref if-return-if.asl
  File if-return-if.asl, line 3, character 2 to line 5, character 5:
  ASL Typing error: function "sign" does not return anything.
  [1]

