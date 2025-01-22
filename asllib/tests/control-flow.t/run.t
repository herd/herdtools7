  $ aslref no-return.asl
  File no-return.asl, line 2, character 5:
  ASL Typing error: the function "main" may not terminate by returning a value
    or raising an exception..
  [1]

  $ aslref with-return.asl

  $ aslref always-throw.asl

  $ aslref inherited-always-throw.asl
  File inherited-always-throw.asl, line 10, characters 2 to 27:
  ASL Typing error: the function "inherited_always_throws" may not terminate by
    returning a value or raising an exception..
  [1]

  $ aslref if-return.asl
  File if-return.asl, line 3, characters 2 to 31:
  ASL Typing error: the function "sign" may not terminate by returning a value
    or raising an exception..
  [1]

  $ aslref if-return-return.asl

  $ aslref if-return-throw.asl

  $ aslref if-return-if.asl
  File if-return-if.asl, line 3, character 2 to line 5, character 6:
  ASL Typing error: the function "sign" may not terminate by returning a value
    or raising an exception..
  [1]

  $ aslref try-00.asl

  $ aslref try-01.asl
  File try-01.asl, line 5, character 2 to line 6, character 41:
  ASL Typing error: the function "test0" may not terminate by returning a value
    or raising an exception..
  [1]

  $ aslref try-02.asl

  $ aslref try-03.asl

  $ aslref try-04.asl
  File try-04.asl, line 5, character 2 to line 9, character 6:
  ASL Typing error: the function "test0" may not terminate by returning a value
    or raising an exception..
  [1]

  $ aslref try-05.asl
  File try-05.asl, line 6, character 2 to line 11, character 6:
  ASL Typing error: the function "test0" may not terminate by returning a value
    or raising an exception..
  [1]

  $ aslref try-06.asl
  File try-06.asl, line 5, character 2 to line 9, character 6:
  ASL Typing error: the function "test0" may not terminate by returning a value
    or raising an exception..
  [1]
