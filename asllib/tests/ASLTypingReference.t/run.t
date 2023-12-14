Hello world should work:

  $ aslref hello_world.asl
  Hello, world!

ASL Typing Reference:
  $ aslref TypingRule.TypeSatisfaction1.asl
  $ aslref TypingRule.TypeSatisfaction2.asl
  $ aslref TypingRule.TypeSatisfaction3.asl
  File TypingRule.TypeSatisfaction3.asl, line 11, characters 2 to 6:
  ASL Typing error: a subtype of pairT was expected,
    provided (integer {1}, T2).
  [1]
  $ aslref TypingRule.Block0.asl
  File TypingRule.Block0.asl, line 5, characters 9 to 19:
  ASL Error: Arity error while calling 'print':
    0 arguments expected and 1 provided
  [1]
