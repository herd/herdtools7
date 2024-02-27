Hello world should work:

  $ aslref hello_world.asl
  Hello, world!

ASL Typing Reference:
  $ aslref TypingRule.TypeSatisfaction1.asl
  $ aslref TypingRule.TypeSatisfaction2.asl
  $ aslref TypingRule.TypeSatisfaction3.asl
  File TypingRule.TypeSatisfaction3.asl, line 14, characters 2 to 6:
  ASL Typing error: a subtype of pairT was expected,
    provided (integer {1}, T2).
  [1]
  $ aslref TypingRule.Block0.asl
  3
  Some text
//  $ aslref TypingRule.EConcatUnresolvableToInteger.asl
  $ aslref TypingRule.CheckBinOp.asl
  $ aslref TypingRule.LDDiscard.asl
  $ aslref TypingRule.LDVar.asl
  $ aslref TypingRule.LDTyped.asl
  $ aslref TypingRule.LDTuple.asl
  $ aslref TypingRule.Lit.asl
