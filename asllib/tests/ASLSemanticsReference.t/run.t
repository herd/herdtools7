Hello world should work:

  $ aslref hello_world.asl
  Hello, world!

ASL Semantics Reference:
  $ aslref SemanticsRule.ELocalVar.asl
  $ aslref SemanticsRule.EGlobalVar.asl
//  $ aslref SemanticsRule.EGlobalVarError.asl
  $ aslref SemanticsRule.EUndefIdent.asl
  File SemanticsRule.EUndefIdent.asl, line 4, characters 8 to 9:
  ASL Error: Undefined identifier: 'y'
  [1]
//  $ aslref SemanticsRule.EBinopPlusPrint.asl
  $ aslref SemanticsRule.EBinopPlusAssert.asl
  $ aslref SemanticsRule.EBinopDIVBackendDefinedError.asl
  File SemanticsRule.EBinopDIVBackendDefinedError.asl, line 3,
    characters 10 to 17:
  ASL Typing error: Illegal application of operator DIV on types integer {3}
    and integer {0}
  [1]
  $ aslref --no-type-check SemanticsRule.EBinopDIVBackendDefinedError.asl
  ASL Execution error: Illegal application of operator DIV for values 3 and 0.
  [1]
  $ aslref SemanticsRule.EUnopAssert.asl
  $ aslref SemanticsRule.ECondFALSE.asl
  $ aslref SemanticsRule.ECondUNKNOWN3.asl
  $ aslref SemanticsRule.ECondUNKNOWN42.asl
  File SemanticsRule.ECondUNKNOWN42.asl, line 9, characters 9 to 14:
  ASL Execution error: Assertion failed: (x == 42)
  [1]
  $ aslref SemanticsRule.ESlice.asl
  $ aslref SemanticsRule.ECall.asl
  $ aslref SemanticsRule.EGetArray.asl
  $ aslref SemanticsRule.EGetArrayTooSmall.asl
  File SemanticsRule.EGetArrayTooSmall.asl, line 7, characters 2 to 10:
  ASL Typing error: a subtype of integer {3} was expected,
    provided integer {0..(3 - 1)}.
  [1]
  $ aslref SemanticsRule.ERecord.asl
  $ aslref SemanticsRule.EConcat.asl
  $ aslref SemanticsRule.ETuple.asl
  $ aslref SemanticsRule.EUnknownInteger0.asl
  $ aslref SemanticsRule.EUnknownInteger3.asl
  File SemanticsRule.EUnknownInteger3.asl, line 4, characters 9 to 13:
  ASL Execution error: Assertion failed: (x == 3)
  [1]
  $ aslref SemanticsRule.EUnknownIntegerRange{3-42}3.asl
  $ aslref SemanticsRule.EUnknownIntegerRange{3-42}42.asl
  File SemanticsRule.EUnknownIntegerRange{3-42}42.asl, line 4,
    characters 9 to 14:
  ASL Execution error: Assertion failed: (x == 42)
  [1]
  $ aslref SemanticsRule.EPatternFALSE.asl
  $ aslref SemanticsRule.EPatternTRUE.asl

