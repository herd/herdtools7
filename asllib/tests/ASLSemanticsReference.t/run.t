Hello world should work:

  $ aslref hello_world.asl
  Hello, world!

ASL Semantics Reference:
  $ aslref SemanticsRule.Lit.asl
  $ aslref SemanticsRule.ELocalVar.asl
  $ aslref SemanticsRule.EGlobalVar.asl
  $ aslref SemanticsRule.EGlobalVarError.asl
  $ aslref SemanticsRule.EUndefIdent.asl
  File SemanticsRule.EUndefIdent.asl, line 5, characters 9 to 10:
  ASL Error: Undefined identifier: 'y'
  [1]
//  $ aslref SemanticsRule.EBinopPlusPrint.asl
  $ aslref SemanticsRule.EBinopPlusAssert.asl
  $ aslref SemanticsRule.EBinopDIVBackendDefinedError.asl
  File SemanticsRule.EBinopDIVBackendDefinedError.asl, line 4,
    characters 10 to 17:
  ASL Typing error: Illegal application of operator DIV on types integer {3}
    and integer {0}
  [1]
  $ aslref --no-type-check SemanticsRule.EBinopDIVBackendDefinedError.asl
  ASL Execution error: Illegal application of operator DIV for values 3 and 0.
  [1]
  $ aslref SemanticsRule.EUnopAssert.asl
  $ aslref SemanticsRule.ECondFALSE.asl
  $ aslref SemanticsRule.ECondUNKNOWN3or42.asl
  $ aslref SemanticsRule.ESlice.asl
  $ aslref SemanticsRule.ECall.asl
  $ aslref SemanticsRule.EGetArray.asl
  $ aslref SemanticsRule.EGetArrayTooSmall.asl
  File SemanticsRule.EGetArrayTooSmall.asl, line 7, characters 8 to 19:
  ASL Typing error: a subtype of integer {0..2} was expected,
    provided integer {3}.
  [1]
  $ aslref SemanticsRule.ERecord.asl
  $ aslref SemanticsRule.EConcat.asl
  $ aslref SemanticsRule.ETuple.asl
  $ aslref SemanticsRule.EUnknownInteger0.asl
  $ aslref SemanticsRule.EUnknownInteger3.asl
  File SemanticsRule.EUnknownInteger3.asl, line 5, characters 9 to 13:
  ASL Execution error: Assertion failed: (x == 3)
  [1]
  $ aslref SemanticsRule.EUnknownIntegerRange3-42-3.asl
  $ aslref SemanticsRule.EUnknownIntegerRange3-42-42.asl
  File SemanticsRule.EUnknownIntegerRange3-42-42.asl, line 5,
    characters 9 to 14:
  ASL Execution error: Assertion failed: (x == 42)
  [1]
  $ aslref SemanticsRule.EPatternFALSE.asl
  $ aslref SemanticsRule.EPatternTRUE.asl
  $ aslref SemanticsRule.LELocalVar.asl
  $ aslref SemanticsRule.LESetArray.asl
  $ aslref SemanticsRule.SReturnNone.asl
  $ aslref SemanticsRule.SCond.asl
  $ aslref SemanticsRule.SCase.asl
  $ aslref SemanticsRule.SWhile.asl
  $ aslref SemanticsRule.SRepeat.asl
  0
  1
  2
  3
  $ aslref SemanticsRule.SFor.asl
  0
  1
  2
  3
  $ aslref SemanticsRule.SThrowNone.asl
  $ aslref SemanticsRule.SThrowSomeTyped.asl
  $ aslref SemanticsRule.SThrowSTry.asl
  aslref cannot find file "SemanticsRule.SThrowSTry.asl"
  [1]
  $ aslref SemanticsRule.Loop.asl
  $ aslref SemanticsRule.For.asl
  $ aslref SemanticsRule.Catch.asl
  $ aslref SemanticsRule.CatchNamed.asl
  $ aslref SemanticsRule.CatchOtherwise.asl
  $ aslref SemanticsRule.CatchNone.asl
  File SemanticsRule.CatchNone.asl, line 15, characters 8 to 24:
  ASL Error: Cannot parse.
  [1]
  $ aslref SemanticsRule.FUndefIdent.asl
  File SemanticsRule.FUndefIdent.asl, line 4, characters 5 to 12:
  ASL Error: Undefined identifier: 'foo'
  [1]
  $ aslref SemanticsRule.FCall.asl
  $ aslref SemanticsRule.FPrimitive.asl
  Hello, world!
  $ aslref SemanticsRule.PAll.asl
  $ aslref SemanticsRule.PAnyTRUE.asl
  $ aslref SemanticsRule.PAnyFALSE.asl
  $ aslref SemanticsRule.PGeqTRUE.asl
  $ aslref SemanticsRule.PGeqFALSE.asl
  $ aslref SemanticsRule.PLeqTRUE.asl
  $ aslref SemanticsRule.PLeqFALSE.asl
  $ aslref SemanticsRule.PNotTRUE.asl
  $ aslref SemanticsRule.PNotFALSE.asl
  $ aslref SemanticsRule.PRangeTRUE.asl
  $ aslref SemanticsRule.PRangeFALSE.asl
  $ aslref SemanticsRule.PSingleTRUE.asl
  $ aslref SemanticsRule.PSingleFALSE.asl
  $ aslref SemanticsRule.PMaskTRUE.asl
  $ aslref SemanticsRule.PMaskFALSE.asl
  $ aslref SemanticsRule.PTupleTRUE.asl
  $ aslref SemanticsRule.PTupleFALSE.asl
  $ aslref SemanticsRule.CTCValue.asl
  $ aslref SemanticsRule.CTCError.asl
  $ aslref -0 SemanticsRule.LEUndefIdentV0.asl
  $ aslref SemanticsRule.LEUndefIdentV1.asl
  File SemanticsRule.LEUndefIdentV1.asl, line 5, characters 2 to 3:
  ASL Error: Undefined identifier: 'y'
  [1]
  $ aslref SemanticsRule.LESlice.asl
  $ aslref SemanticsRule.LESetField.asl
  $ aslref SemanticsRule.LEDestructuring.asl
  $ aslref SemanticsRule.LDVar0.asl
  $ aslref SemanticsRule.LDVar1.asl
  $ aslref SemanticsRule.SliceSingle.asl
  $ aslref SemanticsRule.SliceLength.asl
  $ aslref SemanticsRule.SliceRange.asl
  $ aslref SemanticsRule.SliceStar.asl
  $ aslref SemanticsRule.LDTuple.asl
  $ aslref SemanticsRule.LDTypedTuple.asl
  $ aslref SemanticsRule.LDTypedVar.asl
  $ aslref SemanticsRule.LDUninitialisedTuple.asl
  File SemanticsRule.LDUninitialisedTuple.asl, line 4, characters 2 to 33:
  Unsupported declaration: (x: integer, y: boolean).
  [1]
  $ aslref SemanticsRule.LDTyped.asl
  $ aslref SemanticsRule.LDUninitialisedTyped.asl
  $ aslref SemanticsRule.SAssign.asl
  $ aslref SemanticsRule.SCall.asl
  $ aslref SemanticsRule.SDeclNone.asl
  $ aslref SemanticsRule.SDeclSome.asl
  $ aslref SemanticsRule.SPass.asl
  $ aslref SemanticsRule.SReturnOne.asl
  $ aslref SemanticsRule.SReturnSome.asl
  $ aslref SemanticsRule.SSeq.asl
  $ aslref SemanticsRule.Block.asl
  $ aslref SemanticsRule.SAssignCall.asl
  $ aslref SemanticsRule.SAssignTuple.asl
  $ aslref SemanticsRule.EBinopImplExFalso.asl
  $ aslref SemanticsRule.EBinopOrTrue.asl
  $ aslref SemanticsRule.EBinopAndFalse.asl
  $ aslref SemanticsRule.SAssertOk.asl
  $ aslref SemanticsRule.SAssertNo.asl
  File SemanticsRule.SAssertNo.asl, line 4, characters 10 to 17:
  ASL Execution error: Assertion failed: (42 == 3)
  [1]
  $ aslref SemanticsRule.LEDiscard.asl
  $ aslref SemanticsRule.LDDiscard.asl
