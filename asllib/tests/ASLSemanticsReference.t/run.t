Hello world should work:

  $ aslref hello_world.asl
  Hello, world!

ASL Semantics Tests:
  $ aslref SemanticsRule.Lit.asl
  $ aslref SemanticsRule.ELocalVar.asl
  $ aslref SemanticsRule.EGlobalVar.asl
  $ aslref SemanticsRule.EGlobalVarError.asl
  $ aslref SemanticsRule.EUndefIdent.asl
  File SemanticsRule.EUndefIdent.asl, line 5, characters 9 to 10:
    assert y;
           ^
  ASL Error: Undefined identifier: 'y'
  [1]
//  $ aslref SemanticsRule.EBinopPlusPrint.asl
  $ aslref SemanticsRule.EBinopPlusAssert.asl
  $ aslref SemanticsRule.EBinopDIVBackendDefinedError.asl
  File SemanticsRule.EBinopDIVBackendDefinedError.asl, line 4,
    characters 10 to 17:
  All values in constraints {0} would fail with op DIV, operation will always
  fail.
  File SemanticsRule.EBinopDIVBackendDefinedError.asl, line 4,
    characters 10 to 17:
    let x = 3 DIV 0;
            ^^^^^^^
  ASL Typing error: Illegal application of operator DIV on types integer {3}
    and integer {0}.
  [1]
  $ aslref --no-type-check SemanticsRule.EBinopDIVBackendDefinedError.asl
  File SemanticsRule.EBinopDIVBackendDefinedError.asl, line 4,
    characters 10 to 17:
  All values in constraints {0} would fail with op DIV, operation will always
  fail.
  ASL Dynamic error: Illegal application of operator DIV for values 3 and 0.
  [1]
  $ aslref SemanticsRule.EUnopAssert.asl
  $ aslref SemanticsRule.ECondFALSE.asl
  $ aslref SemanticsRule.ECondARBITRARY3or42.asl
  File SemanticsRule.ECondARBITRARY3or42.asl, line 10, characters 9 to 13:
    assert x==3;
           ^^^^
  ASL Execution error: Assertion failed: (x == 3).
  [1]
  $ aslref SemanticsRule.ESlice.asl
  $ aslref SemanticsRule.ECall.asl
  $ aslref SemanticsRule.EGetArray.asl
  $ aslref SemanticsRule.EGetArrayTooSmall.asl
  ASL Execution error: Mismatch type:
    value 3 does not belong to type integer {0..2}.
  [1]
  $ aslref SemanticsRule.ERecord.asl
  $ aslref SemanticsRule.EGetItem.asl
  $ aslref SemanticsRule.EConcat.asl
  $ aslref SemanticsRule.ETuple.asl
  $ aslref SemanticsRule.EArbitraryInteger0.asl
  $ aslref SemanticsRule.EArbitraryInteger3.asl
  File SemanticsRule.EArbitraryInteger3.asl, line 5, characters 9 to 13:
    assert x==3;
           ^^^^
  ASL Execution error: Assertion failed: (x == 3).
  [1]
  $ aslref SemanticsRule.EArbitraryIntegerRange3-42-3.asl
  $ aslref SemanticsRule.EArbitraryIntegerRange3-42-42.asl
  File SemanticsRule.EArbitraryIntegerRange3-42-42.asl, line 5,
    characters 9 to 14:
    assert x==42;
           ^^^^^
  ASL Execution error: Assertion failed: (x == 42).
  [1]
  $ aslref SemanticsRule.EArbitraryArray.asl
  $ aslref SemanticsRule.EPattern.asl
  $ aslref SemanticsRule.LELocalVar.asl
  $ aslref SemanticsRule.LESetArray.asl
  $ aslref SemanticsRule.LESetEnumArray.asl
  $ aslref SemanticsRule.SReturnNone.asl
  $ aslref SemanticsRule.SCond.asl
  $ aslref SemanticsRule.SCase.asl
  $ aslref SemanticsRule.SWhile.asl
  evaluated limit = 4
  testing 0 <= 3
  i = 0
  testing 1 <= 3
  i = 1
  testing 2 <= 3
  i = 2
  testing 3 <= 3
  i = 3
  testing 4 <= 3
  $ aslref SemanticsRule.SWhile.limit_reached.asl
  File SemanticsRule.SWhile.limit_reached.asl, line 4, character 2 to line 7,
    character 7:
    while i <= 3 looplimit 2 do
      assert i <= 3;
      i = i + 1;
     end;
  ASL Dynamic error: loop limit reached.
  [1]
  $ aslref SemanticsRule.SRepeat.asl
  File SemanticsRule.SRepeat.asl, line 24, character 4 to line 31, character 17:
      repeat
          println("i = ", i);
          assert i < 5;
          if x[i] == '1' then
              ones = ones + 1;
          end;
          i = i + 1;
      until i == 5;
  ASL Warning: Loop does not have a limit.
  j = 0
  j = 1
  j = 2
  j = 3
  j = 4
  #ones in x = 5
  i = 0
  i = 1
  i = 2
  i = 3
  i = 4
  #ones in x = 5
  $ aslref SemanticsRule.SFor.asl
  j = 0
  j = 1
  j = 2
  j = 3
  j = 4
  #ones in x = 5
  i = 4
  i = 3
  i = 2
  i = 1
  i = 0
  #ones in x = 5
  $ aslref SemanticsRule.SThrowNone.asl
  $ aslref SemanticsRule.SThrowSomeTyped.asl
  $ aslref SemanticsRule.SThrowSTry.asl
  aslref cannot find file "SemanticsRule.SThrowSTry.asl"
  [1]
  $ aslref SemanticsRule.SPrint.asl
  string_number_1
  0
  1000000
  53170898287292728730499578000
  TRUE
  FALSE
  12345678900123456789/10000000000
  0
  hello\world
  	 "here I am "
  0xd
  0x
  LABEL_B
  $ aslref SemanticsRule.RethrowImplicit.asl
  Exception value A
  Exception value A
  $ aslref SemanticsRule.Loop.asl
  $ aslref SemanticsRule.For.asl
  $ aslref SemanticsRule.Catch.asl
  $ aslref SemanticsRule.CatchNamed.asl
  $ aslref SemanticsRule.CatchOtherwise.asl
  Otherwise
  $ aslref SemanticsRule.CatchNone.asl
  File SemanticsRule.CatchNone.asl, line 15, characters 8 to 24:
    catch MyExceptionType1;
          ^^^^^^^^^^^^^^^^
  ASL Error: Cannot parse.
  [1]
  $ aslref SemanticsRule.FUndefIdent.asl
  File SemanticsRule.FUndefIdent.asl, line 4, characters 5 to 12:
       foo ();
       ^^^^^^^
  ASL Error: Undefined identifier: 'foo'
  [1]
  $ aslref SemanticsRule.FCall.asl
  $ aslref SemanticsRule.PAll.asl
  $ aslref SemanticsRule.PAny.asl
  $ aslref SemanticsRule.PGeq.asl
  $ aslref SemanticsRule.PLeq.asl
  $ aslref SemanticsRule.PNot.asl
  $ aslref SemanticsRule.PRange.asl
  $ aslref SemanticsRule.PSingle.asl
  $ aslref SemanticsRule.PMask.asl
  $ aslref SemanticsRule.PTuple.asl
  $ aslref SemanticsRule.ATCValue.asl
  $ aslref -0 SemanticsRule.LEUndefIdentV0.asl
  $ aslref SemanticsRule.LEUndefIdentV1.asl
  File SemanticsRule.LEUndefIdentV1.asl, line 5, characters 2 to 3:
    y = 3;
    ^
  ASL Error: Undefined identifier: 'y'
  [1]
  $ aslref SemanticsRule.LESlice.asl
  $ aslref SemanticsRule.LESetField.asl
  $ aslref SemanticsRule.LEDestructuring.asl
  $ aslref SemanticsRule.SliceSingle.asl
  $ aslref SemanticsRule.SliceLength.asl
  $ aslref SemanticsRule.SliceRange.asl
  $ aslref SemanticsRule.SliceStar.asl
  $ aslref SemanticsRule.SliceFromZero.asl
  $ aslref SemanticsRule.Slices.asl
  $ aslref SemanticsRule.LDTuple.asl
  $ aslref SemanticsRule.LDTypedTuple.asl
  $ aslref SemanticsRule.LDTypedVar.asl
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
    assert (42 == 3);
            ^^^^^^^
  ASL Execution error: Assertion failed: (42 == 3).
  [1]
  $ aslref SemanticsRule.LEDiscard.asl
  $ aslref SemanticsRule.LDDiscard.asl
  File SemanticsRule.LDDiscard.asl, line 4, characters 6 to 7:
    var - : integer;
        ^
  ASL Grammar error: Obsolete syntax: Discarded storage declaration.
  [1]

  $ aslref EvalCatchers.asl
  21
  $ aslref SemanticsRule.ATCNotDynamicErrorIfFalse.asl
  $ aslref SemanticsRule.ATCVariousErrors.asl
  File SemanticsRule.ATCVariousErrors.asl, line 4, characters 2 to 30:
    var b: integer{4, 5, 6} = 2;                     // A type error
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Typing error: a subtype of integer {4, 5, 6} was expected,
    provided integer {2}.
  [1]
  $ aslref SemanticsRule.CatchNoThrow.asl
  No exception raised
  $ aslref SemanticsRule.EConcat2.asl
  $ aslref SemanticsRule.LEGlobalVar.asl
  $ aslref SemanticsRule.SCond2.asl
  $ aslref SemanticsRule.SCond3.asl
  File SemanticsRule.SCond3.asl, line 3, characters 9 to 14:
    assert FALSE;
           ^^^^^
  ASL Execution error: Assertion failed: FALSE.
  [1]
  $ aslref SemanticsRule.SCond4.asl
  $ aslref SemanticsRule.STry.asl
  $ aslref SemanticsRule.EGetEnumArray.asl
  $ aslref SemanticsRule.CheckNonOverlappingSlices.asl
  $ aslref SemanticsRule.CheckNonOverlappingSlices.bad.asl
  ASL Dynamic error: overlapping slices (N - 2)+:2, 0+:1.
  [1]
  $ aslref SemanticsRule.DeclareGlobal.asl
  $ aslref SemanticsRule.EvalGlobals.asl
  $ aslref SemanticsRule.MatchFuncRes.asl
  $ aslref SemanticsRule.CheckRecurseLimit.asl
  $ aslref SemanticsRule.CheckRecurseLimit.no_limit.asl
  File SemanticsRule.CheckRecurseLimit.no_limit.asl, line 1, character 0 to
    line 4, character 4:
  func factorial(n: integer) => integer
  begin
      return if n == 0 then 1 else n * factorial(n - 1);
  end;
  ASL Warning: the recursive function factorial has no recursive limit
  annotation.
  $ aslref SemanticsRule.CheckRecurseLimit.limit_reached.asl
  File SemanticsRule.CheckRecurseLimit.limit_reached.asl, line 3,
    characters 37 to 53:
      return if n == 0 then 1 else n * factorial(n - 1);
                                       ^^^^^^^^^^^^^^^^
  ASL Dynamic error: recursion limit reached.
  [1]
  $ aslref SemanticsRule.AssignArgs.asl
  $ aslref SemanticsRule.Call.asl
