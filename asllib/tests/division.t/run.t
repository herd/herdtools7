Simple division checks:
  $ aslref examples.asl

Division by zero:

  $ aslref static-div-zero.asl
  File static-div-zero.asl, line 3, characters 19 to 26: All values in
  constraints {0} would fail with op DIV, operation will always fail.
  File static-div-zero.asl, line 3, characters 19 to 26:
  ASL Typing error: Illegal application of operator DIV on types integer {6}
    and integer {0}.
  [1]

  $ aslref static-divrm-zero.asl
  File static-divrm-zero.asl, line 3, characters 19 to 28: All values in
  constraints {0} would fail with op DIVRM, operation will always fail.
  File static-divrm-zero.asl, line 3, characters 19 to 28:
  ASL Typing error: Illegal application of operator DIVRM on types integer {6}
    and integer {0}.
  [1]

  $ aslref static-mod-zero.asl
  File static-mod-zero.asl, line 3, characters 19 to 26: All values in
  constraints {0} would fail with op MOD, operation will always fail.
  File static-mod-zero.asl, line 3, characters 19 to 26:
  ASL Typing error: Illegal application of operator MOD on types integer {6}
    and integer {0}.
  [1]

Unsupported divisions (caught at type-checking time):

  $ aslref static-div-neg.asl
  File static-div-neg.asl, line 3, characters 19 to 27: All values in
  constraints {(- 3)} would fail with op DIV, operation will always fail.
  File static-div-neg.asl, line 3, characters 19 to 27:
  ASL Typing error: Illegal application of operator DIV on types integer {6}
    and integer {(- 3)}.
  [1]

  $ aslref static-divrm-neg.asl
  File static-divrm-neg.asl, line 3, characters 19 to 29: All values in
  constraints {(- 3)} would fail with op DIVRM, operation will always fail.
  File static-divrm-neg.asl, line 3, characters 19 to 29:
  ASL Typing error: Illegal application of operator DIVRM on types integer {6}
    and integer {(- 3)}.
  [1]

  $ aslref static-mod-neg.asl
  File static-mod-neg.asl, line 3, characters 19 to 27: All values in
  constraints {(- 3)} would fail with op MOD, operation will always fail.
  File static-mod-neg.asl, line 3, characters 19 to 27:
  ASL Typing error: Illegal application of operator MOD on types integer {6}
    and integer {(- 3)}.
  [1]

  $ aslref --no-exec static-div-undiv.asl
  File static-div-undiv.asl, line 3, characters 19 to 26: Division will result
  in empty constraint set, so will always fail.
  File static-div-undiv.asl, line 3, characters 19 to 26:
  ASL Typing error: Illegal application of operator DIV on types integer {5}
    and integer {3}.
  [1]
  $ aslref --no-exec static-div-undiv-bis.asl
  File static-div-undiv-bis.asl, line 3, characters 11 to 18: Division will
  result in empty constraint set, so will always fail.
  File static-div-undiv-bis.asl, line 3, characters 11 to 18:
  ASL Typing error: Illegal application of operator DIV on types integer {1}
    and integer {2}.
  [1]
  $ aslref static-div-undiv-bis.asl
  File static-div-undiv-bis.asl, line 3, characters 11 to 18: Division will
  result in empty constraint set, so will always fail.
  File static-div-undiv-bis.asl, line 3, characters 11 to 18:
  ASL Typing error: Illegal application of operator DIV on types integer {1}
    and integer {2}.
  [1]
  $ aslref --no-exec static-div-undiv-ter.asl

  $ aslref --no-exec static-div-intervals.asl

  $ aslref --no-exec static-mod-intervals.asl

For completeness, those operations are dynamic errors:

  $ aslref dynamic-div-neg.asl
  ASL Dynamic error: Illegal application of operator DIV for values 6 and -3.
  [1]

  $ aslref dynamic-divrm-neg.asl
  ASL Dynamic error: Illegal application of operator DIVRM for values 6 and -3.
  [1]

  $ aslref dynamic-mod-neg.asl
  ASL Dynamic error: Illegal application of operator MOD for values 6 and -3.
  [1]

  $ aslref dynamic-div-zero.asl
  ASL Dynamic error: Illegal application of operator DIV for values 6 and 0.
  [1]

  $ aslref dynamic-divrm-zero.asl
  ASL Dynamic error: Illegal application of operator DIVRM for values 6 and 0.
  [1]

  $ aslref dynamic-mod-zero.asl
  ASL Dynamic error: Illegal application of operator MOD for values 6 and 0.
  [1]

  $ aslref dynamic-div-undiv.asl
  ASL Dynamic error: Illegal application of operator DIV for values 5 and 3.
  [1]

Parametric examples:
  $ aslref param-div-2.asl
  ASL Dynamic error: Illegal application of operator DIV for values 3 and 2.
  [1]

More complicated examples
=========================

Fails because N typing cannot infer that N + 1 is strictly positive.
  $ aslref div-by-param.asl
  ASL Dynamic error: Illegal application of operator DIV for values 5 and 2.
  [1]

Examples with multiple constraints in slices:
  $ aslref div-multi-slices.asl

  $ aslref div-multi-slices-zero.asl
  File div-multi-slices-zero.asl, line 6, characters 10 to 17:
  Warning: Removing some values that would fail with op DIV from constraint set
  {0, 1, 2} gave {1, 2}. Continuing with this constraint set.

Example with constant:

  $ aslref div-constants.asl
  File div-constants.asl, line 3, characters 22 to 29:
  ASL Static error: Illegal application of operator DIV for values 1 and 2.
  [1]

Other example from typing.t:
  $ aslref --no-exec TNegative9-1.asl
  File TNegative9-1.asl, line 3, characters 4 to 59:
  ASL Typing error: a subtype of bits(N) was expected,
    provided bits(((3 * N) DIV 4)).
  [1]
  $ aslref --no-exec TPositive9.asl

Other polynomial equations:
  $ aslref rat-poly-00.asl
  File rat-poly-00.asl, line 15, characters 9 to 19:
  ASL Typing error: Illegal application of operator == on types bits((7 DIV 2))
    and bits(3).
  [1]

  $ aslref rat-poly-01.asl
  ASL Dynamic error: Cannot extract from bitvector of length 0 slice 0+:-2.
  [1]
