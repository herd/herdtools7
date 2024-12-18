  $ aslref binop-read-read.asl
  $ aslref binop-read-write.asl
  File binop-read-write.asl, line 11, characters 10 to 31:
  ASL Typing error: conflicting side effects WritesGlobal "X" and ReadsGlobal "X"
  [1]
  $ aslref binop-write-write.asl
  File binop-write-write.asl, line 11, characters 10 to 47:
  ASL Typing error: conflicting side effects WritesGlobal "X" and WritesGlobal "X"
  [1]
  $ aslref binop-read-write-diff.asl
  $ aslref binop-write-write-diff.asl

  $ aslref bool-binop-write-write.asl
  Should print.
  Should print.

  $ aslref binop-throw-read.asl
  E caught
  $ aslref binop-throw-write.asl
  File binop-throw-write.asl, line 18, characters 12 to 43:
  ASL Typing error: conflicting side effects ThrowsException "E" and WritesGlobal "X"
  [1]
  $ aslref binop-throw-throw.asl
  File binop-throw-throw.asl, line 11, characters 12 to 37:
  ASL Typing error: conflicting side effects ThrowsException "E" and ThrowsException "E"
  [1]
  $ aslref binop-throw-caught.asl
  E caught
  $ aslref binop-throw-not-caught.asl
  File binop-throw-not-caught.asl, line 21, characters 12 to 37:
  ASL Typing error: conflicting side effects ThrowsException "E" and ThrowsException "E"
  [1]
  $ aslref binop-throw-otherwised.asl
  E caught

  $ aslref binop-throw-unknown.asl
  Caught E.
  $ aslref binop-write-unknown.asl
  $ aslref binop-unknown-unknown.asl

  $ aslref binop-throw-atc.asl
  File binop-throw-atc.asl, line 16, characters 12 to 41:
  ASL Typing error: conflicting side effects ThrowsException "E" and PerformsAssertions
  [1]
  $ aslref binop-write-atc.asl
  File binop-write-atc.asl, line 5, characters 10 to 11:
  ASL Execution error: Mismatch type:
    value 1 does not belong to type integer {2}.
  [1]
// We don't need to decide about the following:
// $ aslref binop-atc-atc.asl

  $ aslref constant-func.asl
  $ aslref constant-func-read.asl
  File constant-func-read.asl, line 9, characters 0 to 21:
  ASL Typing error: expected constant-time expression, got foo(4), which
    produces the following side-effects: [WritesGlobal "X"].
  [1]
  $ aslref constant-func-write.asl
  File constant-func-write.asl, line 9, characters 0 to 21:
  ASL Typing error: expected constant-time expression, got foo(4), which
    produces the following side-effects: [WritesGlobal "X"].
  [1]
  $ aslref constant-func-unknown.asl
  File constant-func-unknown.asl, line 7, characters 0 to 21:
  ASL Typing error: expected constant-time expression, got foo(4), which
    produces the following side-effects: [NonDeterministic].
  [1]
  $ aslref constant-func-throw.asl
  File constant-func-throw.asl, line 8, characters 0 to 21:
  ASL Typing error: expected constant-time expression, got foo(4), which
    produces the following side-effects: [ThrowsException "E"].
  [1]
  $ aslref constant-func-throw-caught.asl
  $ aslref constant-func-local-var.asl
  $ aslref constant-func-local-type-global-let.asl
  File constant-func-local-type-global-let.asl, line 12, characters 0 to 21:
  ASL Typing error: expected constant-time expression, got foo(8), which
    produces the following side-effects: [ReadsGlobal "K", PerformsAssertions].
  [1]
  $ aslref constant-func-local-type-local-let.asl
  $ aslref constant-func-sig-let.asl
  File constant-func-sig-let.asl, line 8, characters 0 to 20:
  ASL Typing error: expected constant-time expression, got foo(3), which
    produces the following side-effects: [ReadsGlobal "K"].
  [1]

  $ aslref for-var-no-edit.asl
  $ aslref for-var-edits.asl
  File for-var-edits.asl, line 6, character 2 to line 8, character 6:
  ASL Typing error: conflicting side effects ReadsLocal "x" and WritesLocal "x"
  [1]
  $ aslref for-read-write-global.asl
  File for-read-write-global.asl, line 14, character 2 to line 16, character 6:
  ASL Typing error: conflicting side effects ReadsGlobal "X" and WritesGlobal "X"
  [1]
  $ aslref while-var-edits.asl
  $ aslref repeat-var-edits.asl

  $ aslref for-read.asl
  $ aslref for-write.asl
  File for-write.asl, line 15, characters 15 to 25:
  ASL Typing error: a pure expression was expected, found write_X(), which
    produces the following side-effects: [WritesGlobal "X", ReadsGlobal "X"].
  [1]
  $ aslref for-write-throw.asl
  File for-write-throw.asl, line 13, characters 15 to 26:
  ASL Typing error: a pure expression was expected, found throwing(), which
    produces the following side-effects: [ThrowsException "E"].
  [1]
  $ aslref for-throw-throw.asl
  File for-throw-throw.asl, line 13, characters 15 to 26:
  ASL Typing error: a pure expression was expected, found throwing(), which
    produces the following side-effects: [ThrowsException "E"].
  [1]
  $ aslref for-throw.asl
  File for-throw.asl, line 13, characters 15 to 26:
  ASL Typing error: a pure expression was expected, found throwing(), which
    produces the following side-effects: [ThrowsException "E"].
  [1]
  $ aslref for-unknown.asl
  File for-unknown.asl, line 8, characters 15 to 25:
  ASL Typing error: a pure expression was expected, found unknown(), which
    produces the following side-effects: [NonDeterministic].
  [1]

  $ aslref config-uses-var.asl
  File config-uses-var.asl, line 2, characters 0 to 17:
  ASL Typing error: expected config-time expression, got (X + 3), which
    produces the following side-effects: [ReadsGlobal "X"].
  [1]
  $ aslref config-uses-config.asl
  $ aslref config-uses-let.asl
  File config-uses-let.asl, line 2, characters 0 to 13:
  ASL Typing error: expected config-time expression, got X, which produces the
    following side-effects: [ReadsGlobal "X"].
  [1]
  $ aslref config-uses-constant.asl
  $ aslref config-uses-local-var.asl
  $ aslref config-uses-local-let.asl
  $ aslref config-uses-local-constant.asl
  $ aslref config-uses-var-through-func.asl
  File config-uses-var-through-func.asl, line 8, characters 0 to 18:
  ASL Typing error: expected config-time expression, got foo(), which produces
    the following side-effects: [ReadsGlobal "X"].
  [1]
  $ aslref config-uses-config-through-func.asl
  $ aslref config-uses-let-through-func.asl
  File config-uses-let-through-func.asl, line 8, characters 0 to 18:
  ASL Typing error: expected config-time expression, got foo(), which produces
    the following side-effects: [ReadsGlobal "X"].
  [1]
  $ aslref config-uses-constant-through-func.asl
  $ aslref config-uses-atc.asl
  File config-uses-atc.asl, line 3, characters 9 to 10:
  ASL Execution error: Mismatch type:
    value 0 does not belong to type integer {10}.
  [1]
  $ aslref config-uses-unknown.asl
  File config-uses-unknown.asl, line 6, characters 0 to 18:
  ASL Typing error: expected config-time expression, got foo(), which produces
    the following side-effects: [NonDeterministic].
  [1]

  $ aslref assert-read.asl
  $ aslref assert-write.asl
  File assert-write.asl, line 12, characters 9 to 24:
  ASL Typing error: a pure expression was expected, found (write_X() == 0),
    which produces the following side-effects:
    [WritesGlobal "X", ReadsGlobal "X"].
  [1]
  $ aslref assert-throw.asl
  File assert-throw.asl, line 10, characters 9 to 25:
  ASL Typing error: a pure expression was expected, found (throwing() == 0),
    which produces the following side-effects: [ThrowsException "E"].
  [1]
  $ aslref assert-atc.asl
  File assert-atc.asl, line 3, characters 9 to 10:
  ASL Execution error: Mismatch type:
    value 0 does not belong to type integer {3}.
  [1]

  $ aslref type-read-config.asl
  $ aslref type-read-constant.asl
  $ aslref type-read-let.asl
  $ aslref type-read-local.asl
  File type-read-local.asl, line 5, characters 18 to 19:
  ASL Typing error: a pure expression was expected, found x, which produces the
    following side-effects: [ReadsLocal "x"].
  [1]
  $ aslref type-read-local-let.asl
  $ aslref type-read.asl
  File type-read.asl, line 3, characters 19 to 20:
  ASL Typing error: a pure expression was expected, found X, which produces the
    following side-effects: [ReadsGlobal "X"].
  [1]
  $ aslref type-write.asl
  File type-write.asl, line 10, characters 19 to 29:
  ASL Typing error: a pure expression was expected, found write_X(), which
    produces the following side-effects: [ReadsGlobal "X", WritesGlobal "X"].
  [1]
  $ aslref type-unknown.asl
  File type-unknown.asl, line 8, characters 23 to 33:
  ASL Typing error: a pure expression was expected, found unknown(), which
    produces the following side-effects: [NonDeterministic].
  [1]
  $ aslref type-func-atc.asl
  File type-func-atc.asl, line 3, characters 9 to 10:
  ASL Execution error: Mismatch type:
    value 0 does not belong to type integer {3}.
  [1]
  $ aslref type-func-local-var.asl
  $ aslref type-local-var.asl
  File type-local-var.asl, line 5, characters 15 to 16:
  ASL Typing error: a pure expression was expected, found x, which produces the
    following side-effects: [ReadsLocal "x"].
  [1]
  $ aslref type-throw.asl
  File type-throw.asl, line 8, characters 19 to 30:
  ASL Typing error: a pure expression was expected, found throwing(), which
    produces the following side-effects: [ThrowsException "E"].
  [1]

  $ aslref assert-atc.asl
  File assert-atc.asl, line 3, characters 9 to 10:
  ASL Execution error: Mismatch type:
    value 0 does not belong to type integer {3}.
  [1]
  $ aslref assert-read.asl
  $ aslref assert-throw.asl
  File assert-throw.asl, line 10, characters 9 to 25:
  ASL Typing error: a pure expression was expected, found (throwing() == 0),
    which produces the following side-effects: [ThrowsException "E"].
  [1]
  $ aslref assert-write.asl
  File assert-write.asl, line 12, characters 9 to 24:
  ASL Typing error: a pure expression was expected, found (write_X() == 0),
    which produces the following side-effects:
    [WritesGlobal "X", ReadsGlobal "X"].
  [1]
  $ aslref assert-unknown.asl

  $ aslref rec-assert-throw.asl
  File rec-assert-throw.asl, line 15, characters 9 to 37:
  ASL Typing error: a pure expression was expected,
    found (throwing((n - 1), FALSE) == 3), which produces the following
    side-effects: [CallsRecursive "throwing", ReadsLocal "n"].
  [1]
  $ aslref rec-binop-atc-throw.asl
  File rec-binop-atc-throw.asl, line 15, characters 10 to 54:
  ASL Typing error: conflicting side effects CallsRecursive "throwing" and PerformsAssertions
  [1]
  $ aslref rec-binop-read-throw.asl
  File rec-binop-read-throw.asl, line 22, characters 10 to 45:
  ASL Typing error: conflicting side effects CallsRecursive "throwing" and ReadsGlobal "X"
  [1]
  $ aslref rec-binop-unknown.asl
  $ aslref rec-binop-read.asl
  File rec-binop-read.asl, line 17, characters 10 to 42:
  ASL Typing error: conflicting side effects CallsRecursive "not_throwing" and ReadsGlobal "X"
  [1]
  $ aslref rec-binop-read-local.asl
  $ aslref rec-binop-write.asl
  File rec-binop-write.asl, line 18, characters 10 to 43:
  ASL Typing error: conflicting side effects CallsRecursive "not_throwing" and WritesGlobal "X"
  [1]
  $ aslref rec-assert.asl
  File rec-assert.asl, line 9, characters 10 to 51:
  ASL Typing error: conflicting side effects CallsRecursive "not_throwing" and PerformsAssertions
  [1]
  $ aslref rec-binop-atc.asl
  File rec-binop-atc.asl, line 9, characters 10 to 51:
  ASL Typing error: conflicting side effects CallsRecursive "not_throwing" and PerformsAssertions
  [1]
  $ aslref rec-binop-read-write.asl
  File rec-binop-read-write.asl, line 17, characters 10 to 42:
  ASL Typing error: conflicting side effects CallsRecursive "not_throwing" and ReadsGlobal "X"
  [1]
  $ aslref rec-binop-write-throw.asl
  File rec-binop-write-throw.asl, line 23, characters 10 to 46:
  ASL Typing error: conflicting side effects CallsRecursive "throwing" and WritesGlobal "X"
  [1]
  $ aslref rec-constant.asl
  $ aslref constant-rec.asl
  File constant-rec.asl, line 12, characters 2 to 23:
  ASL Typing error: expected constant-time expression, got foo(1), which
    produces the following side-effects: [CallsRecursive "foo"].
  [1]
  $ aslref rec-local-type.asl
  File rec-local-type.asl, line 12, characters 16 to 23:
  ASL Typing error: a pure expression was expected, found foo(0), which
    produces the following side-effects: [CallsRecursive "foo"].
  [1]
  $ aslref rec-binop-rec.asl
  File rec-binop-rec.asl, line 9, characters 10 to 35:
  ASL Typing error: conflicting side effects CallsRecursive "bar" and CallsRecursive "bar"
  [1]

  $ aslref print-var.asl
  0
  1
  2

  $ aslref global-var-initialisation.asl
  X = 5
  Y0 = 0
  Y1 = 1
  Y2 = 2
  Y3 = 3
  Y4 = 4
  $ aslref global-throw-initialisation.asl
  File global-throw-initialisation.asl, line 8, characters 0 to 29:
  ASL Execution error: unexpected exception E thrown during the evaluation of
    the initialisation of the global storage element "X".
  [1]
