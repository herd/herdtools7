# ASL Reference Review Worklist

Source order: [ASLReference.tex](/Users/romman01/arm/herdtools7-master/asllib/doc/ASLReference.tex)

Status legend: `pending`, `in_progress`, `blocked`, `done`

| # | File | Status | Notes |
|---|------|--------|-------|
| 1 | `notice.tex` | done | Skipped per user instruction. |
| 2 | `disclaimer.tex` | done | Skipped per user instruction. |
| 3 | `introduction.tex` | done | No blocking issue found before reaching `ASLFormal.tex`. |
| 4 | `ASLFormal.tex` | blocked | Verified fix for transitive closure. New issue: `Listing a Set` gives malformed relation signature `\\listset{\\pow{T}} \\cartimes \\KleeneStar{T}`; it needs an explicit symbol/type form, likely a function signature. |
| 5 | `LexicalStructure.tex` | pending | |
| 6 | `Syntax.tex` | pending | |
| 7 | `AbstractSyntax.tex` | pending | |
| 8 | `TypeChecking.tex` | pending | |
| 9 | `Semantics.tex` | pending | |
| 10 | `Literals.tex` | pending | |
| 11 | `PrimitiveOperations.tex` | pending | |
| 12 | `Types.tex` | pending | |
| 13 | `Slicing.tex` | pending | |
| 14 | `Bitfields.tex` | pending | |
| 15 | `Expressions.tex` | pending | |
| 16 | `PatternMatching.tex` | pending | |
| 17 | `AssignableExpressions.tex` | pending | |
| 18 | `LocalStorageDeclarations.tex` | pending | |
| 19 | `Statements.tex` | pending | |
| 20 | `BlockStatements.tex` | pending | |
| 21 | `CatchingExceptions.tex` | pending | |
| 22 | `SubprogramCalls.tex` | pending | |
| 23 | `GlobalStorageDeclarations.tex` | pending | |
| 24 | `TypeDeclarations.tex` | pending | |
| 25 | `SubprogramDeclarations.tex` | pending | |
| 26 | `GlobalPragmas.tex` | pending | |
| 27 | `Specifications.tex` | pending | |
| 28 | `TopLevel.tex` | pending | |
| 29 | `SideEffects.tex` | pending | |
| 30 | `StaticEvaluation.tex` | pending | |
| 31 | `SymbolicSubsumptionTesting.tex` | pending | |
| 32 | `SymbolicEquivalenceTesting.tex` | pending | |
| 33 | `TypeSystemUtilities.tex` | pending | |
| 34 | `SemanticsUtilities.tex` | pending | |
| 35 | `RuntimeEnvironment.tex` | pending | |
| 36 | `ErrorCodes.tex` | pending | |
| 37 | `StandardLibrary.tex` | pending | |

## Learnings

- Render order is driven by [ASLReference.tex](/Users/romman01/arm/herdtools7-master/asllib/doc/ASLReference.tex).
- `generated_macros.tex` is included in the preamble but is not part of the rendered chapter/section worklist.
- `notice.tex` and `disclaimer.tex` are out of scope for the review.
- When a `\Render...` macro appears in a `.tex` file, review it against the corresponding definition in [asl.spec](/Users/romman01/arm/herdtools7-master/asllib/doc/asl.spec). If the mapping is unclear, ask the user before proceeding.
- Fixed: [ASLFormal.tex](/Users/romman01/arm/herdtools7-master/asllib/doc/ASLFormal.tex) now states transitive closure over `E \\subseteq V \\cartimes V`.
- New blocking issue in [ASLFormal.tex](/Users/romman01/arm/herdtools7-master/asllib/doc/ASLFormal.tex): `Listing a Set` uses a malformed signature expression instead of an explicit function/relation signature.

## Current Review State

- Current file: `ASLFormal.tex`
- Current status: `blocked`
- Waiting on user fix: repair the `Listing a Set` signature around the definition of `\\listset`, likely to something like `\\listset : \\pow{T} \\rightarrow \\KleeneStar{T}`
