# ASL branch-decision reuse

**Goal:** Reuse repeated bit-slice comparisons within each symbolic ASL path,
always enabled, without changing Arm pseudocode or the shared event monad.

**Design:** The interpreter carries immutable `(value, bool)` assumptions in
its runtime environment. The herd backend identifies comparisons by their
symbolic source, slice positions and literal. Expression provenance is local
to one interpreter instance; branch decisions are never shared between paths.

## Constraints

- No command-line variant or feature toggle.
- Keep condition evaluation, branch events and chosen-branch constraints.
- Different symbolic reads, bit positions and literals remain distinct.
- Unknown expression shapes retain the existing two-way choice.
- Do not modify `eventsMonad.ml`, `valconstraint.ml` or ASL pseudocode.
- Do not commit without a user request.

## Tasks

- [x] Add real-herd Cram regressions in `herd/tests/branch-decisions` and
  fixtures in `herd/tests/instructions/ASL`. Assert two abstract paths for four
  repeated tests, independent paths after different inputs, and unchanged
  observations. Run `dune runtest herd/tests/branch-decisions` against the old
  backend and observe the repeated-condition count fail (16 instead of 2).
- [x] Add `branch_assumptions : (v * bool) list` and its constructor/update
  in `asllib/env.ml`/`env.mli`. Extend `Backend.S.choice` with optional `assumptions`;
  pass and record these in `Interpreter.choice`, independently of diagnostic
  path tracking. Let `NativeBackend.choice` accept and ignore assumptions.
- [x] Add `herd/ASLBranchDecisions.ml` with `create`, `record_slice`,
  `record_equal`, and `find` operating on `ASLValue.V.v`. Only record proven
  slice/literal equalities; never overwrite provenance or cache memory reads.
  Instantiate it in `ASLSem.build_semantics`, wrap slice/equality operations,
  and replace a known opposite branch with `M.zeroT` in `M.choiceT`.
- [x] Build `dune build _build/default/herd/herd.exe
  _build/default/internal/herd_regression_test.exe`. Run the Cram regressions,
  the ASL instruction suite, native and strict-ASL SVE suites, and ASLRef
  tests. Compare the Vxx native/ASL outputs with the existing 30-second script.
- [x] Review the diff for path isolation, preserved events and accidental
  edits; report measured candidate counts and remaining timeouts.

## Verification commands

```sh
dune runtest herd/tests/branch-decisions
./_build/default/internal/herd_regression_test.exe -herd-path ./_build/default/herd/herd.exe -libdir-path ./herd/libdir -litmus-dir ./herd/tests/instructions/ASL test
./_build/default/internal/herd_regression_test.exe -herd-path ./_build/default/herd/herd.exe -libdir-path ./herd/libdir -litmus-dir ./herd/tests/instructions/AArch64.sve -conf ./herd/tests/instructions/AArch64.sve/asl-strict.cfg -checkstates test
python3 -B -u herd/tests/instructions/AArch64.sve/run-asl-strict.py
```

## Results

- The repeated-condition regression failed on the original backend (16 abstract
  event structures instead of 2); the nested-path case gave 4 instead of 2.
  Both now pass, including true/false initial values. Function-call side effects
  still happen four times. Independent inputs and memory reads retain 16 and 4
  abstract event structures respectively.
- Unit tests cover identical conditions, reversed operands, different bits,
  sources, literals and literal widths, and isolation between paths/instances.
- Build, `dune runtest herd/tests/branch-decisions asllib`, the ASL instruction
  suite, native SVE suite, and configured strict-ASL SVE selection pass.
- Full 38-test native/strict-ASL comparison, with a 30-second timeout per run:
  27 Equal, 8 ASL timeout, 3 ISSUE WITHTest, no Non-equal. Previously: 21 Equal,
  14 ASL timeout, the same 3 issues.
- Newly completed tests: V06, V07, V11, V12, V18, V22. V18 (LD2W) completes
  in about 2.4 seconds; V22 is close to the limit at about 27.6 seconds.
- Remaining timeouts: V05, V10, V16, V17, V25, V26, V28, V30.
  Existing instruction-support issues: V31, V32, Y01.
- Small two-element LD2D/LD3D diagnostics: 32/128 candidates become 8/8;
  final observations remain `Always 1 0`. LD1D stays at 8 candidates.

This is exact-condition reuse, not general constraint solving: independent
predicate bits still branch, and their first decisions are not eliminated.
Timings are indicative, not regression assertions. These results were obtained
before extracting the optimization into its dedicated branch.
