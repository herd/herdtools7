Repeated symbolic conditions must not multiply scenarios. Keep both initial
choices, including across function calls and their side effects.

  $ run() { herd7 -set-libdir ../../libdir -v "$1" 2>&1 | sed -n 's/^Warning: //; /abstract event structures/p; /^Observation /p'; }
  $ run ../instructions/ASL/branch-decisions.litmus
  2 abstract event structures
  Observation BranchDecisions Always 1 0
  $ sed 's/x=1/x=0/; s/0:X0=4/0:X0=0/' ../instructions/ASL/branch-decisions.litmus > inactive.litmus
  $ run inactive.litmus
  2 abstract event structures
  Observation BranchDecisions Always 1 0

Decisions are local to each path, in both directions.

  $ run ../instructions/ASL/branch-decisions-paths.litmus
  2 abstract event structures
  Observation BranchDecisionsPaths Always 1 0
  $ sed 's/x=1/x=0/; s/0:X0=1/0:X0=4/' ../instructions/ASL/branch-decisions-paths.litmus > false-path.litmus
  $ run false-path.litmus
  2 abstract event structures
  Observation BranchDecisionsPaths Always 1 0

Do not equate different inputs or independent memory reads.

  $ run ../instructions/ASL/branch-decisions-inputs.litmus
  16 abstract event structures
  Observation BranchDecisionsInputs Always 1 0
  $ run ../instructions/ASL/branch-decisions-memory.litmus
  4 abstract event structures
  Observation BranchDecisionsMemory Sometimes 1 3

Recomputed arithmetic and conversions reuse both true and false decisions.

  $ run ../instructions/ASL/branch-decisions-expressions.litmus
  2 abstract event structures
  Observation BranchDecisionsExpressions Always 1 0
  $ sed 's/x=3/x=8/; s/0:X0=4/0:X0=0/' ../instructions/ASL/branch-decisions-expressions.litmus > above-limit.litmus
  $ run above-limit.litmus
  2 abstract event structures
  Observation BranchDecisionsExpressions Always 1 0

Reassigning a variable must not reuse its old condition's decision.

  $ run ../instructions/ASL/branch-decisions-updates.litmus
  4 abstract event structures
  Observation BranchDecisionsUpdates Always 1 0
