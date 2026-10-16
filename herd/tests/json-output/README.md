JSON instruction references
==========================

Each execution graph has an `instructions` object, keyed first by processor
number and then by dynamic program-order index (`poi`). JSON object keys are
decimal strings. Instruction-backed events carry numeric `proc` and `poi`
fields; look up their instruction with `instructions[String(proc)][String(poi)]`.
Initial and spurious events have neither field.

Each entry contains `instruction` and `static_poi`. `function` is present only
for `"fault_handler"`; its absence means `"main"`. Static indices identify loaded
code positions within a function, while dynamic indices distinguish repeated
executions and continue through fault handlers. The map contains the occurrences
referenced by the graph's displayed events; indices need not be contiguous.

For self-modifying code, resolved instruction-fetch values supply the instruction
text. `static_poi` still identifies the original code position, whose source text
may differ. Generated instructions, such as `.pagealign` padding branches, may
have static indices absent from `test.program`. Consumers must allow this.
Instruction labels remain in `test.program` rather than being repeated on events.
