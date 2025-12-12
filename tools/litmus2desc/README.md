# litmus2desc

`litmus2desc` generates English prose describing an AArch64 litmus test. It
explains candidate executions that satisfy the test's final condition, covering
a selection of the test's instructions and the Effects they generate.

The goal is to turn a compact litmus-test outcome into text that is easier to
read in documentation, reviews, and reader-facing explanations. If several
satisfying candidate executions lead to identical prose, the tool prints that
description only once. If no candidate execution satisfies the post-condition,
the CLI exits with an error.

## Usage

```sh
litmus2desc [options] FILE
```

Options:

- `-set-libdir PATH`, `--set-libdir PATH`: override the herd library directory.
  Without this option, the tool uses herd's default library directory.
- `--latex`, `-l`: render the execution description as LaTeX item lists and
  paragraph headings.
- `--describe-dep-path`: include intermediate instructions when describing
  dependencies. By default, dependencies are described only from their
  source instruction to their target instruction.

`FILE` must be a supported AArch64 litmus test. Tests for other architectures,
and tests using unsupported variants, are rejected.

## Scope

The scope of what `litmus2desc` can handle right now is limited. For each
satisfying candidate execution, `litmus2desc` can currently describe:

- Load (Acquire) and Store (Release) instructions, and their Explicit Memory Read and Write Effects;
- Barriers and their Effects;
- Data, Address, and Control dependencies;
- final register constraints such as `0:X1=1`;
- final memory constraints such as `[x]=1`.

The test suite is a good place to get an idea of what features the tool supports
and how it supports them.

Extensions to the tool to support Register Effects and other variants (in
particular VMSA and ifetch) are work in progress.

## Internal Assumptions

`litmus2desc` depends on graph data from herd to explain candidate executions.

The implementation is strictly AArch64-only, and makes some assumptions about the cat model being used.
In particular, we assume herd graphs can provide at least these relations:

- `data`, `addr`, and `ctrl`, used as the dependency endpoints to describe;
- `dob`, used to decide whether a control dependency actually orders its
  source and target;
- `dtrm`, used only when `--describe-dep-path` is enabled, to recover
  intermediate instructions along dependency paths;
- `co`, used when explaining final memory values.

## Dependency Descriptions

The description of dependency paths via `--describe-dep-path` is incomplete and
somewhat heuristic. The rules underlying this flag are tailored to prose
generation for the `readers-guide`, and should be taken with a grain of salt
outside that context:

- for data dependencies, we look for `ADD` or `EOR` instructions along the
  `dtrm` path between the source and target events of a `data` edge;
- for address dependencies, we only look for `EOR` instructions along the
  `dtrm` path;
- for control dependencies, we look for commit events along the `dtrm` path.
