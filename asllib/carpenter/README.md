# ASL Carpenter

Carpenter is an AST generator for ASL, working in two different modes:

1. Enumeration of all ASTs by increasing size;
2. Random generation of ASTs following type-checking constraints.

Both modes support constraints on syntax nodes available, and on execution or
type-checking rules used by ASLRef to execute it.

It can also serve as a fuzzing tool, or a mass execution tool.

## Installation

Dependencies:
```bash
opam install dune menhir qcheck zarith re feat fix cmdliner logs
```

Then:
```bash
dune build asllib/carpenter
```

## Usage

See `dune exec carpenter -- --help` for a detailed view of options.

Generating 10 tests and writing them in the `tmp` directory (which is supposed
to exist):
```bash
dune exec carpenter -- generate random -o tmp --small 10
```

Executing them:
```bash
dune exec carpenter -- execute $(find tmp -name "$(date +"%Y-%m-%dT%H")*.asl")
```
This should print a list of results for each file, for example:
```
// results for file: ../../tmp/2026-06-05T10:23:11-0006.asl
Error: File ../../tmp/2026-06-05T10:23:11-0006.asl, line 7, characters 2 to 35:
  var rwqhgkjdozr: boolean = FALSE;
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
ASL Type error: not all control flow paths of the function "main" are
  guaranteed to either return, raise an exception, or invoke unreachable.
// end of results for file: ../../tmp/2026-06-05T10:23:11-0006.asl

...
```

Fuzzing ASLRef on itself on 1000 tests:
```bash
dune exec carpenter -- fuzz enum -o tmp 1000
```
If this does not print anything and returns with a normal exit code, everything
is fine; otherwise, you've found a bug in carpenter or in ASLRef!



