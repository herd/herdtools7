# The ASL Semantics Specification Tool

aslspec is a domain-specific language (DSL) for specifying
the meaning of the ASL language and a tool to check and operate
on such a (meta-)specification.

Further details on the aslspec language are available in
the [language documentation](aslspec.md).

## Installation

Dependencies:
```bash
opam install dune menhir qcheck zarith re feat fix cmdliner logs
```

Then:
```bash
dune build asllib/aslspec
```

## Usage

See `dune exec aslspec -- --help` for a detailed view of options.
