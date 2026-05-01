# Getting started with ASLRef

## Disclaimer

This material covers ASL1, the latest version of Architecture Specification
Language (ASL). ASL appears in the [Arm Architecture Reference Manual](https://developer.arm.com/documentation/ddi0487/latest).
In addition to making contributions to this repository, Arm officially
maintains and regularly releases the ASL Reference, the formal language
definition for ASL1, available on
[Arm Developer](https://developer.arm.com/Architectures/Architecture%20Specification%20Language).

We welcome feedback, questions, and feature requests — please contact us by
writing to [atg-formal@arm.com](mailto:atg-formal@arm.com) or raise issues and
pull requests to the herdtools7 GitHub repository.

## Installation

### Pre-requisites

Install ocaml and opam (ocaml package manager), see
[the manual](https://ocaml.org/docs/up-and-running#installing-ocaml). For
example on MacOS:
```bash
$ brew install opam
```

### Installing the release version

If you want to use the last released version in opam, simply use:
```bash
$ opam install aslref
```

To uninstall, simply run
```bash
$ opam remove aslref
```

### Installing from sources

1. Clone herdtools7:
    ```bash
    $ git clone https://github.com/herd/herdtools7.git
    ```
2. Ensure the opam environment is up-to-date. For example for bash:
   ```bash
   $ eval $(opam env)
   ```
3. Install dependencies:
   ```bash
   $ cd herdtools7
   $ dune build asllib/aslref.opam
   $ opam install ./asllib --deps-only
   ```
4. Build and install into a location `$PREFIX`:
   ```bash
   $ make build-aslref # optional
   $ make install-aslref PREFIX=$PREFIX
   ```

It's done!

To uninstall, you can remove the binaries from `$PREFIX`. You can also run:
```bash
$ make uninstall-aslref PREFIX=$PREFIX
```

### Checking

If `$PREFIX/bin` is in your `$PATH`, the following command should return a similar output:
```bash
$ aslref --version
aslref version 7.56+03 rev 7aa9d1f3cee2598ec64f14372f210e008ac5510f
```

## Usage

### Basics

If `my-test.asl` contains a valid ASL specification returning 0, the tool `aslref`
does not print anything and exits with code 0.
```bash
$ aslref my-test.asl
```

### Version and type-checking flags

For a complete reference of arguments, see `aslref --help`.

#### ASL Version

To use the ASLv0 parser, use the `-0` flag.

The default parser is the ASLv1, but you can still specify it with `-1`.

#### Type-checking

There are currently three possible type-checking settings, listed here from the
strongest to the weakest:
1. `--type-check-strict` fails on the first error encountered while
   type-checking the specification. This is the default setting for ASLv1.
2. `--type-check-warn` logs every error on the standard error output, but does
   not fail on any of them. The specification might not be able to run through
   the interpreter if the type-checking phase failed.
3. `--no-type-check` only performs minimal type-inference. Tries to fail as
   little as possible. This is the default for ASLv0.

