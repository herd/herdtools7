
ACL2 ASL interpreter
=====================

This directory contains an implementation of the ASLRef interpreter in the
[ACL2](https://www.cs.utexas.edu/~moore/acl2) theorem prover.

Installation
------------

The interpreter requires an up-to-date (Github bleeding-edge) copy of
the [ACL2 system and books](https://github.com/acl2/acl2). To install, see
[this installation guide](https://www.cs.utexas.edu/~moore/acl2/current/HTML/installation/installation.html).

Ensure the following environment variables are set and exported:

    ACL2_DIR=<path to the root of your ACL2 git clone>
    ACL2ASL_DIR=<this directory>
    ACL2=${ACL2_DIR}/saved_acl2
    ACL2_SYSTEM_BOOKS=${ACL2_DIR}/books
    ACL2_IMAGES=${ACL2ASL_DIR}/bin
    ACL2_IMAGE_SRC_DIR=${ACL2ASL_DIR}/image-src
    ACL2_PROJECTS=${ACL2ASL_DIR}/PROJECT_DIRS
    PATH=${ACL2_SYSTEM_BOOKS}/build:${ACL2ASL_DIR}/bin:$PATH

The `env.sh` script in this directory can be sourced to set up the latter
variables once the first two (ACL2_DIR and ACL2ASL_DIR) are set.

Once these are set, an ACL2 executable with the ASL interpreter code
pre-loaded can be built using (Gnu) `make` in this directory. Using
Make's parallel build capability is very useful to speed this up;
e.g., `make -j 8`.

To run examples, aslref must also be built and installed in your PATH --
see the README.mld in the parent directory.

Usage
-----

The ACL2 ASL interpreter reads an ASL type-checked AST which can be
dumped by aslref using the `--print-lisp` option. E.g.,

    aslref --print-lisp --no-exec mytest.asl > mytest.asl.lsp

The file `tests/run-interactive.lsp` can be used to run the ASL
program interactively in acl2asl as follows:

    (assign :fname "mytest.asl.lsp")
    (ld "run-interactive.lsp")

Proofs showing the correctness of the ASL stdlib functions are in
proofs/stdlib/. They can be run using `make` in that directory.

The script `bin/run-acl2asl.sh` takes an asl file as an argument, uses
aslref to write out the AST object, then runs the interpreter on that
object using acl2asl. It prints the output from the ACL2 ASL
interpreter and exits with either the status returned from the ASL
"main" function if it completed successfully, or 1 otherwise,
including in cases of runtime errors or uncaught exceptions.

Testing
-------

Running `make` in the aslreftests subdirectory replicates the aslref
regression tests using acl2asl. The output is a set of diffs showing
what the output/status of each run was supposed to be versus what it
is.

The output when there is an error differs between aslref and acl2asl;
we aren't likely to fix all these differences because this would
require reimplementing all the pretty printing. For now, when both
interpreters produce a dynamic error, we don't compare their outputs.

A lighter set of tests can be run using `make` in the tests
subdirectory. These consist of most aslref tests that are supposed to
complete without error.

Both of these test builds leave `*.t` files under their subdirectories,
which can disrupt dune-based testing of herdtools and asllib. Run
`make clean` in both test subdirectories to fix this.

