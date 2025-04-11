
ASL Standard Library ACL2 Correctness Proofs
============================================

This directory contains ACL2 proofs about the behavior of ASL standard
library definitions, located in this repository
[HERDTOOLS7](github.com/herd/herdtools7) in asllib/libdir/stdlib*.asl, as
interpreted by the ACL2 ASL interpreter (asllib/acl2/interp.lisp).

To run the proofs, first please follow the directions under
"Installation" in asllib/acl2/README.md. Then you may run `make` in
this directory to run the proofs.  Parallelism (e.g. `make -j 8`) can
be helpful in speeding up this process.

The build system for this directory doesn't recognize when ASLRef or
its standard library is updated. That is, if the proofs were
previously run to completion and ASLRef was subsequently updated,
running `make` will not rebuild anything. Run `make clean` first in
order to clear out the previous certificates so they will be actually
rebuilt.

Proofs may fail if ASL standard library functions change. If new
standard library functions are added but the existing functions remain
the same, the existing proofs should continue to work but a
completeness check in `top.lisp` will then fail. Same if an existing
proof is removed.

