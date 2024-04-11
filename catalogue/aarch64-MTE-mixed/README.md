aarch64-MTE-mixed: Catalogue for AArch64 with MTE and mixed size semantics
==========================================================================

This catalogue contains artifacts from the work on the conjuction of
MTE and mixed-size ordering semantics of the AArch64 memory model.

Simulating with herd7
---------------------

    % herd7 -variant mte,sync,mixed tests/@all

Running with litmus7
--------------------

Currently, litmus7 does not have support for the tests in this
catalogue.
