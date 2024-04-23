aarch64-MTE: Catalogue for AArch64 with MTE and instructions with pick dependencies
===================================================================================

This catalogue contains artifacts from the work on the conjuction of
MTE and pick dependencies ordering semantics of the AArch64 memory
model.

Simulating with herd7
---------------------

    % herd7 -variant mte,sync tests/@all

Running with litmus7
--------------------

Currently, litmus7 does not have support for the tests in this
catalogue.
