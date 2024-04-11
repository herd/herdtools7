aarch64-MTE: Catalogue for AArch64 with MTE
===========================================

This catalogue contains artifacts from the work on the MTE semantics
of the AArch64 memory model. The tests is this catalogue execute
concurrently:
- Instructions that modify allocation tags, and
- Checked memory instructions that make use of the same allocation
tags.

Simulating with herd7
---------------------

    % herd7 -variant mte,sync -kinds tests/kinds.txt tests/@all

Running with litmus7
--------------------

Currently, litmus7 does not have support for the tests in this
catalogue.
