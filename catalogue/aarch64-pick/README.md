aarch64-pick: Catalogue for AArch64 with pick dependencies
===========================================================

This catalogue contains artifacts from the work on the pick dependency
semantics of the AArch64 memory model. The tests in this catalogue use
instructions with a pick dependency semantics such as CSEL and CAS.

Simulating with herd7
---------------------

    % herd7 -kinds tests/kinds.txt tests/@all

Running with litmus7
--------------------

    # Assuming there exists a folder at the path ${TESTS_DIR}
    % litmus7 -a 2 -kinds tests/kinds.txt -o ${TESTS_DIR}/ catalogue/tests/@all
    % cd ${TESTS_DIR}
    % make
    % sh ./run.sh

> **_NOTE:_** that litmus7 and the C compiler might emit warnings for the tests:
> - CoRW2+posq0q0+q0,
> - CoRW2+posw0w0+w0,
> - LB+dmb.sy+data-wsi-wsi+MIXED,
> - MP+QAmo+AcqAmo,
> - MP+dmb.syw0w4+dataw4w4-rfiw4q0, and
> - MP+popl.w0-posl.w0q0+poa.w4p.
>
> These are known and do not affect the correctness of these tests.
