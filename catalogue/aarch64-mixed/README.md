aarch64-mixed: Catalogue for AArch64 with mixed size semantics
==============================================================

This catalogue contains artifacts from the work on the mixed-size
semantics of the AArch64 memory model. The tests in this catalogue
execute concurrently memory instructions with overlapping Memory
effects (for example, in CO-MIXED-20cc+H; P0 executes a STR byte to x
and LDR half-word from x, and P1 executes a STR half-word to x).

Simulating with herd7
---------------------

    % herd7 -variant mixed -kinds tests/kinds.txt tests/@all

Running with litmus7
--------------------

    # Assuming there exists a folder at the path ${TESTS_DIR}
    % litmus7 -a 2 -kinds tests/kinds.txt -o ${TESTS_DIR}/ tests/@all
    % cd ${TESTS_DIR}
    % make
    % sh ./run.sh

> **_NOTE:_** litmus7 and the C compiler might emit warnings for the tests:
> - CoRW2+posq0q0+q0.litmus,
> - CoRW2+posq0q0+q0.litmus,
> - CoRW2+posw0w0+w0.litmus,
> - LB+dmb.sy+data-wsi-wsi+MIXED.litmus,
> - LB+dmb.sy+data-wsi-wsi+MIXED.litmus,
> - MP+QAmo+AcqAmo.litmus,
> - MP+QAmo+AcqAmo.litmus,
> - MP+QAmo+AcqAmo.litmus,
> - MP+dmb.syw0w4+dataw4w4-rfiw4q0.litmus,
> - MP+dmb.syw0w4+dataw4w4-rfiw4q0.litmus,
> - MP+dmb.syw0w4+dataw4w4-rfiw4q0.litmus,
> - MP+popl.w0-posl.w0q0+poa.w4p.litmus, and
> - MP+popl.w0-posl.w0q0+poa.w4p.litmus.
>
> These are known and do not affect the correctness of these tests.
