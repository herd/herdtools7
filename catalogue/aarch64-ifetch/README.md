 aarch64-ifetch: Catalogue for instruction fetch semantics
=========================================================

This catalogue contains artifacts from the work on the instruction
fetch semantics of the AArch64 memory model. The tests in this
catalogue concurrently execute:
- Instructions that modify other instructions, and
- The instructions that are affected by the modication.

Simulating with herd7
---------------------

    % herd7 -variant ifetch tests/@all

Running with litmus7
--------------------

ifetch tests need to be handled in a special way by litmus7. This is
because ifetch litmus tests modify a memory location and execute the
instructions held in that same memory location. As a result, litmus7
needs to use memory that is both writable and executable. This is
enabled with the command line argument `-variant self`.

For operating systems (for example, Linux based) where it is
possible to execute such binaries in EL0, the litmus tests can be run
as follows:

    # Assuming there exists a folder at the path ${TESTS_DIR}
    % litmus7 -variant self -a 2 -o ${TESTS_DIR} tests/@all
    % cd ${TESTS_DIR}
    % make
    % sh ./run.sh

For operating systems where it is *not* possible to execute such
binaries in EL0, the ifetch litmus tests can be run in EL1 using
hardware virtualization. The command line argument `-mach kvm-aarch64`
instructs litmus7 to generate such tests. In this configuration,
litmus7 generates source code that dependents on
[kvm-unit-tests](http://www.linux-kvm.org/page/KVM-unit-tests). Assuming
that the user has downloaded a copy of kvm-unit-test and build
according to the project's
[instructions]{https://gitlab.com/kvm-unit-tests/kvm-unit-tests/-/blob/master/README.md?ref_type=heads#building-the-tests)
in the folder ${KUT_DIR}:

    % mkdir ${KUT_DIR}/litmus
    % litmus7 -mach kvm-aarch64 -variant self -a 2 -o ${KUT_DIR}/litmus tests/@all
    % cd ${KUT_DIR}/litmus
    % make
    % cd ..
    % sh ./litmus/run.sh


> **_NOTE:_** litmus7 and the C compiler might emit warnings for the tests:
> - MP.FF+dc.cvau-dsb.ish-ic.ivau-dsb.ish+po,
> - MP.FF+dc.cvau-dmb.ish+dsb.ish-ic.ivau-dsb.ish-rfiINSTNOP, and
> - MP.FF+dc.cvau-dsb.ish-ic.ivau-dsb.ish+dmb.ish+rfiINST.
>
> These are known and do not affect the correctness of these tests.
