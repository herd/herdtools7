aarch64-ETS2: Catalogue with the semantics of Armv8-A VMSA with support for FEAT_ETS2
=====================================================================================

This catalogue contains artifacts from the work on the VMSA semantics
of the AArch64 memory model for systems that implement
FEAT_ETS2. The tests in this catalogue execute concurrently:
- Instructions that update Translation Table Descriptors, and
- Memory instructions that use the same Translation Table Descriptors.

Simulating with herd7
---------------------

    % herd7 -variant vmsa,fatal,ets2 -kinds tests/VMSA-ETS2-kinds.txt tests/@all

Running with litmus7
--------------------

VMSA litmus tests need to control the Translation Tables and the
exception handler vector. For this reason, litmus7 generates binaries
that run in EL1 using hardware virtualization. `-mach kvm-aarch64`
instructs litmus7 to generate such tests. In this mode, litmus7
generates source code that dependends on
[kvm-unit-tests](http://www.linux-kvm.org/page/KVM-unit-tests). Assuming
that the user has downloaded a copy of kvm-unit-test and [build its
source](https://gitlab.com/kvm-unit-tests/kvm-unit-tests/-/blob/master/README.md?ref_type=heads#building-the-tests)
in the folder `${KUT_DIR}`:

    % mkdir ${KUT_DIR}/litmus
    % litmus7 -mach kvm-aarch64 -a 2 -o ${KUT_DIR}/litmus -kinds tests/VMSA-ETS2-kinds.txt tests/@armv8-a
    % cd ${KUT_DIR}/litmus
    % make
    % cd ..
    % sh ./litmus/run.sh

Some of the tests in this catalogue require support for FEAT_LSE. If
hardware implements Armv8.1-A which requires FEAT_LSE, you can instead
run litmus7 with the following command:

    % litmus7 -mach kvm-armv8.1 -a 2 -o ${KUT_DIR}/litmus -kinds tests/ETS2-kinds.txt tests/@all

> **_NOTE:_** litmus7 and the C compiler might emit warnings for the tests:
> - LDR,
> - LDRaf0-HA,
> - LDRaf0-noHA,
> - LDRv1,
> - MP+DSB+ctrl,
> - NT-00-data,
> - NT-01-data,
> - NT-02-data,
> - NT-03-data,
> - NT-04-data,
> - NT-20-data+dsb,
> - NT-20-data,
> - NT-23-data+dsb,
> - NT-23-data,
> - NT-30-data+dsb,
> - NT-30-data,
> - NT-33-data+dsb,
> - NT-33-data,
> - R3+W,
> - R3+W,
> - R3+W,
> - R3-DSBs+W,
> - R3-DSBs+W,
> - R3-DSBs+W,
> - STR-db0-noHD,
> - STR,
> - STRx+STCLRptex+af,
> - STRx+STCLRptex+db+dbm,
> - STRx+STCLRptex+db,
> - STRx+STCLRptex+db2+dbm,
> - STRx+STCLRptex+db2,
> - STRx+STCLRptex+db3+dbm,
> - STRx+STCLRptex+db3,
> - STRx+STCLRptex+db4,
> - coRWImpExp+WExp, and
> - coRWImpExp.
>
> These are known and do not affect the correctness of these tests.
