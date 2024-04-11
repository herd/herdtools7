aarch64-faults: Catalogue with test that demonstrate Faults
============================================================

This catalogue contains artifacts from the work on the semantics of
faults of AArch64 memory model. The tests in this catalogue execute
instructions that generate different type of faults.

Simulating with herd7
---------------------

    % herd7 -kinds tests/kinds.txt tests/@all

Running with litmus7
--------------------

VMSA litmus tests need to control the Translation Tables and the
exception handler vector. For this reason, litmus7 generates binaries
that run in EL1 using hardware virtualization. `-mach kvm-aarch64`
instructs litmus7 to generate such tests. In this mode, litmus7
generates source code that dependends on
[kvm-unit-tests](http://www.linux-kvm.org/page/KVM-unit-tests). Assuming
that the user has downloaded a copy of kvm-unit-test and [build its
source]{https://gitlab.com/kvm-unit-tests/kvm-unit-tests/-/blob/master/README.md?ref_type=heads#building-the-tests)
in the folder `${KUT_DIR}`:

    % mkdir ${KUT_DIR}/litmus
    % litmus7 -mach kvm-aarch64 -a 2 -kinds tests/kinds.txt -o ${KUT_DIR}/litmus tests/{3faults,LDRaf0F,STRdb0F,UDF,noUDF}.litmus
    % cd ${KUT_DIR}/litmus
    % make
    % cd ..
    % sh ./litmus/run.sh

> **_NOTE:_** litmus7 does not support MTE and, consequently, cannot run the test LDRredF.
