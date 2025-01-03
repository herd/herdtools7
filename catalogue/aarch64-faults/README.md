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

Support for fault atoms in herd7 and litmus7
--------------------------------------------

The syntax for the fault atom is `fault(<Pn>:<label>, <location>, <fault_type>)`.
The components of the syntax are defined as follows:

| Component      | Mandatory | Description                                                                             |
|----------------|-----------|-----------------------------------------------------------------------------------------|
| `<Pn>`         | Yes       | The processor or thread identifier. Represents the context in which the fault occurred. |
| `<label>`      | No        | A label associated with the fault, indicating where the fault has occurred.             |
| `<location>`   | No        | The memory address or resource where the fault occurred.                                |
| `<fault_type>` | No        | The type of fault encountered as defined below.                                         |

Currently, herdtools7 supports the following types of faults:

    - `MMU:Translation` fault, illustrated by 3faults.litmus test.
    - `MMU:AccessFlag` fault, illustrated by 3faults.litmus and LDRaf0F.litmus tests.
    - `MMU:Permission` fault, illustrated by STRdb0F.litmus.
    - `TagCheck` fault, illustrated by LDRredF.litmus test. Not applicable to litmus7.
    - `UndefinedInstruction`, illustrated by UDF.litmus and noUDF.litmus tests.
    - `SupervisorCall`, illustrated by SVC.litmus test.

The litmus tests above follow the convention that the `fault()` predicate
inquires about the instruction that caused the fault rather than the instruction
to which the program returns after exiting the fault handler. This difference
is only relevant for the SVC instruction, where the return address is the next
instruction after SVC.
