# Litmus Tests for BPF Memory model
Author: Puranjay Mohan <puranjay@kernel.org>
Co-developed-by: Paul E. McKenney <paulmck@kernel.org>

## Introduction

This README will explain the current status of BPF support in herd7. It explains how some of the litmus tests can be run using herd7 and BPF assembly. the test/ directory has some litmus tests ported from the linux kernel.
### BPF ISA support in herd7
In the current state, herd7 supports the following BPF instructions:

#### Data movement instructions
1. Register to Register
```
r0 = r5
```
2. Immediate to Register
```
r3 = 40
```

#### Arithemetic and Logical Instructions
The meanings of these symbols are self explainatory. They match the C style operators.
1. Register to Register
```
r0 += r1
r2 -= r3
r3 *= r5
r2 /= r5
r4 %= r6
r8 &= r7
r1 |= r6
r4 ^= r4
r4 <<= r5
r3 >>= r4
```
2. Immediate to Register
```
r0 += 5
r2 -= 33
r3 *= 44
r2 /= -2
r4 %= 3
[...]
Continues like register to register
```
#### Load / Store Instructions
The current support of BPF in herd7 doesn't include mixed sized operations, so all these Loads and Stores are fixed at 32-bits. You may use any size in the litmus tests, but the operations are implemented as 32-bit loads and stores.
1. Load Register [rd = *(u64 *) (rs + offset16)]
```
r0 = *(u8 *)(r0 + 10)
r2 = *(u16 *)(r4 - 3)
r4 = *(u32 *)(r5 + 64)
r5 = *(u64 *)(r6 - 6)
```
2. Store Register [*(u64 *) (rs + offset16) = rd]
```
*(u8 *)(r0 + 32)  =  r0
*(u16 *)(r4 + 64) =  r2
*(u32 *)(r5 + 86) =  r4
*(u64 *)(r6 - 34) =  r5

```
3. Store Immediate [*(u64 *) (rs + offset16) = imm64]
```
*(u8 *)(r0 + 0)  = 23
*(u16 *)(r4 + 0) = -32
*(u32 *)(r5 + 0) = 44
*(u64 *)(r6 + 0) = 55
```
#### Atomic Instructions

1. 32 and 64 bit atomic operations:
These instructions do an ALU op to a memory location (register + offset) atomically. It can work with 32-bit and 64-bit data.
```
lock *(u32 *)(rd + offset16) += rs
lock *(u32 *)(rd + offset16) &= rs
lock *(u32 *)(rd + offset16) |= rs
lock *(u32 *)(rd + offset16) ^= rs

lock *(u64 *)(rd + offset16) += rs
lock *(u64 *)(rd + offset16) &= rs
lock *(u64 *)(rd + offset16) |= rs
lock *(u64 *)(rd + offset16) ^= rs
```

2. 32 and 64 bit atomic operations with fetch:
These instructions fetch the value from memory and then do an ALU op on the memory location (register + offset) atomically. It can work with 32-bit and 64-bit data.
```
rs = atomic_fetch_add ((u32 *)(rd + offset16), rs)
rs = atomic_fetch_and ((u32 *)(rd + offset16), rs)
rs = atomic_fetch_or ((u32 *)(rd + offset16), rs)
rs = atomic_fetch_xor ((u32 *)(rd + offset16), rs)

rs = atomic_fetch_add ((u64 *)(rd + offset16), rs)
rs = atomic_fetch_and ((u64 *)(rd + offset16), rs)
rs = atomic_fetch_or ((u64 *)(rd + offset16), rs)
rs = atomic_fetch_xor ((u64 *)(rd + offset16), rs)
```

3. Atomic exchange instruction 32 and 64 bit
This instruction atomically exchanges the value in a register rs with the value addressed by rd + offset
```
rs = xchg_64 (rd + offset16, rs)
rs = xchg_32_32 (rd + offset16, rs)
```

4. Atomic compare and exchange instruction 32 and 64 bit
This instruction compares value in r0 to value addressed by rd + offset16. On match, the value addressed by rd + offset16 is replaced with the value in rs. Regardless, the value that was at rd + offset16 is zero-extended and loaded into r0.

```
r0 = cmpxchg_64 (rd + offset16, r0, rs)
r0 = cmpxchg32_32 (rd + offset16, r0, rs)
```

####  Barrier / Fence instruction
BPF doesn't have a real sync instruction. BPF atomic instructions with fetch provide full ordering as enforced by `bpf.cat`. So, we a atomic op with fetch to enforce ordering in the programs.
```
r6 = atomic_fetch_add((u64*)(r5 + 0), r6)
```
####  Jump instructions
Jump instructions work with labels in herd, offsets can't be used directly.

1. Unconditonal jump
This instruction causes a jump to the label unconditionaly.
format: goto lbl
```
r0 = 10
goto EXIT
r0 = 5
EXIT:
```

2. Conditional jump
This instruction causes a jump to the label when a condition is met.
Format `if rd cond rs goto label` or `if rd cond imm32 goto label`
here `cond` can be one of -> `==, !=, >=, <`

```
r5 = 0
if r6 >= 10 goto EXIT
r5 = 10
EXIT:
```

BPF doesn't have a real sync instruction. BPF atomic instructions with fetch provide full ordering as enforced by `bpf.cat`. So, we a atomic op with fetch to enforce ordering in the programs.
### Provided cat models
The default model for BPF is one based on LKMM, so if you run a litmus test like:
```
/usr/local/bin/herd7 SB+poonceonces.litmus
```

This will use the `bpf.cat` model and all litmus tests pass with it.

```
## Running Litmus Tests

#### Compiling and installing herd
From the root directory of herdtools7 run:
```
make PREFIX=/usr/local
sudo make PREFIX=/usr/local install
```

### Launch herd7 and run tests
The tests directory with this README.md has some litmus tests, let's run them:

#### CoRR+poonceonce+Once.litmus [Verifies R -> R from same address should be ordered ]
```
/usr/local/bin/herd7 CoRR+poonceonce+Once.litmus
Test CoRR+poonceonce+Once Allowed
States 3
1:r1=0; 1:r2=0;
1:r1=0; 1:r2=1;
1:r1=1; 1:r2=1;
No
Witnesses
Positive: 0 Negative: 3
Condition exists (1:r1=1 /\ 1:r2=0)
Observation CoRR+poonceonce+Once Never 0 3
Time CoRR+poonceonce+Once 0.00
Hash=1046c07e495c24483978cd17c36e4d31
```
As we see, now this says `NEVER` which is correct according to LKMM.

#### IRIW+poonceonces+OnceOnce [Allowed by LKMM]
```
/usr/local/bin/herd7 IRIW+poonceonces+OnceOnce.litmus
Test IRIW+poonceonces+OnceOnce Allowed
States 16
1:r2=0; 1:r3=0; 3:r2=0; 3:r3=0;
1:r2=0; 1:r3=0; 3:r2=0; 3:r3=1;
1:r2=0; 1:r3=0; 3:r2=1; 3:r3=0;
1:r2=0; 1:r3=0; 3:r2=1; 3:r3=1;
1:r2=0; 1:r3=1; 3:r2=0; 3:r3=0;
1:r2=0; 1:r3=1; 3:r2=0; 3:r3=1;
1:r2=0; 1:r3=1; 3:r2=1; 3:r3=0;
1:r2=0; 1:r3=1; 3:r2=1; 3:r3=1;
1:r2=1; 1:r3=0; 3:r2=0; 3:r3=0;
1:r2=1; 1:r3=0; 3:r2=0; 3:r3=1;
1:r2=1; 1:r3=0; 3:r2=1; 3:r3=0;
1:r2=1; 1:r3=0; 3:r2=1; 3:r3=1;
1:r2=1; 1:r3=1; 3:r2=0; 3:r3=0;
1:r2=1; 1:r3=1; 3:r2=0; 3:r3=1;
1:r2=1; 1:r3=1; 3:r2=1; 3:r3=0;
1:r2=1; 1:r3=1; 3:r2=1; 3:r3=1;
Ok
Witnesses
Positive: 1 Negative: 15
Condition exists (1:r2=1 /\ 1:r3=0 /\ 3:r2=1 /\ 3:r3=0)
Observation IRIW+poonceonces+OnceOnce Sometimes 1 15
Time IRIW+poonceonces+OnceOnce 0.01
Hash=dc3f0f1510ace4b9a3cc9d1128afbe8c
```
Here we see `Sometimes`, this is allowed by LKMM.

#### IRIW+fencembonceonces+OnceOnce [Not Allowed]
Now, fences/barriers are in place, so it should say `Never`
```
/usr/local/bin/herd7 IRIW+fencembonceonces+OnceOnce.litmus
Test IRIW+fencembonceonces+OnceOnce Allowed
States 15
1:r2=0; 1:r3=0; 3:r2=0; 3:r3=0;
1:r2=0; 1:r3=0; 3:r2=0; 3:r3=1;
1:r2=0; 1:r3=0; 3:r2=1; 3:r3=0;
1:r2=0; 1:r3=0; 3:r2=1; 3:r3=1;
1:r2=0; 1:r3=1; 3:r2=0; 3:r3=0;
1:r2=0; 1:r3=1; 3:r2=0; 3:r3=1;
1:r2=0; 1:r3=1; 3:r2=1; 3:r3=0;
1:r2=0; 1:r3=1; 3:r2=1; 3:r3=1;
1:r2=1; 1:r3=0; 3:r2=0; 3:r3=0;
1:r2=1; 1:r3=0; 3:r2=0; 3:r3=1;
1:r2=1; 1:r3=0; 3:r2=1; 3:r3=1;
1:r2=1; 1:r3=1; 3:r2=0; 3:r3=0;
1:r2=1; 1:r3=1; 3:r2=0; 3:r3=1;
1:r2=1; 1:r3=1; 3:r2=1; 3:r3=0;
1:r2=1; 1:r3=1; 3:r2=1; 3:r3=1;
No
Witnesses
Positive: 0 Negative: 15
Condition exists (1:r2=1 /\ 1:r3=0 /\ 3:r2=1 /\ 3:r3=0)
Observation IRIW+fencembonceonces+OnceOnce Never 0 15
Time IRIW+fencembonceonces+OnceOnce 0.01
Hash=cb55ae2e831237cb295221a97bf9a6b0
```


This says `Never` which is the correct outcome.

## Running all the tests

All the tests can be run together and compared with the expected outcomes using the following commands:

From the root directory of the project run:

```
make cata-bpf-test

_build/default/internal/herd_catalogue_regression_test.exe \
        -j 32 \
        -herd-timeout 16.0 \
        -herd-path _build/install/default/bin/herd7 \
        -libdir-path ./herd/libdir \
        -kinds-path catalogue/bpf/tests/kinds.txt \
        -shelf-path catalogue/bpf/shelf.py \
        test
herd7 catalogue bpf tests: OK
```
