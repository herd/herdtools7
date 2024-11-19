A previous [note](ordering.md) I defined the dependency relation in of the
Pointer Authentication Code instructions, but this description is probably
incomplete because it explains how to encode the authentication in the exeisting
dependencies, but it doesn't explain some new dependencies we might expect from
the authentication instruction.

## New rules

In the current AArch64 memory model, the locally ordered before relation is a
sub-relation of the two next relations (in `aarch64hwreqs.cat`):

```
(* Dependency-ordered-before *)
let dob =
 | addr; [Exp & M]; po; [Exp & W | HU]
 | ...

 (* Pick-ordered-before *)
let pob =
 | pick-addr-dep; [Exp & M]; po; [Exp & W | HU]
 | ...

(* Locally-ordered-before *)
let lob =
 | lob; lob
 | dob
 | pob
 | ...
```

In terms of hardware implementation, those relations means that previous
addresses translations must finish before commiting a store and making it
visible by the other cores.

But in presence of `FEAT_FPAC`, authentication may also fail, so one may expect
to have a similar rule in the cat files to say:

```
let FPAC_AUTH = if "fpac" then AUTH else empty_set
pick-basic-dep; [FPAC_AUTH]; po; [Exp & W | HU]
basic-dep; [FPAC_AUTH]; po; [Exp & W | HU]
```

Here `BAUTH` represent a branching authentication event in presence of `FPAC`,
it's an authentication that might fault. Such that every store must wait for the
previous authentication to succede before being visible by the other coherent
observers.

## A few hardware tests

To test such reordering, we can try to use a litmus test like this one:

```
AArch64 example
Variant=pac,fpac
{
    0:x0=x; 0:x1=y; 0:x2=1;
    1:x0=x; 1:x1=y; 1:x2=2; 1:x5=z;
}
P0          | P1               ;
str x2,[x0] | ldr x3,[x1]      ;
dmb sy      | eor x4,x3,x3     ;
str x2,[x1] | pacdza x5        ;
            | autda x5,x4      ;
            | str x2,[x0]      ;
exists (1:x3=1 /\ [x]=1)
(* Forbidden if `FEAT_FPAC` is implemented and `SCTLR_EL1.EnDA = 1` *)
(* Allowed otherwise *)
```

In this example we have a `pick-basic-dep` from the first `ldr` to the
`FPAC_AUTH` event, and no `basic-dep` because `autda x5,x4` only have a
`pick-basic-dep` between its register-read event in `x4` and its `FPAC_AUTH`
event. And this `pick-basic-dep` must be enough to force the first `ldr` to be
observed before the last `str` instruction. We can also use those tests to
observe the same ordering:

```
AArch64 example
Variant=pac,fpac
{
    0:x0=x; 0:x1=y; 0:x2=1;
    1:x0=x; 1:x1=y; 1:x2=2; 1:x5=z;
}
P0          | P1               ;
str x2,[x0] | ldr x3,[x1]      ;
dmb sy      | cmp x3,x3        ;
str x2,[x1] | csel x4,x4,x4,eq ;
            | pacdza x5        ;
            | autda x5,x4      ;
            | str x2,[x0]      ;
exists (1:x3=1 /\ [x]=1)
(* Forbidden if `FEAT_FPAC` is implemented and `SCTLR_EL1.EnDA = 1` *)
(* Allowed otherwise *)
```

```
AArch64 example
Variant=pac,fpac
{
    0:x0=x; 0:x1=y; 0:x2=1;
    1:x0=x; 1:x1=y; 1:x2=2; 1:x5=z;
}
P0          | P1               ;
str x2,[x0] | ldr x3,[x1]      ;
dmb sy      | cmp x3,x3        ;
str x2,[x1] | csel x5,x5,x5,eq ;
            | pacdza x5        ;
            | autdza x5        ;
            | str x2,[x0]      ;
exists (1:x3=1 /\ [x]=1)
(* Forbidden if `FEAT_FPAC` is implemented and `SCTLR_EL1.EnDA = 1` *)
(* Allowed otherwise *)
```
