The goal of this note is to show a demonstration of how to use the predicate
solver to solve hash collision in presence of pointer authentication codes. The
previous notes are focussed around the internal structure of the solver and the
memory model, but this not is focussed on the user interface, and how to use it
to understand the behaviour of the hash collisions in a concurrent
program.

# PAC introduction

Pointer Authentication Code or PAC is a set of extensions of AArch64 to add
cryptographic signatures in the most significant bits of a virtual address. It
introduces a new set of instructions to
- Add a cryptographic hash in a pointer
- Remove a cryptographic hash from a pointer
- Authenticate a pointer and remove its hash only in case of success

As example, this assembly program add a hash in the pointer `x` before doing a
successful authentication and loading a value from the pointer:

```
AArch64 Basic PAC example
Variant=pac
{ 0:x0=x; x1=42; }
P0              ;
    pacda x0,x1 ; (* write pacda(x,42) in x0 *)
    autda x0,x1 ; (* write x in x0 *)
    ldr x2,[x0] ;
```

And this program does the same thing without the authentication:

```
AArch64 Basic PAC example
Variant=pac
{ 0:x0=x; x1=42; }
P0              ;
    pacda x0,x1 ; (* write pacda(x,42) in x0 *)
    xpacd x0    ; (* write x in x0 *)
    ldr x2,[x0] ;
```

But a memory operation from a pointer with a pointer containing a cryptographic
hash (named non-canonical) must fail at address translation:

```
AArch64 Basic PAC example
Variant=pac
{ 0:x0=x; x1=42; }
P0              ;
    pacda x0,x1 ; (* write pacda(x,42) in x0 *)
    ldr x2,[x0] ; (* try to load from the address pacda(x,42) *)
exists Fault(P0,MMU:Translation)
```

And depending on the enabled features on the processor, a failing authentication
may either return a new non-canonical pointer, or fault. As example with
`FEAT_PAuth2`:

```
AArch64 Basic PAC example
Variant=pac
{ 0:x0=x; x1=42; }
P0              ;
    pacda x0,x1 ; (* write pacda(x,42) in x0 *)
    autdb x0,x1 ; (* write pacda(pacdb(x,42),42) in x0 *)
    ldr x2,[x0] ; (* try to load from a non-canonical pointer *)
exists Fault(P0,MMU:Translation)
```

Here `pacda(pacdb(...),...)` represent the combination of the two hashes using an
exclusive `OR`, if the hashes are not equals then their `EOR` must be different to
zero, resulting in a non-canonical pointer.


And with `FEAT_PAuth2` and `FEAT_FPAC`:

```
AArch64 Basic PAC example
Variant=pac,fpac
{ 0:x0=x; x1=42; }
P0              ;
    pacda x0,x1 ; (* write pacda(x,42) in x0 *)
    autdb x0,x1 ; (* Fault *)
    ldr x2,[x0] ;
exists Fault(P0,PacCheck:DB)
```

# Testing the memory model of `PAC` instructions

To test the memory model of `PAC` instructions in herd, we will use some
parallel assembly programs like this one:

```
AArch64 Demo
Variant=pac
{
    0:x0=x; 0:x1=y; 0:x2=1;
    1:x0=x; 1:x1=y; 1:x2=1;
}
P0          | P1          ;
str x2,[x0] | ldr x2,[x1] ;
dmb sy      | TO COMPLETE ;
str x2,[x1] | ldr x3,[x0] ;
exists (1:x2=1 /\ 1:x3=0)
```

In this example:
- `P0` write to `x` then `y`, in order because of the presence of the `dmb sy`
    instruction
- `P1` read from `y` and `x`, by default these reads can be in any order, but
    it's possible to add some code in the `TO COMPLETE` part to force the
    ordering of the loads.

A very classical way to ensure this ordering in `P1` is to use the
`eor x4,x2,x2; add x0,x0,x4` sequence because this adds an address dependency
between the two load events, and this is the result expected by herd:

```
$ cat demo.litmus
AArch64 Demo
Variant=pac
{
    0:x0=x; 0:x1=y; 0:x2=1;
    1:x0=x; 1:x1=y; 1:x2=1;
}
P0          | P1           ;
str x2,[x0] | ldr x2,[x1]  ;
dmb sy      | eor x4,x2,x2 ; (* write 0 in x4 *)
str x2,[x1] | add x0,x0,x4 ; (* write x in x0 *)
            | ldr x3,[x0]  ;
exists (1:x2=1 /\ 1:x3=0)

$ herd7 demo.litmus
Test Demo Allowed
States 3
1:X2=0; 1:X3=0;
1:X2=0; 1:X3=1;
1:X2=1; 1:X3=1;
No
...
```

This address dependency means that the processor need the result of the first
load to compute the address of the second, and it is not allowed to predict this
address.

And this is also the case if we add a `xpacd x0` instruction or
`pacda x0,x0; xpacd x0` in this sequence because it also generates an
address dependency between it's register-read/write events:

```
$ cat demo.litmus
AArch64 Demo
Variant=pac
{
    0:x0=x; 0:x1=y; 0:x2=1;
    1:x0=x; 1:x1=y; 1:x2=1;
}
P0          | P1           ;
str x2,[x0] | ldr x2,[x1]  ;
dmb sy      | eor x4,x2,x2 ; (* write 0 into x4 *)
str x2,[x1] | pacda x0,x4  ; (* write pacda(x,0) in x0 *)
            | xpacd x0     ; (* write x in x0 *)
            | ldr x3,[x0]  ;
exists (1:x2=1 /\ 1:x3=0)

$ herd7 demo.litmus
Test Demo Allowed
States 3
1:X2=0; 1:X3=0;
1:X2=0; 1:X3=1;
1:X2=1; 1:X3=1;
No
...
```

But with a successfull `aut*` instruction, the result may be different because
`autda Xd,Xn` only create a pick address dependency between it's read event in
`Xn` and it's register-write event in `Xd` when it succede, and this kind of
dependency is not enough to force the ordering of two loads:

```
$ cat demo.litmus
AArch64 Demo
Variant=pac
{
    0:x0=x; 0:x1=y; 0:x2=1;
    1:x0=x; 1:x1=y; 1:x2=1;
}
P0          | P1           ;
str x2,[x0] | ldr x2,[x1]  ;
dmb sy      | eor x4,x2,x2 ; (* write 0 into x4 *)
str x2,[x1] | pacdza x0    ; (* write pacda(x,0) in x0 *)
            | autda x0,x4  ; (* write x in x0 *)
            | ldr x3,[x0]  ;
exists (1:x2=1 /\ 1:x3=0)

$ herd7 demo.litmus
Test Demo Allowed
States 4
1:X2=0; 1:X3=0;
1:X2=0; 1:X3=1;
1:X2=1; 1:X3=0;
1:X2=1; 1:X3=1;
Ok
...
```

This pick address dependency means that the processor still need the result of
the first load to compute the address of the second, but now it may predict this
address by predicting if the authentication will succede.

# A first unexpected result

In theory with `FEAT_PAuth2` and without `FEAT_FPAC`, during the execution of a
`autda Xd,Xn` that ***fail***, we also expect to see an address dependency between the
regeister-read event in `Xn` and the register-write event in `Xd`. And this kind
of dependency is enough to force the ordering of two loads, but if we simulate
this program with herd:

```
$ cat demo.litmus
AArch64 Demo
Variant=pac
{
    0:x0=x; 0:x1=y; 0:x2=1;
    1:x0=x; 1:x1=y; 1:x2=1;
}
P0          | P1           ;
str x2,[x0] | ldr x2,[x1]  ;
dmb sy      | eor x4,x2,x2 ; (* write 0 into x4 *)
str x2,[x1] | pacdzb x0    ; (* write pacdb(x,0) in x0 *)
            | autda x0,x4  ; (* write pacda(pacdb(x,0),x0) in x0 *)
            | xpacd x0     ; (* write x in x0 *)
            | ldr x3,[x0]  ;
exists (1:x2=1 /\ 1:x3=0)

$ herd7 demo.litmus
Test Demo Allowed
States 4
1:X2=0; 1:X3=0;
1:X2=0; 1:X3=1;
1:X2=1; 1:X3=0;
1:X2=1; 1:X3=1;
Ok
...
```

At least in theory we expect the result `1:x2=1 /\ 1:x3=0` to be impossible
because the authentication failure must add a strong enough dependency to force
the ordering of the loads. But in reality the authentication *may succede*!!!
PAC add cryptographic hashes in the most significant bits of the virtual
addresses, but those hash are small because they must fit inside the unused bits
of the pointers. So we expect to see some hash collisions with a very small
probability, and this is exactly what herd did, using a constraint solver to
enumerate all the possible collisions in this program. And we can use
this solver to debug this litmus test:

```
$ cat demo.litmus
AArch64 Demo
Variant=pac
{
    0:x0=x; 0:x1=y; 0:x2=1;
    1:x0=x; 1:x1=y; 1:x2=1;
}
P0          | P1           ;
str x2,[x0] | ldr x2,[x1]  ;
dmb sy      | eor x4,x2,x2 ; (* write 0 into x4 *)
str x2,[x1] | pacdzb x0    ; (* write pacdb(x,0) in x0 *)
            | autda x0,x4  ; (* write pacda(pacdb(x,0),x0) in x0 *)
            | xpacd x0     ; (* write x in x0 *)
            | ldr x3,[x0]  ;
exists (1:x2=1 /\ 1:x3=0)

$ herd7 demo.litmus -debug pac,pred-solver
Test Demo Allowed
States 7
1:X2=0; 1:X3=0;
1:X2=0; 1:X3=0; pacda(x, 0x0)=pacdb(x, 0x0);
1:X2=0; 1:X3=1;
1:X2=0; 1:X3=1; pacda(x, 0x0)=pacdb(x, 0x0);
1:X2=1; 1:X3=0; pacda(x, 0x0)=pacdb(x, 0x0);
1:X2=1; 1:X3=1;
1:X2=1; 1:X3=1; pacda(x, 0x0)=pacdb(x, 0x0);
Ok
...
```

In this example we can observe that the only possible execution that give the
unexpected result require a hash collision between `pacda(x,0)` and
`pacdb(x,0)`, and we can update the test to ensure that we only look at the
execution without this collision:

```
$ cat demo.litmus
AArch64 Demo
Variant=pac
{
    0:x0=x; 0:x1=y; 0:x2=1;
    1:x0=x; 1:x1=y; 1:x2=1;
}
P0          | P1           | P2          ;
str x2,[x0] | ldr x2,[x1]  | pacdza x0   ; (* write pacda(x,0) in x0 *)
dmb sy      | eor x4,x2,x2 | autdzb x0   ; (* write pacda(pacdb(x,0),x0) in x0 *)
str x2,[x1] | pacdzb x0    | ldr x0,[x0] ; (* succede iff pacda(x,0) == pacdb(x,0) *)
            | autda x0,x4  |             ;
            | xpacd x0     |             ;
            | ldr x3,[x0]  |             ;
exists (1:x2=1 /\ 1:x3=0 /\ Fault(P2))

$ herd7 demo.litmus -debug pac,pred-solver
Test Demo Allowed
States 7
1:X2=0; 1:X3=0;  ~Fault(P2); pacda(x, 0x0)=pacdb(x, 0x0);
1:X2=0; 1:X3=0; Fault(P2,pacda(pacdb(x, 0x0), 0x0),MMU:Translation);
1:X2=0; 1:X3=1;  ~Fault(P2); pacda(x, 0x0)=pacdb(x, 0x0);
1:X2=0; 1:X3=1; Fault(P2,pacda(pacdb(x, 0x0), 0x0),MMU:Translation);
1:X2=1; 1:X3=0;  ~Fault(P2); pacda(x, 0x0)=pacdb(x, 0x0);
1:X2=1; 1:X3=1;  ~Fault(P2); pacda(x, 0x0)=pacdb(x, 0x0);
1:X2=1; 1:X3=1; Fault(P2,pacda(pacdb(x, 0x0), 0x0),MMU:Translation);
No
...
```

Now we can observe using this test that if the authentication fail, then the
dependencies are strong enough to force the ordering of the two loads, because
if the anthentication pass, then `P2` can't raise a fault.
