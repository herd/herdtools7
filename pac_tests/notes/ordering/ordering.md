# Pointer Authentication Code

This note focus on Pointer Authentication Code with the different extensions:
- `FEAT_Pauth`: Basic pointer authentication extension
- `FEAT_Pauth2`: Change the way we add an authentication to a pointer
- `FEAT_FPAC`: Allow to generate a fault dirrectly at the autheutiation in case
    of a fail, instead of faulting at memory translation.

## Instructions

Pointer authentication add some new instructions, some basic instructions, and
some combined instruction to do the `pac/aut` instruction and the associated
memory operation (load, branch, store...) in one cycle, the basic instructions
are:
- `pac* x,y` with `*`the correponding authentication key (`da`, `db`,
    `ia` or `ib`), and there is some special instruction for diffent specific
    values of `y`, like `pacdza x` for `y=XZR`. This instruction add a pac field
    (a cryptographic signature of `(x, y)` using the given key) in the most
    significant bits of the virtual address `x`. We call `y` the modifier.
- `xpacd x` or `xpaci x`: remove the pac field of the virtual address `x`, in
    abscence of address tagging the two instruction have the same semantic.
- `aut* x, y`: authenticate the pac field of a virtual address using a given
    key, a virtual address `x` and a modifier `y`. If the anthentication is a
    success then this instruction return the initial value of `x` (it removes
    it's pac field), otherwise a fault is generated.

To understand the sequencial behaviour of these instructions we can write the
following simplified table (in abscence of hash collisions):

| x0:         | x                  | pac(x,da,0)        | pac(x,ia,0)        |
|:------------|:-------------------|:-------------------|:-------------------|
| ldr x1,[x0] | `x1 <- mem[x]`     | Fault(Translation) | Fault(Translation) |
| pacdza x0   | pad(x,da,0)        | non-deterministic  | non-deterministic  |
| autdza x0   | Fault(PacCheck:DA) | x                  | Fault(PacCheck:DA) |
| xpacd x0    | x                  | x                  | x                  |

Here, the non-deterministic means that `pacda` apply to a non-canonical virtual
address (most significant bits to `0...0` or `1...1`) can overwrite the bit 55
by the bit 63, and "forget" the virtual address range of the original pointer:
the `aut*` and `xpac*` use the bit 55 to deduce the virtual address range of the
address but the `pac*` instruction use the bit 63 (but the bit 55 if
`FEAT_CONSTPACFIELD` is implemented). In particular this means that this program

```
AArch64 switch VA range
Variant=pauth2
{ 0:x0=x }
P0        ;
pacdza x0 ;
pacdza x0 ;
xpacd x0  ;
```

can write `x` in `x0` with probability `0.5`, or `0b1...10...0 ^ x` with
probability `0.5` (the number of `1` is the number of bits in the pac field).
For this reason my implementation in herd7 will raise a user error if we try to
add two pac field in a pointer using the `pac*` instruction.

## Hash collisions

The cryptographic signatures in PAC are small, because they must be in the most
significant bits of a virtual address. As example according to the tests I did
in `KVM-unit-tests`, they are `15` bits (the PAC field is `16` bits long but the
bit `55` is reserved to know the virtual address range of the signed pointer),
so the probability that two random pac fields are equals is `p=1 / 32768` but
this number depend on the size of the virutal address and may be lower or higher
in another context. In particular this means that this litmus test:

```
AArch64 Collisions in loads
Variant=pauth2
{ 0:x0=x }
P0              ;
  pacdza x0     ;
  ldr x1, [x0]  ;
exists ( ~Fault(P0, MMU:Translation) )
```

will succeed with a probability `p`, and this test too:

```
AArch64 Collisions in aut*
Variant=pauth2,fpac
{ 0:x0=x }
P0          ;
  pacdza x0 ;
  autdzb x0 ;
exists ( ~Fault(P0, PacCheck:DB) )
```

This means that each time we see an equality test that compare two pac fields,
if this test may fail, we must split the states in two because even if the pac
fields use different key or modifiers, they may be equal because of a hash
collision. But `pac(x, da, 0)` will be always different to `pac(y, da, 0)` in
abscence of aliasing because the less significant bits of `pac(x, da, 0)` are
equals to the less significant bits of `x` (and the bit 55), and these bits are
enough to see the difference between `x` and `y` (because all the other bits
replicate their bit 55).

The difficulty here is that the set of collisions we found must be coherent, as
example:

```
AArch64 Incoherent collisions 1
Variant=pauth2,fpac
{
  0:x0=x;
  1:x0=x
}
P0        | P1        ;
pacdza x0 | pacdza x0 ;
autdzb x0 | autdzb x0 ;
exists ( Fault(P0, PacCheck:DB) /\ ~Fault(P1) )
```

and

```
AArch64 Incoherent collisions 2
Variant=pauth2,fpac
{
  0:x0=x;
  1:x0=x
}
P0            | P1        ;
  pacdzb x0   | autdzb x0 ;
  ldr x1,[x0] |           ;
exists ( ~Fault(P0) /\ Fault(P1) )
```

and

```
AArch64 Incoherent collisions 3
Variant=pauth2,fpac
{
  0:x0=x;
  0:x1=x
}
P0            ;
  pacdza x0   ;
  pacdzb x1   ;
  cmp x0, x1  ;
  b.ne finish ;
  autdzb x0   ;
finish:       ;
exists ( Fault(P0, PacCheck:DB) )
```

will never succeed because at two different points they require that a same
collision is observed and not observed in the time.

## Memory ordering

Pointer signature and authentication may add new ordering dependencies in the
memory model, in particular data dependency because the output of `pac*`, `aut*`
and `xpac*` depend on both of their inputs. But also some control dependencies
because `aut*` may fail, and any memory operation may fail if the virtual address
they use as input is not canonical.

### `xpac*` instruction:

The `xpacd Xd` and `xpaci Xd` instrucitons strip the PAC field of their inputs.
It must create a `basic-dependency` between the register-read event of the
register `Xd` and the register-write event of `Xd`. We can observe
this with this litmus test:

```
AArch64 xpacd-basic-dep
Variant=pauth2
{
  int64_t x=0;
  int64_t y=0;
  0:X0=x; 0:X1=y; int64_t 0:X2=1;
  1:X0=x; 1:X1=y; int64_t 1:X2=0; int64_t 1:X3=0;
}
P0          | P1           ;
STR X2,[X0] | PACDZA X0    ; (* Write pacda(x) in 1:X0 *)
DMB SY      | LDR X2,[X1]  ;
STR X2,[X1] | EOR X3,X2,X2 ; (* Write 0 in 1:X3 *)
            | ADD X0,X0,X3 ; (* Write pacda(x) in 1:X0 *)
            | XPACD X0     ; (* Write x in 1:X0 *)
            | LDR X4,[X0]  ;
exists (1:X2=1 /\ 1:X4=0)
```

and here is the graph expected for the execution of the `xpacd x0` instruction:

![dependency graph of the `xpacd` instruction](xpacd.png)

### `pac*` instruction:

The `pac*` instruction must add two register read data dependencies, in
particular it will read two registers and use their values to write the output
value `pacda(x, da, 0x0)` in the destination register (`x0` in this example).

So here is an example of litmus test with the pacda instruction:

The `pac*` instruction semantic depend on the status of the register
`SCTLR_EL1.En*` represented in the litmus test by the presence of the variant
`no-key-*`:

- If `no-key-da` is set, then `pacda` ececute like a `mov Xd,Xd`, so there is a
    `basic-dep` between the register-read event of `Xd` and the register-write
    event of `Xd`.
- Otherwise there is a `basic dependency` between the register-read
    event of `Xd` and the register-write event of
    `Xd`. And a `basic-dependency` between the regisger-read event of the
    register `Xn` and the register-write event of `Xd`.


```
AArch64 pac: RXd ---basic-dep--> WXd
Variant=pauth2
{
  int64_t x=0;
  int64_t y=0;
  0:X0=x; 0:X1=y; int64_t 0:X2=1;
  1:X0=x; 1:X1=y; int64_t 1:X2=0; int64_t 1:X3=0;
}
P0          | P1           ;
STR X2,[X0] | LDR X2,[X1]  ;
DMB SY      | ADD X0,X0,X2 ;
STR X2,[X1] | PACDZA X0    ;
            | XPACD X0
            | LDR X3,[X0]  ;
exists ( 1:X2=1 /\ 1:X3=0 )
(* Forbidden because of the `addr` dependency *)
```

```
AArch64 pac: RXn ---basic-dep--> WXd
Variant=pauth2
{
  int64_t x=0;
  int64_t y=0;
  0:X0=x; 0:X1=y; int64_t 0:X2=1;
  1:X0=x; 1:X1=y; int64_t 1:X2=0; int64_t 1:X3=0;
}
P0          | P1           ;
STR X2,[X0] | LDR X2,[X1]  ;
DMB SY      | PACDA X0,X2  ;
STR X2,[X1] | XPACD X0     ;
            | LDR X3,[X0]  ;
exists ( 1:X2=1 /\ 1:X3=0 )
(* Forbidden if `SCTLR_EL1.EnDA = 1` because of the `addr` dependency *)
(* Allowed otherwise *)
```

And here is the expected event graph for an execution of `pacda x0,x1`:

![Expected event graph for `pacda`](pacda.png)

### `aut*` instruction with `FEAT_PAuth` but without `FEAT_PAuth2`

If the key associated with an authentication instruction is disabled, as example
if we try to execute `autda Xd,Xn` with `SCTLR_EL1.EnDA = 0`, then `autda` will
execute like a `mov Xd,Xd` instruction, so their is a `basic-dep` between the
register-read event of `Xd` and the register-write event of `Xd`.
Otherwise, if `SCTLR_EL1.EnDA = 1`, then
- If the authentication succede, then there is a `basic-dep` between the
    register-read event of `Xd` and the register-write event of the
    register `Xd`. And a `pick-basic-dep` between the register-read event of the
    register `Xn` and the register-write event of `Xd`.
- Otherwise, there is a `basic-dep` between the register-read event of the
    register `Xd` and the register-write event of `Xd`. And a
    `pick-basic-dep` between the register-read event of `Xn` and the
    register-write event of `Xd`.

One can observe those dependencies using some litmus tests. Here are two litmus
tests to observe the basic dependency between the register-read/write events in
`Xd` (one in case of an authentication susccess, and the other in case of a fail
):

```
AArch64 PAuth1 auth success: RXd ---basic-dep--> WXd
Variant=pauth1
{
    int64_t x=0;
    int64_t y=0;
    0:X0=x; 0:X1=y; int64_t 0:X2=1;
    1:X0=x; 1:X1=y; int64_t 1:X2=0; int64_t 1:X3=0;

}
P0          | P1           ;
STR X2,[X0] | PACDZA X0    ; (* Write pacda(x) in 1:X0 *)
DMB SY      | LDR X2,[X1]  ;
STR X2,[X1] | EOR X3,X2,X2 ; (* Write 0 in 1:X3 *)
            | ADD X0,X0,X3 ; (* Write pacda(x) in 1:X0 *)
            | AUTDZA X0    ; (* Write x in 1:X0 *)
            | LDR X4,[X0]  ;
exists ( 1:X2=1 /\ 1:X4=0 )
(* Always forbidden with pauth1 because there is an `addr` dependency between
 * the two loads in `P1` *)
```

```
AArch64 PAuth1 auth fail: RXd ---basic-dep--> WXd
Variant=pauth1
{
    int64_t x=0;
    int64_t y=0;
    0:X0=x; 0:X1=y; int64_t 0:X2=1;
    1:X0=x; 1:X1=y; int64_t 1:X2=0; int64_t 1:X3=0;
    2:X0=x; int64_t 2:X1=0;
}
P0          | P1           | P2          ;
STR X2,[X0] | LDR X2,[X1]  | PACDZA X0   ; (* Write pacda(x) in 2:X0 *)
DMB SY      | EOR X3,X2,X2 | LDR X1,[X0] ; (* Write 0 in 1:X3 *)
STR X2,[X1] | ADD X0,X0,X3 |             ; (* Write x in 1:X0 *)
            | AUTDZA X0    |             ; (* Write pacda(x) in 1:X0 *)
            | XPACD X0     |             ; (* Write x in 1:X0 *)
            | LDR X4,[X0]  |             ;
exists ( 1:X2=1 /\ 1:X4=0 /\ Fault(P2) )
(* Always forbidden with pauth1 because there is an `addr` dependency between
 * the two loads in `P1` *)
```

Then we can observe the presence of a `pick-basic-dep` between the register-read
event of `Xn` to the register-read event of `Xd`:

```
AArch64 PAuth1 auth success: RXn ---pick-basic-dep--> WXd
Variant=pauth1
{
  int64_t x=0;
  int64_t y=0;
  0:X0=x; 0:X1=y; int64_t 0:X2=1;
  1:X0=x; 1:X1=y; int64_t 1:X2=0; int64_t 1:X3=2; int46_t 1:X4=0;
}
P0          | P1           ;
STR X2,[X0] | PACDZA X0    ; (* Write pacda(x) in 1:X0 *)
DMB SY      | LDR X2,[X1]  ;
STR X2,[X1] | EOR X4,X2,X2 ; (* Write 0 in 1:X4 *)
            | AUTDA X0,X4  ; (* Write x in 1:X0 *)
            | STR X3,[X0]  ;
exists ( 1:X2=1 /\ [x]=1 )
(* Forbidden if `SCTLR_EL1.EnDA = 1` *)
(* Allowed otherwise *)
```

```
AArch64 PAuth1 auth fail: RXn ---pick-basic-dep--> WXd
Variant=pauth1
{
  int64_t x=0;
  int64_t y=0;
  0:X0=x; 0:X1=y; int64_t 0:X2=1;
  1:X0=x; 1:X1=y; int64_t 1:X2=0; int64_t 1:X3=2; int46_t 1:X4=0;
  2:X0=x; int64_t 2:X1=0;
}
P0          | P1           | P2          ;
STR X2,[X0] | LDR X2,[X1]  | PACDZA X0   ; (* Write pacda(x) in 2:X0 *)
DMB SY      | EOR X4,X2,X2 | LDR X1,[X0] ; (* Write 0 in 1:X4 *)
STR X2,[X1] | AUTDA X0,X4  |             ; (* Write non-canonical(x,A) in 1:X0 *)
            | XPACD X0     |             ; (* Write x in 1:X0 *)
            | STR X3,[X0]  |             ;
exists ( 1:X2=1 /\ [x]=1 /\ Fault(P2) )
(* Forbidden if `SCTLR_EL1.EnDA = 1` *)
(* Forbidden otherwise because the fault  in `P2` is unreachable *)
```

And those tests show that it's not required to have a `basic-dep` between the
register-read event of `Xn` and the register-write event of `Xd` in case of
success or failure:

```
AArch64 PAuth1 auth success: RXn ---no-basic-dep--> WXd
Variant=pauth1
{
  int64_t x=0;
  int64_t y=0;
  0:X0=x; 0:X1=y; int64_t 0:X2=1;
  1:X0=x; 1:X1=y; int64_t 1:X2=0; int64_t 1:X3=0; int46_t 1:X4=0;
}
P0          | P1           ;
STR X2,[X0] | PACDZA X0    ; (* Write pacda(x) in 1:X0 *)
DMB SY      | LDR X2,[X1]  ;
STR X2,[X1] | EOR X4,X2,X2 ; (* Write 0 in 1:X4 *)
            | AUTDA X0,X4  ; (* Write x in 1:X0 *)
            | LDR X3,[X0]  ;
exists ( 1:X2=1 /\ 1:X3=0 )
(* Always allowed *)
```

```
AArch64 PAuth1 auth fail: RXn ---no-basic-dep--> WXd
Variant=pauth1
{
    int64_t x=0;
    int64_t y=0;
    0:X0=x; 0:X1=y; int64_t 0:X2=1;
    1:X0=x; 1:X1=y; int64_t 1:X2=0; int64_t 1:X3=0;
    2:X0=x; int64_t 2:X1=0;
}
P0          | P1           | P2          ;
STR X2,[X0] | LDR X2,[X1]  | PACDZA X0   ; (* Write pacda(x) in 2:X0 *)
DMB SY      | EOR X3,X2,X2 | LDR X1,[X0] ; (* Write 0 in 1:X3 *)
STR X2,[X1] | AUTDA X0,X3  |             ; (* Write pacda(x) in 1:X0 *)
            | XPACD X0     |             ; (* Write x in 1:X0 *)
            | LDR X4,[X0]  |             ;
exists ( 1:X2=1 /\ 1:X4=0 /\ Fault(P2) )
(* Allowed if `SCTLR_EL1.EnDA = 1` because the `pick-addr` dependency between
 * the two loads is not enough to force their order *)
(* Forbidden otherwise because the MMU translation will not fail in the process
 * `P2` if `SCTLR_EL1.EnDA = 0` *)
```

Here is the expected graph of the execution of the `autda Xd,Xn` instruction in
case of a success:

![`autda Xd,Xn` instruction success](pauth1_autda_success.png)

Here is the expected graph of the execution of the `autda Xd,Xn` instruction in
case of a failure:

![`autda Xd,Xn` instruction success](pauth1_autda_failure.png)

### `aut*` instruction with `FEAT_PAuth2` but without `FEAT_FPAC`

If the key associated with an authentication instruction is disabled, as example
if we try to execute `autda Xd,Xn` with `SCTLR_EL1.EnDA = 0`, then `autda` will
execute like a `mov Xd,Xd` instruction, so there is a `basic-dep` between the
register-read event of `Xd` and the register-write event of `Xd`.
Otherwise, if `SCTLR_EL1.EnDA = 1`, then
- If the authentication succede, then there is a `basic-dep` between the
    register-read event of `Xd` and the register-write event of the
    register `Xd`. And a `pick-basic-dep` between the register-read event of the
    register `Xn` and the register-write event of `Xd`.
- Otherwise, there is a `basic-dep` between the register-read event of the
    register `Xd` and the register-write event of `Xd`. And a
    `basic-dep` between the register-read event of `Xn` and the
    register-write event of `Xd`.

One can observe those dependencies using some litmus tests. Here are two litmus
tests to observe the basic dependency between the register-read/write events in
`Xd` (one in case of an authentication susccess, and the other in case of a fail
):

```
AArch64 PAuth2 without FPAC auth success: RXd ---basic-dep--> WXd
Variant=pauth2
{
    int64_t x=0;
    int64_t y=0;
    0:X0=x; 0:X1=y; int64_t 0:X2=1;
    1:X0=x; 1:X1=y; int64_t 1:X2=0; int64_t 1:X3=0;

}
P0          | P1           ;
STR X2,[X0] | PACDZA X0    ; (* Write pacda(x) in 1:X0 *)
DMB SY      | LDR X2,[X1]  ;
STR X2,[X1] | EOR X3,X2,X2 ; (* Write 0 in 1:X3 *)
            | ADD X0,X0,X3 ; (* Write pacda(x) in 1:X0 *)
            | AUTDZA X0    ; (* Write x in 1:X0 *)
            | LDR X4,[X0]  ;
exists ( 1:X2=1 /\ 1:X4=0 )
(* Always forbidden without fpac because there is an `addr` dependency between
 * the two loads in `P1` *)
```

```
AArch64 PAuth2 without FPAC auth fail: RXd ---basic-dep--> WXd
Variant=pauth2
{
    int64_t x=0;
    int64_t y=0;
    0:X0=x; 0:X1=y; int64_t 0:X2=1;
    1:X0=x; 1:X1=y; int64_t 1:X2=0; int64_t 1:X3=0;
    2:X0=x; int64_t 2:X1=0;
}
P0          | P1           | P2          ;
STR X2,[X0] | LDR X2,[X1]  | PACDZA X0   ; (* Write pacda(x) in 2:X0 *)
DMB SY      | EOR X3,X2,X2 | LDR X1,[X0] ; (* Write 0 in 1:X3 *)
STR X2,[X1] | ADD X0,X0,X3 |             ; (* Write x in 1:X0 *)
            | AUTDZA X0    |             ; (* Write pacda(x) in 1:X0 *)
            | XPACD X0     |             ; (* Write x in 1:X0 *)
            | LDR X4,[X0]  |             ;
exists ( 1:X2=1 /\ 1:X4=0 /\ Fault(P2) )
(* Always forbidden without fpac because there is an `addr` dependency between
 * the two loads in `P1` *)
```

Then one can observe the presence of a `pick-basic` dependency between the
register-read event of `Xn` and the register-write event of `Xd` in case of
success:

```
AArch64 PAuth2 without FPAC auth success: RXn ---pick-basic-dep--> WXd
Variant=pauth2
{
  int64_t x=0;
  int64_t y=0;
  0:X0=x; 0:X1=y; int64_t 0:X2=1;
  1:X0=x; 1:X1=y; int64_t 1:X2=0; int64_t 1:X3=2; int46_t 1:X4=0;
}
P0          | P1           ;
STR X2,[X0] | PACDZA X0    ; (* Write pacda(x) in 1:X0 *)
DMB SY      | LDR X2,[X1]  ;
STR X2,[X1] | EOR X4,X2,X2 ; (* Write 0 in 1:X4 *)
            | AUTDA X0,X4  ; (* Write x in 1:X0 *)
            | STR X3,[X0]  ;
exists ( 1:X2=1 /\ [x]=1 )
(* Forbidden if `SCTLR_EL1.EnDA = 1` *)
(* Allowed otherwise *)
```

and the absence of a `basic-dep` between the register-read event of `Xn` and the
register-write event of `Xd`:

```
AArch64 PAuth2 without FPAC auth success: RXn ---no-basic-dep--> WXd
Variant=pauth2
{
  int64_t x=0;
  int64_t y=0;
  0:X0=x; 0:X1=y; int64_t 0:X2=1;
  1:X0=x; 1:X1=y; int64_t 1:X2=0; int64_t 1:X3=0; int46_t 1:X4=0;
}
P0          | P1           ;
STR X2,[X0] | PACDZA X0    ; (* Write pacda(x) in 1:X0 *)
DMB SY      | LDR X2,[X1]  ;
STR X2,[X1] | EOR X4,X2,X2 ; (* Write 0 in 1:X4 *)
            | AUTDA X0,X4  ; (* Write x in 1:X0 *)
            | LDR X3,[X0]  ;
exists ( 1:X2=1 /\ 1:X3=0 )
(* Always allowed *)
```

But the ordering difference of `PAuth2` without `FPAC` and `PAuth1` is the
presence of a `basic-dependency` between the register-read event of `Xn` and the
register-write event of `Xd` in case of failure, and we can observe it in this
test:

```
AArch64 PAuth2 without FPAC auth fail: RXn ---basic-dep--> WXd
Variant=pauth2
{
    int64_t x=0;
    int64_t y=0;
    0:X0=x; 0:X1=y; int64_t 0:X2=1;
    1:X0=x; 1:X1=y; int64_t 1:X2=0; int64_t 1:X3=0;
    2:X0=x; int64_t 2:X1=0;
}
P0          | P1           | P2          ;
STR X2,[X0] | LDR X2,[X1]  | PACDZA X0   ; (* Write pacda(x) in 2:X0 *)
DMB SY      | EOR X3,X2,X2 | LDR X1,[X0] ; (* Write 0 in 1:X3 *)
STR X2,[X1] | AUTDA X0,X3  |             ; (* Write pacda(x) in 1:X0 *)
            | XPACD X0     |             ; (* Write x in 1:X0 *)
            | LDR X4,[X0]  |             ;
exists ( 1:X2=1 /\ 1:X4=0 /\ Fault(P2) )
(* Forbidden if `SCTLR_EL1.EnDA = 1` because the `addr` dependency between
 * the two loads is enough to force their order *)
(* Forbidden otherwise because the MMU translation will not fail in the process
 * `P2` if `SCTLR_EL1.EnDA = 0` *)
```

Here is the expected graph of the execution of the `autda Xd,Xn` instruction in
case of a success:

![`autda Xd,Xn` instruction success](pauth2_without_fpac_autda_success.png)

Here is the expected graph of the execution of the `autda Xd,Xn` instruction in
case of a failure:

![`autda Xd,Xn` instruction success](pauth2_without_fpac_autda_failure.png)

### `aut*` instruction with `FEAT_FPAC`:

If the key associated with an authentication instruction is disabled, as example
if we try to execute `autda Xd,Xn` with `SCTLR_EL1.EnDA = 0`, then `autda` will
execute like a `mov Xd,Xd` instruction, so there is a `basic-dep` between the
register-read event of `Xd` and the register-write event of `Xd`.
Otherwise, if `SCTLR_EL1.EnDA = 1`, then
- If the authentication succede, then there is a `basic-dep` between the
    register-read event of `Xd` and the register-write event of the
    register `Xd`. And a `pick-basic-dep` between the register-read event of the
    register `Xn` and the register-write event of `Xd`.
- Otherwise, there a fault is generated with a `pick-basic-dep` dependency
    between the register-read of `Xd` and `Xn`, and the fault
    event.

One can observe those dependencies, as example this is a litmus test to see the
`basic-dependency` between the register-read event of `Xd` and the
register-write event of `Xd` in case of success:

```
AArch64 PAuth2 with FPAC auth success: RXd ---basic-dep--> WXd
Variant=pauth2,fpac
{
    int64_t x=0;
    int64_t y=0;
    0:X0=x; 0:X1=y; int64_t 0:X2=1;
    1:X0=x; 1:X1=y; int64_t 1:X2=0; int64_t 1:X3=0;

}
P0          | P1           ;
STR X2,[X0] | PACDZA X0    ; (* Write pacda(x) in 1:X0 *)
DMB SY      | LDR X2,[X1]  ;
STR X2,[X1] | EOR X3,X2,X2 ; (* Write 0 in 1:X3 *)
            | ADD X0,X0,X3 ; (* Write pacda(x) in 1:X0 *)
            | AUTDZA X0    ; (* Write x in 1:X0 *)
            | LDR X4,[X0]  ;
exists ( 1:X2=1 /\ 1:X4=0 )
(* Always forbidden with fpac because there is an `addr` dependency between
 * the two loads in `P1` *)
```

And here is a litmus test to observe the `pick-basic-dep` between the
register-read event of `Xn` and the register-write event of `Xd`:

```
AArch64 PAuth2 with FPAC auth success: RXn ---pick-basic-dep--> WXd
Variant=pauth2,fpac
{
  int64_t x=0;
  int64_t y=0;
  0:X0=x; 0:X1=y; int64_t 0:X2=1;
  1:X0=x; 1:X1=y; int64_t 1:X2=0; int64_t 1:X3=2; int46_t 1:X4=0;
}
P0          | P1           ;
STR X2,[X0] | PACDZA X0    ; (* Write pacda(x) in 1:X0 *)
DMB SY      | LDR X2,[X1]  ;
STR X2,[X1] | EOR X4,X2,X2 ; (* Write 0 in 1:X4 *)
            | AUTDA X0,X4  ; (* Write x in 1:X0 *)
            | STR X3,[X0]  ;
exists ( 1:X2=1 /\ [x]=1 )
(* Forbidden if `SCTLR_EL1.EnDA = 1` *)
(* Allowed otherwise *)
```

and a litmus test to see the absence of a `basic-dep`:

```
AArch64 PAuth2 with FPAC auth success: RXn ---no-basic-dep--> WXd
Variant=pauth2,fpac
{
  int64_t x=0;
  int64_t y=0;
  0:X0=x; 0:X1=y; int64_t 0:X2=1;
  1:X0=x; 1:X1=y; int64_t 1:X2=0; int64_t 1:X3=0; int46_t 1:X4=0;
}
P0          | P1           ;
STR X2,[X0] | PACDZA X0    ; (* Write pacda(x) in 1:X0 *)
DMB SY      | LDR X2,[X1]  ;
STR X2,[X1] | EOR X4,X2,X2 ; (* Write 0 in 1:X4 *)
            | AUTDA X0,X4  ; (* Write x in 1:X0 *)
            | LDR X3,[X0]  ;
exists ( 1:X2=1 /\ 1:X3=0 )
(* Always allowed *)
```

And here are two litmus tests to see the `pick-basic-dep` between the read event
of `Xd` and `Xn` and the fault event in case of failure:

```
AArch64 PAuth2 with FPAC auth success: RXn ---pick-basic-dep--> Fault
Variant=pauth2,fpac
{
    int64_t x=0;
    int64_t y=0;
    0:X0=x; 0:X1=y; int64_t 0:X2=1;
    1:X0=x; 1:X1=y; int64_t 1:X2=0; int64_t 1:X3=0; int64_t 1:X4=0;
}
P0          | P1           | P1.F        ;
STR X2,[X0] | LDR X2,[X1]  | LDR X3,[X0] ;
DMB SY      | EOR X3,X2,X2 |             ; (* Write 0 in 1:X3 *)
STR X2,[X1] | AUTDA X1,X3  |             ; (* Fault *)
            | MOV X4,#1    |             ; (* Write 1 in 1:X4 if hash collision *)
exists ( 1:X2=1 /\ 1:X4=0 /\ 1:X3=0 )
(* Forbidden if `SCTLR_EL1.EnDA = 1` bebcause of the `pick-basic-dep` *)
(* Forbidden otherwise bebcause P1 doesn't fault before the `mov x4` instruction *)
```

And

```
AArch64 PAuth2 with FPAC auth success: RXd ---pick-basic-dep--> Fault
Variant=pauth2,fpac
{
    int64_t x=0;
    int64_t y=0;
    0:X0=x; 0:X1=y; int64_t 0:X2=1;
    1:X0=x; 1:X1=y; int64_t 1:X2=0; int64_t 1:X3=0; int64_t 1:X4=0;
}
P0          | P1           | P1.F        ;
STR X2,[X0] | LDR X2,[X1]  | LDR X3,[X0] ;
DMB SY      | EOR X3,X2,X2 |             ; (* Write 0 in 1:X3 *)
STR X2,[X1] | ADD X1,X1,X3 |             ; (* Write y in 1:X1 *)
            | AUTDZA X1    |             ; (* Fault *)
            | MOV X4,#1    |             ; (* Write 1:X4 if hash collision *)
exists ( 1:X2=1 /\ 1:X4=0 /\ 1:X3=0 )
(* Forbidden if `SCTLR_EL1.EnDA = 1` bebcause of the `pick-basic-dep` *)
(* Forbidden otherwise bebcause P1 doesn't fault before the `mov x4` instruction *)
```

Here is the expected graph of the execution of the `autda Xd,Xn` instruction in
case of a success:

![`autda Xd,Xn` instruction success](pauth2_with_fpac_autda_success.png)

Here is the expected graph of the execution of the `autda Xd,Xn` instruction in
case of a failure:

![`autda Xd,Xn` instruction success](pauth2_with_fpac_autda_failure.png)

### `aut*` conclusion

Here is a table that show the dependency between the register-read event
in `Xd` and the register-write event in `Xd` for the instruction `autda Xd,Xn`
for all the possible set of feature that change this dependency (note that we
may replace `FEAT_FPAC` by `FEAT_FPACCOMBINE` in case of a combined instruction):

| `FEAT_PAuth2` | `FEAT_FPAC` | Success         | Fail             |
|:--------------|:------------|:----------------|:-----------------|
| No            | No          | `basic-dep`[^1] | `basic-dep`[^1]  |
| Yes           | No          | `basic-dep`[^1] | `basic-dep`[^1]  |
| Yes           | Yes         | `basic-dep`[^1] | Fault (no write) |

Here is a table that show the minimal dependency between the register-read event
in `Xn` and the register-write event in `Xd` for the instruction `autda Xd,Xn`
for all the possible set of feature that change this dependency:

| `FEAT_PAuth2` | `FEAT_FPAC` | Success              | Fail                 |
|:--------------|:------------|:---------------------|:---------------------|
| No            | No          | `pick-basic-dep`[^1] | `pick-basic-dep`[^1] |
| Yes           | No          | `pick-basic-dep`[^2] | `basic-dep`[^1]      |
| Yes           | Yes         | `pick-basic-dep`[^2] | Fault (no write)     |

[^1]: Memory model from the ASL implementation
[^2]: Correspond to the possibility of the `aut*` instruction to predict its
    success, because this was one of the demand of the architects in
    [this ticket](https://jira.arm.com/browse/AARCH-21866)

### `ldr` instruction:

Load instruction, in case we use virtual addresses, may have some additional
dependencies to check that the virtual address is canonical. These dependencies
are present also without Pointer Authentication, but PAC allow generating
non-canonical pointers. To do this we must add a branching event at each load that
depend on the input address, and an intrinsic control dependency between this
event and the load event each time we see an `ldr` instruction.

Here is an example of `ldr` success:

```
AArch64 load success
Variant=pauth2
{ 0:x0=x; int x = 42; }
P0;
  ldr x1,[x0];
exists
( 0:x1=42 /\ ~Fault(P0) )
```

![Expected event graph for an `ldr` success](ldr_success.png)

And here is an example of `ldr` failure:

```
AArch64 load failure
Variant=pauth2
{ 0:x0=pac(x, da, 0); int x = 42; }
P0;
  ldr x1,[x0];
exists
( 0:x1=0 /\ Fault(P0,MMU:Translation) )
```

![Expected event graph for an `ldr` failure](ldr_failure.png)

### `str` instruction:

Like the load instruction, the `str` instruction may fail because of an address
translation failure.

Here is an example of litmus test to illustrate a `str` success:

```
AArch64 str success
Variant=pauth2
{ 0:x0=x; 0:x1=42 }
P0;
  str x1,[x0];
exists
( [x]=42 /\ ~Fault(P0) )
```

![Expected event graph for an `str` success](str_success.png)

And here is an example of a `str` failure:

```
AArch64 str failure
Variant=pauth2
{ 0:x0=pac(x, da, 0); int x = 42 }
P0;
  str x1,[x0];
exists
( [x]=42 /\ Fault(P0, MMU:Translation) )
```

![Expected event graph for an `str` failure](str_failure.png)

And we have similar intrinsic control dependencies for the atomic operations
(`cas`, `swp`...).
