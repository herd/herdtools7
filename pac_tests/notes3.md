# Pointer Authentication Code

This note focus on Pointer Authentication Code with the different extensions:
- `FEAT_Pauth`: Basic pointer authentication extension
- `FEAT_Pauth2`: Change the way we add an authentication to a pointer
- `FEAT_FPAC`: Allow to generate a fault dirrectly at the autheutiation in case
    of a fail, instead of faulting at memory translation.

## Instructions

Pointer authentication add some new instructions, the new one are
- `pac* x,y` with `*`the correponding authentication key (may be `da`, `db`,
    `ia` or `ib`), and their is some special instruction for diffent specific
    values of `y`, like `pacdza x` for `y=XZR`. This instruction add a pac field
    (a cryptographic signature of `(x, y)` using the given key) in the most
    significant bits of the virtual address `x`. We call `y` the modifier.
- `xpacd x` or `xpaci x`: remove the pac field of the virtual address `x`, in
    abscence of address tagging the two instruction have the same semantic.
- `aut* x, y`: authenticate the pac field of a virtual address using a given
    key, a virtual address `x` and a modifier `y`. If the anthentication is a
    success then this instruction return the initial value of `x` (it remove
    it's pac field), otherwise a fault is generated.

To understand the sequencial behaviour of these instruction we can write the
following simplified table:

| x0:         | x                  | pac(x,da,0)        | pac(x,ia,0)        |
|:------------|:-------------------|:-------------------|:-------------------|
| ldr x1,[x0] | `x1 <- mem[x]`     | Fault(Translation) | Fault(Translation) |
| pacdza x0   | pad(x,da,0)        | none deterministic | non deterministic  |
| autdza x0   | Fault(PacCheck:DA) | x                  | Fault(PacCheck:DA) |
| xpacd x0    | x                  | x                  | x                  |

## Hash collisions

The cryptographic signatures in PAC are small, because they must be in the most
significant bits of a virtual address. As example according to the tests I did
in `KVM-unit-tests`, they are `15` bits (the PAC field is `16` bits long but the
bit `55` is reserved to know the virtual address range of the signed pointer),
so the probability that two random pac fields are equals is `p=1 / 32768`. In
particular this means that this litmus test:

```asm
AArch64 Collisions in loads
{ 0:x0=pac(x, da, 0) }
P0              ;
  ldr x1, [x0]  ;
exists ( ~Fault(P0, MMU:Translation) )
```

will succeed with a probability `p`, and this test too:

```asm
AArch64 Collisions in aut*
{ 0:x0=pac(x, da, 0) }
P0          ;
  autdzb x0 ;
exists ( ~Fault(P0, PacCheck:DB) )
```

It's important to note that the number of bits of the pac field depend of the
context of execution and in particular the size of the virtual addresses, in
another context the pac field may be `8` or `31` bits...

But their is also some collisions in the final state:

```asm
AArch64 Collisions with the final state
{ 0:x0=pac(x, da, 0) }
P0    ;
  nop ;
exists ( 0:x0=pac(x, da, 42) )
```

and

```asm
AArch64 Collisions with the final state 2
{ 0:x0=pac(x, da, 0) }
P0    ;
  nop ;
exists ( 0:x0=x )
```

may succeed with a probability `p`.


The difficulty here is that the set of collisions we found must be coherent, as
example:

```asm
AArch64 Incoherent collisions (aut*)
{
    0:x0=pac(x, da, 0);
    0:x1=pac(x, db, 0)
}
P0          ;
  autdzb x0 ;
exists ( 0:x1=pac(x, da, 0) /\ Fault(P0, PacCheck:DB) )
```

and

```asm
AArch64 Incoherent collisions (final state)
{
    0:x0=pac(x, da, 0);
    0:x1=pac(x, db, 0)
}
P0    ;
  nop ;
exists ( 0:x0=pac(x, db, 0) /\ ~(0:x1=pac(x, da, 0)) )
```

and

```asm
AArch64 Incoherent collisions (cmp and autdzb)
{
    0:x0=pac(x, da, 0);
    0:x1=pac(x, db, 0)
}
P0            ;
  cmp x0, x1  ;
  b.eq finish ;
  autdzb x0   ;
  mov x2,#1   ;
finish:       ;
exists ( 0:x2=1 )
```

will never succeed because at two different points they require that a same
collision (`pac(x, da, 0) = pac(x, db, 0)`) is observed and not observed in the
same execution.
