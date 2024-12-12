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
following simplified table (in abscence of hash collisions):

| x0:         | x                  | pac(x,da,0)        | pac(x,ia,0)        |
|:------------|:-------------------|:-------------------|:-------------------|
| ldr x1,[x0] | `x1 <- mem[x]`     | Fault(Translation) | Fault(Translation) |
| pacdza x0   | pad(x,da,0)        | non deterministic  | non deterministic  |
| autdza x0   | Fault(PacCheck:DA) | x                  | Fault(PacCheck:DA) |
| xpacd x0    | x                  | x                  | x                  |

Here the non-deterministic means that `pacda` apply to a non-canonical virtual
address (most significant bits to `0...0` or `1...1`) can overwrite the bit 55
by the bit 63, and "forget" the virtual address range of the original pointer:
the `aut*` and `xpac*` use the bit 55 to deduce the virtual address range of the
address but the `pac*` instruction use the bit 63 (but the bit 55 if
`FEAT_CONSTPACFIELD` is implemented). In particular this means that this program

```
AArch64 switch VA range
{ 0:x0=x }
P0        ;
pacdza x0 ;
pacdza x0 ;
xpacd x0  ;
```

can write `x` in `x0` with probability `0.5`, or `0b1...10...0 ^ x` with
probability `0.5` (the number of `1` is the number of bits in the pac field).
For this reason my implementation in herd7 will raise an user error if we try to
add two pac field in a pointer using the `pac*` instruction.

## Hash collisions

The cryptographic signatures in PAC are small, because they must be in the most
significant bits of a virtual address. As example according to the tests I did
in `KVM-unit-tests`, they are `15` bits (the PAC field is `16` bits long but the
bit `55` is reserved to know the virtual address range of the signed pointer),
so the probability that two random pac fields are equals is `p=1 / 32768` but
this number depend of the size of the virutal address and may be lower or higher
in another context. In particular this means that this litmus test:

```
AArch64 Collisions in loads
{ 0:x0=pac(x, da, 0) }
P0              ;
  ldr x1, [x0]  ;
exists ( ~Fault(P0, MMU:Translation) )
```

will succeed with a probability `p`, and this test too:

```
AArch64 Collisions in aut*
{ 0:x0=pac(x, da, 0) }
P0          ;
  autdzb x0 ;
exists ( ~Fault(P0, PacCheck:DB) )
```

But their is also some collisions in the final state, as example

```
AArch64 Collisions with the final state
{ 0:x0=pac(x, da, 0) }
P0    ;
  nop ;
exists ( 0:x0=pac(x, da, 42) )
```

and

```
AArch64 Collisions with the final state 2
{ 0:x0=pac(x, da, 0) }
P0    ;
  nop ;
exists ( 0:x0=x )
```

may succeed with a probability `p`.

This means that each times we see an equality test that compare two pac fields,
if this test may fail, we must split the states in two because even if the pac
fields use different key or modifiers, they may be equal because of a hash
collusion. But `pac(x, da, 0)` wil always be different to `pac(y, da, 0)` in
abscence of aliasing because the less significant bits of `pac(x, da, 0)` are
equals to the less significant bits of `x` (and the bit 55), and these bits are
enough to see the difference between `x` and `y` (because all the other bits
replicate their bit 55).

The difficulty here is that the set of collisions we found must be coherent, as
example:

```
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

```
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

```
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

## Memory ordering

Pointer signature and authentication may add new ordering dependencies in the
memory model, in particular data dependency because the output of `pac*`, `aut*`
and `xpac*` depend syntacticly of their inputs. But also control dependencies
because `aut*` may fail and memory operation may fail if the virtual address
they use as input is not canonical.


### `pac*` instruction:

The `pac*` instruction must add two register read data dependencies, in
particular it will read two registers and use their values to write the output
value `pacda(x, da, 0x0)` in the destination register (`x0` in this example).

So here is an example of litmus test with the pacda instruction:

```
AArch64 pacda
{
  0:x0=x;
}
P0               ;
  pacda x0,x1      ;
exists
(0:x0=pac(x, da, 0))
```

And here is the expected event graph for an execution of this litmus test:

![Expected event graph for `pacda`](pacda.png)

### `xpac*` instruction:

The `xpacd` and `xpaci` instructions take a register and clear it's PAC field
(set it to `0...0` or `1...1` depending of the bit 55 of the virtual address).
So it add a data dependency between the input event and the output event.

As example the `xpacd` instruction must generate these events:

```
AArch64 xpacd
{ 0:x0=pac(x, da, 42) }
P0         ;
  xpacd x0 ;
exists
( 0:x0=x )
```

![Expected event graph for `xpacd`](xpacd.png)

### `aut*` instruction:

In presence of `FEAT_FPAC`, the `aut*` instruction is different to the `pacd*`
instruction because it can raise a fault, so it add some `iico_ctrl`
dependencies. And the presence of data dependencies depend of the success of the
operation because the fault handling doesn't dirrectly use the output register
of `aut*` but only set the error code and add to the `esr_el1` register the
the key (`ia`, `da`, `ib`, or `db`) that generate the fail.

Here is a litmus test with `aut*` that must succede:

```
AArch64 autda success
{ 0:x0=pac(x, da, 0) }
P0            ;
  autda x0,x1 ;
exists
( 0:x0=x /\ ~Fault(P0) )
```

![Expected event graph for an `autda` success](autda_success.png)

And here is a litmut test that may fail (in abscence of a hash collision):

```
AArch64 autdb failure
{ 0:x0=pac(x, da, 0) }
P0            ;
  autdb x0,x1 ;
exists
( 0:x0=pac(x, da, 0) /\ Fault(P0, PacCheck:DB) )
```

![Expected event graph for an `autdb` failure](autdb_failure.png)


***Observation:*** If my understanting of PAC is correct, the write event in
the register `x0` has a data dependency in the read event in the register `x1`
in the success case. Even if the value we write in `x0` doesn't semantically
depend of the value of `x1` (the value is `x` in case of success that doesn't
depend of the modifier, and nothing is write in case of failure). This is
because according to the Arm ARM the output value in `x0` contains in it's most
significant bits the exclusive `OR` of the pac field of `x0` and the pac field
of `ComputePAC(x, X[1, 64], ...)`, so the output has a syntactic dependency even
if the ASL code raise a fault if this pac field is not equal to a constant 7
lines after this EOR.

### `ldr` instruction:

Load instruction, in case we use virtual addresses, may have some additional
dependencies to check that the virtual address is canonical. These dependencies
are present also without Pointer Authentication, but PAC allow to generate non
canonical pointers. To do this we must add a branching event at each load that
depend of the input address, and an intrinsic control dependency between this
event and the load event each times we see an `ldr` instruction.

Here is an example of `ldr` success:

```
AArch64 load success
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

Here an example of litmus test to illustrate a `str` success:

```
AArch64 str success
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
{ 0:x0=pac(x, da, 0); int x = 42 }
P0;
  str x1,[x0];
exists
( [x]=42 /\ Fault(P0, MMU:Translation) )
```

![Expected event graph for an `str` failure](str_failure.png)

And we have similar intrinsic control dependencies for the atomic operations
(`cas`, `swp`...).
