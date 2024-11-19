# Features relative to the PAC collision constraint solver

## List of the possible sources of hash collisions

### Memop address collision

Memory operations must raise a translation fault if the virtual address they use
as input contains a non-canonical PAC field. But in case of a hash collision a
virtual address may not be syntactically canonical but semantically canonical.
As example:

```
AArch64 Aut* instruction

{
  0:x0=pac(x, da, 42);
}

P0          ;
  autdza x0 ;

exists ( ~Fault(P0) )
```

The following output is expected:

```
States 2
  ~Fault(P0);
 Fault(P0, pac(x,da,42), MMU:Translation);
Ok
Witnesses
Positive: 1 Negative: 1
Condition exists (not (fault(P0)))
```

because `pac(x,da,42)` may be equal to `x` due to a hash collision in their PAC
fields.

### Aut* virtual address collision

In presence of `FEAT_FPAC`, an `aut*` instruction must fail if the PAC field of
the virtual address doesn't correspond to the expected PAC field given by the
instruction key and modifier. But in presence of hash collisions, two
syntactically differents PAC fields may be semantically equals, as example

```
AArch64 Aut* instruction

{
  0:x0=pac(x, db, 42);
}

P0          ;
  autdza x0 ;

exists ( ~Fault(P0) )
```

The following output is expected:

```
States 2
  ~Fault(P0);
 Fault(P0,PacCheck:DA);
Ok
Witnesses
Positive: 1 Negative: 1
Condition exists (not (fault(P0)))
```

because in one of the execution `pac(x, db, 42) == pac(x, da, 0)` so the fault
is not raise in this execution.

Without `FEAT_FPAC`, `aut*` doesn't check for any collision because it return
the `xor` of the two PAC fields and doesn't check that the output is canonical.

### Cmp virtual address collision

### Pac* collision

Without `FEAT_CONSTPACFIELD`, the `pac*` instructions doesn't support a virtual
address with a non-canonical PAC field as input, but in presence of a hash
collision, the PAC field may be non syntactically canonical, but semantically
canonical:

```
AArch64 Pac* collision

{
  0:x0=pac(x, da, 42);
  0:x1=x;
}

 P0                  ;
  cmp x0, x1         ;
  b.ne finish        ;
  pacdza x0          ;
finish:              ;
exists ( 0:X0=pac(x, da, 0) )
```


The following output is expected

```
States 2
0:X0=pac(x, da, 0x0, 0);
0:X0=pac(x, da, 0x2a, 0);
Ok
Witnesses
Positive: 1 Negative: 1
Condition exists (0:X0=pac(x, da, 0x0, 0))
Observation L001 Sometimes 1 1
```

because the virtual address in `x0` will always be canonical because of the hash
collision in the previous `cmp` instruction.

### Sub or Xor address collision

Xor and sub instruction is only partial in herd7, so my implementation is a
best-effort to add the PAC field collision check for these instruction. In
particular to have a correct semantic for the `cmp x,y; b.eq label;` sequence
that use the `sub` implementation inside the `cmp` instruction to test for
equality.

Currently the `sub` of two virtual addresses `x, y` just return the value of
`Constant.compare x y`, so it return a value in `0, 1, -1`, and is used in
practice to set the correct flag for the equality test of virtual addresses in
code sequences like `cmp x,y; b.eq label;` because the `cmp` instruction will
use the same implementation as `sub` in `herd/symbValue.ml`.
This semantic is unsound because

```
AArch64 Sub bug
{ 0:x0=x; 0:x1=y; int x = 0; int y = 0; }
P0             ;
  sub x0,x0,x1 ;
exists ( 0:x0=1 )
```

will return

```
States 1
0:X0=-1;
No
Witnesses
Positive: 0 Negative: 1
Condition exists (0:X0=1)
Observation L001 Never 0 1
```

even if this value is not possible: the substraction of two addresses must be a
multiple of their alignment (`4` in this case). Also only one possible value is
returned by herd7, but this is not coherent with `litmus` because it may return
more than one value (`PAGE_SIZE, 2*PAGE_SIZE, 3*PAGE_SIZE...`). So we may
expect an user error to be raise in this situation instead of one incorrect
value.

So my implementation just return `0` in case of a collision or the value of the
previous implementation otherwise.


For the moment my implementation doesn't support the `xor` instruction.


### Final state

The final state can introduce some "collisions" too, as example if we assign the
value `pac(x,da,42)` to a register in a program, and we assert in the final
state that the register have the value `x`. This assertion may fail or succede
depending of the presence of a collision between `pac(x,da,42)` and `x`.

```
AArch64 Final state collision
{ 0:x0=pac(x, da, 42) }
P0    ;
  nop ;
exists ( 0:x0=x )
```

must return the following output

```
States 2
0:X0=pac(x, da, 0x2a, 0);
0:X0=x;
Ok
Witnesses
Positive: 1 Negative: 1
Condition exists (0:X0=x)
Observation L001 Sometimes 1 1
```

But this program:

```
AArch64 Final state collision
{ 0:x0=pac(x, da, 42) }
P0    ;
  nop ;
exists ( 0:x0=pac(x,da,42) )
```

must return the following output

```
States 1
0:X0=pac(x, da, 0x2a, 0);
Ok
Witnesses
Positive: 1 Negative: 0
Condition exists (0:X0=pac(x, da, 0x2a, 0))
Observation L001 Always 1 0
```

because the evaluation of the output property doesn't depend of the presence of
a hash collision.
