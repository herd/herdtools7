# Solving PAC hash collisions contraints

As I explain in [my previous notes](notes3.md), Pointer Authentication can
introduce hash collisions, as example this program

```
AArch64 Collision test
{ 0:x0=pac(x, da, 0) }
P0         ;
  autdzb x0;
exists
( ~Fault(P0) /\ 0:x0=x )
```

can finish without generating a fault because the pac field of `pac(x, da, 0)`
can be equal to the pac field of `pac(x, db, 0)` due to a hash collision. But
the user can add constraints to ensure that a program execute without hash
collisions:

```
AArch64 Collision test
{ 0:x0=pac(x, da, 0); 0:x1=pac(x, da, 0) }
P0         ;
  autdzb x0;
exists
( ~Fault(P0) /\ 0:x0=x /\ not (0:x1=pac(x, db, 0)) )
```

and in this case the final condition is unreachable because it ensure that their
is no hash collisions. To solve this problem we may use a constraints solver to
check that the current set of collisions doesn't generate a contradiction.

## Solving in presence of `FEAT_FPAC` and without `FEAT_CONSTPACFIELD`

With this set of features we can assume that the equations we have to solve are
of the form

- `canonical == canonical`
- `canonical != canonical`
- `pac_field(vaddr1, key1, modifier1) == canonical`
- `pac_field(vaddr1, key1, modifier1) == pac_field(vaddr2, key2, modifier2)`
- `pac_field(vaddr1, key1, modifier1) != canonical`
- `pac_field(vaddr1, key1, modifier1) != pac_field(vaddr2, key2, modifier2)`


(with `pac(varrd, key, modifier) == concat(pac_field(vaddr, key, modifier), lsb(x))`)

So we can solve these equations by using an union find data structure to
maintain the set of equalities, and raise a contradiction each times that the two
operands of an inequality are in different equivalence classes. This method works
until `2^n - 1` inequalities, with `n` the number of bits in the pac fields, as
explained in the second part.

## Solving in presence of `FEAT_CONSTPACFIELD` or without `FEAT_FPAC`

### Problem to solve
In this case the user can construct terms with a XOR of multiple pac fields, as
example this program

```
AArch64 multiple pac fields
{ 0:x0=pac(x, da, 42); }
P0         ;
  pacdzb x0;
exists
(0:x0=pac(pac(x, db, 0), da, 42))
```

and we can note that the `pac` function is commutative, associative, and is it's
own inverse. Because it use an exclusive OR and never depend of the previous
pac fields but only of the original virtual address `x`, and canonical act as a
zero (no pac field is present).

So the equations we have to solve are of the form:

- `x(1) ^ ... ^ x(n) == 0`
- `x(1) ^ ... ^ x(n) != 0`

with `x` a set of pac fields.

### Invariant

And we can solve them with an incremental Gause pivot algorithm (similar to a
simplex algorithm). The idea is to maintain a partition of the variables using a
set of "basic" variables and a set of "non-basic" variables and maintain the
equations in a canonical form:

- forall `x` basic, `x = y(1) ^ ... ^ y(n)` with `y` a set of non basic
    variables, so `y(1) ^ ... ^ y(n)` act as the "definition" of `x`
- each inequality is of the form `y(1) ^ ... ^ y(n) != 0` with `y` a set of non
    basic variables

### Proof

With this method the equalities are rewrite in a way such that they act as
definitions for the basic variables, and the non-basic variables are
unconstrained, so solving the set of inequality is relatively easy: If an
inequality is of the form `0 != 0` (`y` is empty), we found a contradiction.
Otherwise if we assume that their is less than `2^n - 1` inequalities (with `n`
the number of bits in the PAC field), then their is no contradiction:

- if `N` is the set of non basic variables, we can iterate over it's elements:
    - let `N(1) != f1(N(2), N(3), ...)`, `N(1) != f2(N(2),...)`... the inequalities
        that contains `N(1)`, if their is less than `2^n - 1` such inequalities,
        for all values of `N(2)`, `N(3)` we can find a value for `N(1)`
        different to `f1(N(2), ...)`, `f2(N(3), ...)`, ...
    - and we continue by using the inequalities that doesn't contains `N(1)`...

So if the program have more than `2^n - 1` inequalities I just raise an user
error. This method also prove the soundness of the previous union-find
algorithm.

### Algorithm

So all we have to do is to maintains the canonical form each times we add a new
equation and check if all the inequalities are not empty. To do it each times we
want to add an equation `x(1) ^ ... ^ x(n) == 0` or `x(n) ^ ... ^ x(n) != 0` we
start by replacing all the basic variables by their definition in term of
non-basic variables so we can assume that all the variables are non-basic, and:

- if this is an equality, if it is not empty, we set `x(1)` as a new basic
    variable of definition `x(2) ^ ... ^ x(n)` and remplace it by it's
    definition in all the current equations (equalities and inequalities). And
    if an inequality became empty, then we found a contradiction. This operation
    is call a pivot, and the new problem is equivalent to the oringinal problem.
- if this is an inequality we just check that it is not empty and add it to the
    set of inequalities.

### Example

Here is a program we want to test

```
AArch64 pac
Variant=pac,fpac,const-pac-field
{
  0:x0=pac(x,da,0);
  0:x1=pac(x,da,0);
}
P0            ;
  mov x2,#42  ;
  pacda x0,x2 ;
  autdba x0   ;
exists ( ~Fault(P0) /\ not (0:x1=pac(x,db,0)) )
```

This program with this final state has these contraints:

- `pac(pac(pac(x, da, 0), da, 42), db, 0) == x` due to the abscence of fault.
- `pac(x, da, 0) != pac(x, db, 0)`

We start by an empty solver state with no equations. So the solver state is

- basic variables: `{}`
- equalities: `{}`
- inequalities: `{}`

Then we add the first equation: `a ^ b ^ c = 0` (with `a := pac_field(x,da,0)`
, `b := pac_field(x,db,0)`, and `c := pac_field(x,da,42)`). We can use `a` as a
pivot variable and find the new solver state:

- basic variables: `{a}`
- equalities: `{a := b ^ c}`
- inequalities: `{}`

Finally we add the inequality `a ^ b != 0` that we simplify using the equality
`a == b ^ c`: `a ^ b != 0 <==> (b ^ c) ^ b != 0 <==> c != 0` because `XOR`
is associative, commutative and involutive. So the final state of the solver is

- basic variables: `{a}`
- equalities: `{a := b ^ c}`
- inequalities: `{c != 0}`

and this state is satisfiable because it contains less than `2^n - 1`
inequalities, and they are non-empty.

### Discussion about the hypothesis

This approach is not perfect because it suppose that we have less than `2^n`
inequalities but it is very unlikely to have this number of inequalities because
this number is per execution (with `n=15` for kvm-unit-tests). So the program
must either have a loop that we unfold 32768 times, or have more than 32768
lines of assembly...

An other assumption is that we don't use virtual addresses with non-canonical
pac fields as modifier for the pac computation. This assumption may change the
soundness: without it we must improve the union-find algorithm with a congruence
closure algorithm, and the simplex-like algorithm with a Nelson-Oppen algorithm
to combine it with a congruence closure algorithm and this make the
implementation of the solver very hard.
