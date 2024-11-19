# Pretty printing of hash collisions

As I explained in my previous [notes](notes3.md), it is possible to have hash
collision in the computation of Pointer Authentication Codes of virtual
addresses, these collisions can be represented as equalities or
inequalities of pac-fields, or XOR of pac-fields if we don't use `FEAT_FPAC` or
if we use `FEAT_CONSTPACFIELD`.


As I explained in a previous [note](solver.md), we can solve such systems of
equalities and inequalities using a constraints solver based on a symplex-like
algorithm using Gauss pivots to maintain a set of basic variables, a set of
definition of basic variables as XOR of non-basic variables, and a set of
inequalities of XOR of non basic variables.


This note will discuss the ways to use the internal representation of this
solver state to pretty print the output states in herd, and the limitation of
the current implementation.


## Normalisation

### A good example

A first approach is to use normalisation of the output states to pretty print
the results. This approach is easy to implement, because we just need to relace
all the basic variables in the states by their representation as XOR of non-basic
variables, and allow some good results. As example this litmus test

```
AArch64 normalisation_ex
Variant=pac,fpac,const-pac-field
{ 0:x0=pacda(x,0) }
P0;
    autdzb x0;

locations [0:x0;]
exists (~Fault(P0))
```

will generate two states without normalisation:

```
0:x0=pacda(pacdb(x,0),0); ~Fault(P0);
0:x0=pacda(x,0); Fault(P0,PacCheck:DB);
```

So it's not clear that the first state succede because `pacda(pacdb(x,0),0)` is
equal to `x` in the current solver state. But with normalisation, it's possible
to simplify this equation and have as a final state:

```
0:x0=x; ~Fault(P0);
0:x0=pacda(x,0); Fault(P0,PacCheck:DB);
```

In this case, it's clear that the first test pass, because we directly have a
canonical value in `x0`.

### A pathological example

But normalisation is not always enough to solve the problem of making visible
the reason of the success or failure of a test. As example if the variable is
already canonical, but we test it this variable is equal a more complicated
term, normalisation fail:

```
AArch64 normalisation_cex
Variant=pac,fpac,const-pac-field
{ 0:x0=x }
P0;
    nop;
exists (0:x0=pacda(x,0))
```

This example will generate two states (with or without normalisation):

```
0:x0=x;
0:x0=x;
```

one that pass, and the other fail because in one case the solver contain the
equality `pacda(x,0)=x`, in the other case it contain the inequality
`pacda(x,0)<>x`. So all test that use this pattern (testing in the final state
if a simple term like `x` is equal to a more complicated term) will generate
confusing results if we only use normalisation.

## Pretty printing the solver

The solution I use to solve the problem is to simply pretty print the solver
state. As example the previous test will give the following result:

```
0:x0=x; pacda(x,0)=x;
0:x0=x; pacda(x,0)<>x;
```

So it's clear that only the first result is a success. Now the question is what
set of solver states we want in function of the test that we are looking for.

- The list of all the possible models of the formula
- The set of minimal models to not hase useless redondencies in the final
    states
- An approximation using the syntax of the formula

### A first example to see the two first options

```
AArch64 pp_example1
Variant=pac,fpac,const-pac-field
{ 0:x=pacda(x,0) }
P0;
    nop;
exists ( 0:x0=x \/ 0:x0=pacdb(x,0) )
```

Here is the list of all the possible models of the formula:

```
0:X0=x; pacda(x, 0x0)=x; pacda(x, 0x0)=pacdb(x, 0x0);
0:X0=x; pacda(x, 0x0)=x; pacda(x, 0x0)<>pacdb(x, 0x0);
0:X0=pacdb(x, 0x0); pacda(x, 0x0)=pacdb(x, 0x0); pacda(x, 0x0)<>x;
0:X0=pacda(x, 0x0); pacda(x, 0x0)<>pacdb(x, 0x0); pacda(x, 0x0)<>x;
```

All possible equalities and inequalities must be assigned even if the
satisfaction of the output formula doesn't depend of those predicates.


Here is the set of minimal models of the output condition:

```
0:X0=x; pacda(x, 0x0)=x;
0:X0=pacdb(x, 0x0); pacda(x, 0x0)=pacdb(x, 0x0);
0:X0=pacda(x, 0x0); pacda(x, 0x0)<>pacdb(x, 0x0); pacda(x, 0x0)<>x;
```

For each possible execution, the output states for a given proposition represent
the minimum set of equalities/inequalities to satisfy the proposition or it's
negation.

### Current approximation

The current use none of those semantics, instead I use a syntactic search
because this approach is easier to implement. Concretly the searh algorithm is
the following:

```ocaml
type 'a monad = solver_state -> ('a * solver_state) list
let do_rec sign : unit monad = function
(* a simplified predicate to see the principle *)
    | Eq x y ->
        if sign
        then assume_equality x y
        else assume_inequality x y
    | Or ps ->
        if sign
        then alt (List.map (do_rec sign) ps)
        else iter (List.map (do_rec sign) ps)
    | And ps ->
        if sign
        then iter (List.map (do_rec sign) ps)
        else alt (List.map (do_rec sign) ps)
    | Not p ->
        do_rec (not sign) p
    ...
```

so the search is split at each disjunction, even if the solver state returned by
one branch is a subset of the solver state returned by another branch. At the
end I do some manipulations to remove all the duplications (when two solver
states are equals).


To see the difference here is a modified version of the first example:

```
AArch64 pp_example2
Variant=pac,fpac,const-pac-field
{ 0:x=pacda(x,0) }
P0;
    pacdzb x0;

exists ( (0:x0=x \/ 0:x0=pacdb(x,0)) \/ ~Fault(P0) )
```

The set of minimal models is the following:

```
0:X0=x;  ~Fault(P0); pacda(x, 0x0)=pacdb(x, 0x0);
0:X0=pacda(x, 0x0); Fault(P0,PacCheck:DB); pacda(x, 0x0)<>pacdb(x, 0x0);
```

But the current version return the following result:

```
0:X0=x;  ~Fault(P0); pacdb(x, 0x0)=x; pacda(x, 0x0)=x;
0:X0=x;  ~Fault(P0); pacda(x, 0x0)=pacdb(x, 0x0);
0:X0=pacda(x, 0x0); Fault(P0,PacCheck:DB); pacda(x, 0x0)<>pacdb(x, 0x0); pacda(x, 0x0)<>x;
0:X0=pacda(x, 0x0); Fault(P0,PacCheck:DB); pacda(x, 0x0)<>pacdb(x, 0x0);
```

because it will return some states even if they are a subset of some other
states with the same execution and evaluation of the final state. So some output
states are "useless".
