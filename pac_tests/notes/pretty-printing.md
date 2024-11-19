# Pretty printing of hash collisions

As I explained in my previous [notes](notes3.md), it is possible to have hash
collision in the computation of Pointer Authentication Codes of virtual
addresses, these collisions can be represented as equalities or
inequalities of pac-fields, or XOR of pac-fields if we don't use `FEAT_FPAC` or
if we use `FEAT_CONSTPACFIELD`.


As I explained in a previous [note](solver.md), we can solve such systems of
equalities and inequalities using a constraint solver based on a symplex-like
algorithm using Gauss pivots to maintain a set of basic variables, a set of
definition of basic variables as XOR of non-basic variables, and a set of
inequalities of XOR of non-basic variables.


This note will discuss the ways to use the internal representation of this
solver state to pretty print the output states in herd, and the limitation of
the current implementation.

All this note is about the pretty printing if `-debug pac,pred-solver` is
enabled because otherwise we cannot pretty print any PAC for solver state
because this information is not accessible bly litmus and the output of herd
must match the output of litmus.

## Normalisation

### A good example

A first approach is to use normalisation of the output states to pretty print
the results. This approach is easy to implement, because we just need to relace
all the basic variables in the states by their representation as XOR of non-basic
variables. This method allow some good results, as example this litmus test:

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

But normalisation is not always enough to understant the behaviour of a program,
in particular, one can want to see the internal state of the solver to know the
exact set of collisions requires seeing a final state, and modify the program to
avoid them.


### A Pathological example

## Pretty printing the solver

The pretty printing of the solver state is quite easy, the only thing we need
is to translate each of the equalities/inequalities of PAC fields into
equalities/inequalities of constants. In addition, I remove all the inequalities
because the values are differents unless they are explicitly equals.
