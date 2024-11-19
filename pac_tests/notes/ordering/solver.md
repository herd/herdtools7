# Solving PAC hash collisions contraints

As I explain in [my previous notes](ordering.md), Pointer Authentication can
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

And in this case the final condition is unreachable because it ensures that there
is no hash collisions. To solve this problem we may use a constraint solver to
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

So we can solve these equations by using a union find data structure to
maintain the set of equalities, and raise a contradiction each time that the two
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

and we can note that the `pac` function is commutative, associative, and is its
own inverse. Because it use an exclusive OR and never depend on the previous
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

- forall `x` basic, `x = y(1) ^ ... ^ y(n)` with `y` a set of non-basic
    variables, so `y(1) ^ ... ^ y(n)` act as the "definition" of `x`
- each inequality is of the form `y(1) ^ ... ^ y(n) != 0` with `y` a set of
    non-basic variables

### Proof

With this method the equalities are rewriting in a way such that they act as
definitions for the basic variables, and the non-basic variables are
unconstrained, so solving the set of inequality is relatively easy: If an
inequality is of the form `0 != 0` (`y` is empty), we found a contradiction.
Otherwise, if we assume that there is less than `2^n - 1` inequalities (with `n`
the number of bits in the PAC field), then there is no contradiction:

- if `N` is the set of non-basic variables, we can iterate over its elements:
    - let `N(1) != f1(N(2), N(3), ...)`, `N(1) != f2(N(2),...)`... the inequalities
        that contain `N(1)`, if there is less than `2^n - 1` such inequalities,
        for all values of `N(2)`, `N(3)` we can find a value for `N(1)`
        different to `f1(N(2), ...)`, `f2(N(3), ...)`, ...
    - and we continue by using the inequalities that doesn't contain `N(1)`...

So if the program have more than `2^n - 1` inequalities I just raise a user
error. This method also prove the soundness of the previous union-find
algorithm.

### Algorithm

So all we have to do is to maintain the canonical form each time we add a new
equation and check if all the inequalities are not empty. To do it each time we
want to add an equation `x(1) ^ ... ^ x(n) == 0` or `x(n) ^ ... ^ x(n) != 0` we
start by replacing all the basic variables by their definition in terms of
non-basic variables, so we can assume that all the variables are non-basic, and:

- if this is an equality, if it is not empty, we set `x(1)` as a new basic
    variable of definition `x(2) ^ ... ^ x(n)` and remplace it by its
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

Finally, we add the inequality `a ^ b != 0` that we simplify using the equality
`a == b ^ c`: `a ^ b != 0 <==> (b ^ c) ^ b != 0 <==> c != 0` because `XOR`
is associative, commutative and involutive. So the final state of the solver is

- basic variables: `{a}`
- equalities: `{a := b ^ c}`
- inequalities: `{c != 0}`

And this state is satisfiable because it contains less than `2^n - 1`
inequalities, and they are non-empty.

### Discussion about the hypothesis

This approach is not perfect because it supposes that we have less than `2^n`
inequalities, but it is very unlikely to have this number of inequalities because
this number is per execution (with `n=15` for kvm-unit-tests). So the program
must either have a loop that we unfold 32768 times, or have more than 32768
lines of assembly...

Another assumption is that we don't use virtual addresses with non-canonical
pac fields as modifier for the pac computation. This assumption may change the
soundness: without it, we must improve the union-find algorithm with a congruence
closure algorithm, and the simplex-like algorithm with a Nelson-Oppen algorithm
to combine it with a congruence closure algorithm and this make the
implementation of the solver very hard.

# Constraint solver (CollisionSolver.ml)

## Current solver

Now the challenge is to add this collision solver inside the existing constraint
solver in `valconstraint.ml` and to adapt `mem.ml` to use the new solver. This
solver is in charge of solving the constraints generated by the event monad (in
`eventsMonad.ml`):

- if we have a symbolic variable (a value of the form `v: V.Var x` with `x :
    V.csym`), and we perform the operation `M.op Op.Eq v v'`, then the event
    monad will call `Op.op Op.Eq v v'` and this function will raise an
    `Undetermined` exception. The event monad will catch this exception and add
    the constraint `VC.Assign (free_var, Binop Op.Eq v v')` to the constraint
    solver to be solved at the end of the execution, and return `fresh_var`.
- Then the constraint solver will be in charge of solving the list of final
    constraints of the execution, each constraint can be an assignation (of a
    variable or a constant), or a delayed exception like a user warning.


To solve the constraints, the solver use a two phases algorithm:

- The first phase is a union find algorithm to solve all the constraints of the
    form `Assign (V.Var i, Atom (V.Var j))` and generate a new system of
    equations without these constraints by replacing each variable by the
    representant of its equivalence class.
- The second phase is a topological sort to identify the strong connected
    components. Then the solver complete a solution (a map from variables to
    constants) following the topological order, and perform one pass on each
    SCC (strong connected components). Then it returns the solution and the set
    of unsolved constraints if it doesn't find a contradiction.


### Example

This is a set of constraints (with `S0, S1...` the variables, and `A, B, C...`
the constraints).

```
A: S0 := 0
B: S0 := S2 + S1
C: S1 := 0
D: S2 := S0
```

According to the union find phase it's equivalent to solve these constraints:

```
A: S0 := 0
B: S0 := S0 + S1
C: S1 := 0
```

And the topological sort will order them in this order:

```
SCC0: { C: S1 := 0 }
SCC1: { A: S0 := 0 }
SCC2: { B: S0 := S0 + S1 }
```

And the solver will find a solution `{S0 := 0; S1 := 0; S2 := 0}` (the solver
assign `S2` to the value of the representant of its equivalence class).


## Adding the collision solver


### Changes in `archOp.ml`, `AArch64Op.ml` and `value.mli`

Now each architecture can declare its own type of constraint, using a generic
interface like:

```ocaml
type predicate

exception Constraint of predicate * cst * cst

val eq_satisfiable : cst -> cst -> predicate option
```

The new `predicate` type represent a constraint that a constraint solver
(declared in `value.mli`) have to check, when an architecture specific operation
raise the exception `Constraint (pred, ctrue, cfalse)`, then the result must be
interpreted as `if pred then ctrue else cfalse`. And each times this
exception is raise, `eventsMonad.ml` will delay the resolution of the operation
similarly that the `Undetermined` exception.

And if we have to look for the equality of two constants, now the result of the
equality must be interpreted using:

```ocaml
if Cst.eq c1 c2 then c_true (* In this case eq_satisfiable must return None *)
else
    match eq_satisfiable c1 c2 with
    | Some pred -> raise (Constraint (pred, c_true, c_false))
    | None -> c_false
```

In particular this function is used in `symbValue.ml` as example to compare two
virtual addresses with te instruction `CMP`, or to assume the equality of two
constants during the resolution of the assignations in `valconstraint.ml`.

Then the satisfiability of the conjunction of literal (a predicate  or it's
negation) generated during an execution will be solved in `valconstraint.ml` and
`constraints.ml` using an architecture specific constraint solver declared in
`value.mli` with this interface:

```ocaml
type solver_state

val empty_solver

val add_predicate : bool -> predicate -> solver_state -> solver_state option
```

At any times the solver state represent a conjunction of literals and the
`add_predicate` must return `None` if the conjunction of the previous literals
represented by the solver plus the new literal is unsatisfiable, or
`Some new_solver` otherwise.


### Changes in the constraints solver

Now `valconstraint.ml` need to solve some sets of assignations and literals
(used to propagate the predicates from one resolution to another) knowing that
the resolution of some assignations will raise a
`Constraint (pred, ctrue, cfalse)` exception. Each of those exceptions split the
resolution in two (the case assuming the predicate, and the case assuming it's
negation), and as a consequence the solver must return a list of solution
instead of a unique solution, and this have some consequence it's design.


To do so I use a new ***monad***!

```ocaml
type monad_state =
    { solution: cst V.Solution.t
    ; solver: V.solver_state
    ; predicates: cnstrnts }

type 'a solver_monad = monad_state -> (monad_state * 'a) list

(* Bind operator *)
let (let*) (x: 'a solver_monad) (f: 'a -> 'b solver_monad) : 'b solver_monad =
    fun st -> List.concat_map (fun (st,y) -> f y st) (x st)

(* Map operator *)
let (let+) (x: 'a solver_monad) (f: 'a -> 'b) : 'b solver_monad =
    fun st -> List.map (fun (st,y) -> (st,f y)) (x st)

(* Return operator *)
let pure : 'a -> 'a solver_monad = fun st x -> [st,x]

(* Raise a contradiction *)
let contradiction : 'a solver_monad = fun _ -> []

(* Explore two possibilities *)
let alternative (f g: 'a solver_monad) : 'a solver_monad =
    fun st -> List.concat [f st; g st]
```

So an object of type `'a solver_monad` represent a computation that given the
current state, iterate over all the possible solutions of PAC fields equalities
or inequalities. As example `contradiction` read the current state and return
the empty set of executions, and `alternative` take two computations, and perform
them in "parallel".

With this in mind we can build a function to be call in case we try to solve an
operation that raise a `Constraint` exception:

```ocaml
let add_predicate (b: bool) (p: V.arch_pred) : unit solver_monad =
    (* Add the new literal to propagate them to the next calls of the solver *)
    let* preds = get_predicates in
    let* _ = set_predicates (Predicate (b,p) :: preds) in

    let* solver = get_solver in
    match V.add_predicate b p solver with
    | Some solver -> set_solver solver
    | None -> contradiction

let solve_predicate (pred: V.arch_pred) (vtrue vfalse: V.v) : V.v solver_monad =
    alternatice
        (let+ _ = add_predicate true pred in vtrue)
        (let+ _ = add_predicate false pred in vfalse)
```

This function take a literal, and split its state in two to solve the case
asssuming the predicate, and the case assuming its negation in two
independant traces. Then we can use those functions to define all the operators
we need for the resolution:

```ocaml
val assume_equality : cst -> cst -> unit solver_monad
val add_solution : V.csym -> cst -> unit solver_monad
val process_assignation : V.v -> cst -> unit solver_monad
val subst_expr : expr -> expr solver_monad
...
val solve_one : cnstrnt -> cnstrnts solver_monad
val solve_topo : cnstrnts -> answer
```
