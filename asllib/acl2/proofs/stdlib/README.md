
ASL Standard Library ACL2 Correctness Proofs
============================================

This directory contains ACL2 proofs about the behavior of ASL standard
library definitions, located in this repository
[HERDTOOLS7](github.com/herd/herdtools7) in asllib/libdir/stdlib*.asl, as
interpreted by the ACL2 ASL interpreter (asllib/acl2/interp.lisp).

To run the proofs, first please follow the directions under
"Installation" in asllib/acl2/README.md. Then you may run `make` in
this directory to run the proofs.  Parallelism (e.g. `make -j 8`) can
be helpful in speeding up this process.

The build system for this directory doesn't recognize when ASLRef or
its standard library is updated. That is, if the proofs were
previously run to completion and ASLRef was subsequently updated,
running `make` will not rebuild anything. Run `make clean` first in
order to clear out the previous certificates so they will be actually
rebuilt.

Proofs may fail if ASL standard library functions change. If new
standard library functions are added but the existing functions remain
the same, the existing proofs should continue to work but a
completeness check in `top.lisp` will then fail. Same if an existing
proof is removed.

Understanding What Is Proved
----------------------------

The theorems about the stdlib functions are generated via a macro named
`def-asl-subprogram`. For example:

```
(def-asl-subprogram LowestSetBitNZ-correct
  :function "LowestSetBitNZ"
  :params (n)
  :args (x)
  :error-cond (equal x.val 0)
  :return-values ((v_int (bitops::trailing-0-count x.val)))
  :prepwork (...))
```

The above form generates an ACL2 `defthm` command, that is, a request
to ACL2 to prove a theorem. The `def-asl-subprogram` form can be read
as follows to get a general understanding of what the theorem says:

> The theorem `LowestSetBitNZ-correct` says that if the ASL function
> `LowestSetBitNZ` is run on well-typed parameters `n` and arguments `x`,
> then if the `val` field of `x` equals 0, it results in an error; otherwise,
> it produces a single return value which is an integer value type (`v_int`)
> containing the `trailing-0-count` of the `val` field of `x`.

(The `:prepwork` argument just gives some preparatory commands to ACL2
and doesn't affect the statement of the theorem.)

For more detail, we can certify the book containing this theorem, `bits.lisp`:
```
cert.pl bits.lisp
```
then run `acl2` and include that book:
```
(include-book "bits")
```
so we can then examine the theorem:
```
:pe LowestSetBitNZ-correct
```
which prints the following (line numbers added for our reference):
```
1.      (DEFTHM LOWESTSETBITNZ-CORRECT
2.       (B* (((V_INT N)) ((V_BITVECTOR X)))
3.        (IMPLIES
4.         (AND
5.          (SUBPROGRAMS-MATCH
6.               '("LowestSetBitNZ" "Zeros-1" "IsZero" "LowestSetBit")
7.               (GLOBAL-ENV->STATIC (ENV->GLOBAL ENV))
8.               (STDLIB-STATIC-ENV))
9.          (EQUAL (VAL-KIND N) :V_INT)
10.         (EQUAL (VAL-KIND X) :V_BITVECTOR)
11.         (EQUAL X.LEN N.VAL)
12.         (<= 2 (IFIX CLK)))
13.        (LET*
14.         ((RES
15.             (MV-NTH 0
16.                     (EVAL_SUBPROGRAM ENV "LowestSetBitNZ" (LIST N)
17.                                      (LIST X))))
18.          (SPEC
19.           (EV_NORMAL
20.               (FUNC_RESULT
21.                    (LIST (V_INT (BITOPS::TRAILING-0-COUNT X.VAL)))
22.                    (ENV->GLOBAL ENV)))))
23.         (AND (IMPLIES (EQUAL X.VAL 0)
24.                       (EQUAL (EVAL_RESULT-KIND RES)
25.                              :EV_ERROR))
26.              (IMPLIES (NOT (EQUAL X.VAL 0))
27.                       (EQUAL RES SPEC))))))
28.      :HINTS ...)
```

Line by line:

1. Name the theorem LowestSetBitNZ-Correct
2. Destructring bindings for accessing fields of `n` and `x` --
   namely, `n.val`, `x.val`, and `x.len`
4. Hypotheses:
5. The static environment of `env` has definitions for LowestSetBitNZ
   and three supporting functions that match the definitions stored in
   `(stdlib-static-env)`, i.e., the static env resulting from parsing
   and typechecking an empty program
9. `n` is an integer value (required, as with the following two
    hypotheses, by the type signature of LowestSetBitNZ)
10. `x` is a bitvector value
11. The length of bitvector `x` equals the value of integer `n`
12. The interpreter's global recursion limit `clk` (an invisible
    argument to `eval_subprogram`) is at least 2
13. Bindings for the conclusion:
14. Bind `res` to the first (`(mv-nth 0 ...)`) value returned from
    `eval_subprogram` of `LowestSetBitNZ` with parameters `n` and
    arguments `x`
18. Bind `spec` to the expected result: a normal evaluation pairing a
    single return value, an integer value containing the
    `trailing-0-count` of x's value, with the unchanged global
    environment
23. If the value of `x` is 0, then the subprogram produces an error.
16. Otherwise, the result `res` equals the expected result `spec`.

Notes about this:
 * The environment here is a free variable, and all we assume about it
   is that the listed subprogram definitions match those from the
   parsed constant static environment from an empty program.   
 * `n` and `x` are of a type `val` that is essentially our choice of
   representation for ASLRef's "native value". Its type definition is
   in "asllib/acl2/interp-types.lisp". It is a sum-of-products type;
   the variants used here are `v_int` which contains an integer named
   `val`, and `v_bitvector` which contains a natural named `len` and
   an integer named `val` (with the further restriction that `val` is
   an unsigned integer of length at most `len`).
 * eval_subprogram is similar to the ASLRef function eval_subprogram,
   but here returns two results: the fist, which is similar to the
   ASLRef result, and the second, an "oracle" which provides
   nondeterministic values for the `E_ARBITRARY` expression type. As a
   general rule we don't prove anything about the oracle.
 * The (first) value returned by eval_subprogram is of a type similar
   to the ASLRef interpreter's `func_eval_type`. It is an eval_result
   -- a type similar to ASLRef's maybe_exception, except that in
   addition to the Normal (here ev_normal) and Throwing (here
   ev_throwing) cases it has an Error case (here ev_error). The
   payload of that eval_result in the normal case is a product type
   `func_result`, a list of values paired with a global environment.
 * So far we don't prove anything about the exact error produced if
   there is an error, but we do prove exactly what the function
   produces in the normal (non-error, non-throwing) case.

Specifications
--------------

In the example above, what is the specification function,
`trailing-0-count`? In this case it's an ACL2 function from the
`bitops` library, dating from 2015. Who is to say that
`trailing-0-count` is bug-free or corresponds to what the spec for
`LowestSetBitNZ` is supposed to be? Ideally, people familiar with the
intentions behind the ASL stdlib definitions should review these
specifications and determine whether they are right. In this case, it
has two facts in its favor: (1) it was developed independently and for
general purposes, not just as a spec for an ASL function, (2) it has a
straightforward, readable recursive definition, and (3) it makes sense
to the author of these proofs as a spec for this function. The more
review the specifications receive, the better.

Two particular specification functions may need further explanation.

### Rational-exponent and integer-length

Integer-length is a Common Lisp function that produces, according to
the Common Lisp spec, "the number of bits needed to represent [the
input] in binary two's-complement format", although in actuality it is
the number of bits needed if you leave out the sign bit---e.g.,
`(integer-length 0)` and `(integer-length -1)` both equal 0. For
positive integers, this is always one less than the floor of the log
base 2. We use it as such in the specifications for `FloorLog2` and
`CeilLog2`.

Rational-exponent is a function in the `bitops` library which finds
the floor of the log base 2 of the absolute value of its input. It has
been used in floating point reasoning, e.g. in the `intel/ifp`
library. It has a rather complicated definition since it is intended
for relatively efficient execution on rational values, but it comes
with theorems that ensure it is correct (e.g.,
`rational-exponent-unique` which says that for x between two
consecutive powers of 2, the rational-exponent of x equals the lower
power). It is used as the specifcation for `ILog2` and is also used to
define the correctness condition for `SqrtRounded` (discussed below).

### Sqrtrounded

The specification function for ASL's `SqrtRounded` is a function
called `sqrtrounded`, which is really just a rendering into ACL2 of
the same algorithm used for the ASL function. However, in
"sqrtrounded-alg.lisp" we define a relational correctness condition
`sqrt-rounded-correctness-condition` and show that (1) a value
satisfying that condition for a given input must be unique
(`sqrt-rounded-correctness-condition-unique`), and (2) the
`sqrtrounded` function always satisfies that condition
(`sqrtrounded-correct`).

