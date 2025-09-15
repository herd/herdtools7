aslspec is both the name of the language for specifying the semantics of ASL as well
a [tool](README.md) for checking a given specification and for rendering the specification as LaTeX
to be included in the ASL Reference.

aslspec allows specifying the following kinds of elements:
- Types
- Constants
- Relation signatures
- Inference rules (not just yet)

# Type definitions
Types can be used to define generic mathematical types (like Booleans) and AST node types.

Built-in type constructors:
- `powerset(T)`: any subset of elements from type `T`
- `list0(T)`: a possibly-empty list of elements from type `T`
- `list1(T)`: a non-empty list of elements from type `T`
- `(T...T)`: a tuple of elements from respective types `T1`...`Tn`
- `L(T1...Tn)`: a tuple type affixed with the label `L`
- `[f_1: T1...f_n: Tn]`: a function mapping each identifier `f_i` to an element of `T_i`.
    This is conventionally referred to as a record type.
- `L[field: term...field: term]`: a record type affixed with the label `L`.
- `fun A -> B`: the type of total functions from `A` to `B`.
- `partial T -> T`: the type of partial functions from `A` to `B`.

## Type definition vs. type reference
A typedef like `typedef A = L1(B) | L2(C(Z))` treats the labelled type term
appearing directly as variants as type definitions.
So, L1 and L2 as defined along with A.
In contrast, nested type terms like `C(Z)` are considered references to types
defined elsewhere.
In order to refer to a labelled type in a variant, one can surround it by
parenthesis like so: `typedef A = (L(A))`.
This does not change the semantics of the type `A` as a singleton tuple
`(T)` is considered equivalent to `T`.
