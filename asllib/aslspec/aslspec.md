aslspec is both the name of the language for specifying the semantics of ASL as well
a [tool](README.md) for checking a given specification and for rendering the specification as LaTeX
to be included in the ASL Reference.

aslspec allows specifying the following kinds of elements:
- Types
- Constants
- Relation signatures
- Rendering elements
- Inference rules

# Type definitions
Types definitions define mathematical types (like Booleans) and AST node types.

For example a *basic type*:
```
typedef N
{  prose_description = "natural number",
    math_macro = \N,
};
```
defines a type `N` whose domain of values is left unspecified (although it is
intended to represent natural numbers).
As the example shows, definitions can (and often are required) specify
attributes, which are used to produce LaTeX content.

The attribute `prose_description` is used when the description of the type `N`
is needed for producing prose. This attribute is mandatory for type definitions.

The optional attribute `math_macro` is used when the type is used in a LaTeX
math mode. When no `math_macro` is provided, a default one is created
(the name of the generated macro is produced from the type definition by
removing underscores, e.g., `My_Type` generates a macro `MyType`).

In order to specify the the domain of values for a type, use *complex types*.
For example
```
typedef static_envs
    {
        prose_description = "static environment",
        math_macro = \staticenvs,
    } =
 (G: global_static_envs, L: local_static_envs)
    {
        prose_description = "static environment with global static environment {G} and local static environment {L}",
    }
;
```
means that the type `static_envs` has the domain of all pairs of values where the first
component is from the type `global_static_envs` and the second is from the type `local_static_envs`.
The notation `G: global_static_envs` means that `G` can be used when describing values
of this form, based on the template given by `prose_description`.

aslspec supports the following type constructors:
- `powerset(T)`: any subset of elements from type `T`
- `powerset_finite(T)`: any finite subset of elements from type `T`
- `list0(T)`: a possibly-empty list of elements from type `T`
- `list1(T)`: a non-empty list of elements from type `T`
- `(T...T)`: a tuple of elements from respective types `T1`...`Tn`
- `L(T1...Tn)`: a tuple type affixed with the label `L`
- `[f_1: T1...f_n: Tn]`: a function mapping each identifier `f_i` to an element of `T_i`.
    This is conventionally referred to as a record type.
- `L[field: term...field: term]`: a record type affixed with the label `L`.
- `fun A -> B`: the type of total functions from `A` to `B`.
- `partial T -> T`: the type of partial functions from `A` to `B`.

Complex type definitions can also use *variants* where by a union of different type constructors
is used to define the domain of values.
For example:
```
ast lexpr { prose_description = "\assignableexpression{}", } =
    | LE_Discard
    { prose_description = "discarding \assignableexpression{}", }
    | LE_Var(var: Identifier)
;
```
means that the type `lexpr` is the union of the type defined by `LE_Discard`
(which defines only a single value) and the type defined by `LE_Var(var: Identifier)`.

The `ast` keyword serves the same role as `typedef`, except when rendering the
type definitions. A `typedef` definition uses `=` between the type name and variants,
and a union symbol between variants. An a `ast` definition uses `->` between the type
name and variants, and `|` between the variants.

## Type definition vs. type reference
A definition like `typedef A = L1(B) | L2(C(Z))` treats the labelled type term
appearing directly under `A`, which we refer to as variants, as type definitions.
So, `L1` and `L2` as defined along with `A`.
In contrast, nested type terms like `C(Z)` are considered references to types
defined elsewhere.
In order to refer to a labelled type in a variant, one can surround it by
parenthesis like so: `typedef A = (L(A))`.
This does not change the semantics of the type `A` as a singleton tuple
`(T)` is considered equivalent to `T`.

# Constant definitions
Constants definitions introduce a single (immutable) named value.
They support the following attributes, with the same meaning as type definitions:
- `prose_description`: A LaTeX string describing the constant in prose.
- `math_macro`: Optional LaTeX macro name to use in LaTeX math mode.

Example:
```
constant empty_tenv {
    prose_description = "empty static environment",
    math_macro = \emptytenv,
};
```

# Relation definitions
Relations define the type, or *signature* of a relation symbol.

FOr example:
```
relation annotate_expr(tenv: static_envs, e: expr) -> (t: ty, new_e: expr, ses: powerset(TSideEffect)) | type_error
{
    prose_description = "annotates the expression {e} in the \staticenvironmentterm{} {tenv},
                        resulting in the following:
                        {t} is the type inferred for {e};
                        {new_e} is the \typedast{} for {e}, also known as the \emph{annotated expression};
                        and {ses} is the \sideeffectsetterm{} inferred for {e}. \ProseOtherwiseTypeError",
    prose_application = "annotating {e} in {tenv} yields
        {t}, the annotated expression {new_e} and {ses}\ProseOrTypeError",
    math_layout = ((_,_),(_,_)),
};
```
defines a relation `annotate_expr` between input values of the type `(static_envs,expr)`
and output values of the type `(ty, expr, powerset(TSideEffect)) | type_error`
where `|` is a shorthand for union.

Relation definitions support the following attributes, with the same meaning as type definitions:
- `prose_description`: A LaTeX string describing the relation in prose.
- `prose_application`: A LaTeX string describing the relation in prose relative to given inputs
    and outputs (in the premise/conclusion of an inference rule).
- `math_macro`: Optional LaTeX macro name to use in LaTeX math mode.


# Type rendering definitions
Type rendering controls how types and their variants are displayed.
They allow rendering several types together,
which produces better results than rendering types separately.
They also allow rendering types with just a subset of their variants.

For example:
- `render calls = expr(E_Call), stmt(S_Call);` defines `calls` as a handle for rendering
   how `expr` with juts its `E_Call` variant together with `stmt` with just its `S_Call` variant.
- `render ty_int_constraint_and_kind = ty(T_Int), int_constraint(-), constraint_kind(-);`
  specifies three types, where the notation `(-)` for `int_constraint` is used to select
  all of its variants.

Type renders do not support any attributes.

# A note on attributes
A string with an unspecified attribute name, for example, `"description"` is shorthand
for `prose_description = "description"`.

# Using definitions in LaTeX
Each kind of definition produces a definition of a LaTeX macro:
- A type `T` produces a macro `\RenderType{T}`
- A constant `c` produces a macro `\RenderConstant{c}`
- A relation `r` produces a macro `\RenderRelation{r}`
- A rendering definition `d` produces a macro `RenderTypes{d}`

These macros are defined in [here](../doc/rendering_macros.tex)
