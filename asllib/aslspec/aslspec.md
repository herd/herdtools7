aslspec is both the name of the language for specifying the semantics of ASL as well
a [tool](README.md) for checking a given specification and for rendering the specification as LaTeX
to be included in the ASL Reference.

aslspec allows specifying the following kinds of elements:
- Types
- Constants
- Relations with or without inference rules
- Operators
- Rendering elements

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
- `{f_1: T1...f_n: Tn}`: a function mapping each identifier `f_i` to an element of `T_i`.
    This is conventionally referred to as a record type.
- `L{field: term...field: term}`: a record type affixed with the label `L`.
- `fun A -> B`: the type of total functions from `A` to `B`.
- `partial T -> T`: the type of partial functions from `A` to `B`.

Type definitions can declare a type parameter in double brackets. References
to the parameter are scoped to the definition. For example:
```
typedef Tree[[T]] =
    | Leaf(value: T)
    | Node(left: Tree[[T]], right: Tree[[T]])
;
```
An application such as `Tree[[Z]]` instantiates `T` with `Z`.
When a parameterized variant is used in an expression, its parameter is
inferred from the types of the variant's components. Repeated occurrences of
the parameter must unify to a single type. For example, `Node(left, right)`
requires the types inferred for `left` and `right` to be compatible.

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

For example:
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

# Operator definitions
Operators define typed expression forms. They are written like function
signatures, may have type parameters, and do not have inference rules:

```
operator equal[T](a: T, b: T) -> Bool
{
    math_macro = \equal,
    prose_application = "{a} is equal to {b}",
};
```

An operator application can use ordinary application syntax, such as
`equal(a, b)`. Some operator names are also produced by built-in expression
syntax; for example, `a = b` is an application of the operator `equal`, and
`a + b` is an application of `num_plus`.

The keyword `variadic` defines an operator that accepts any number of actual
arguments. A variadic operator should have a single formal argument whose type
is `list0(T)` or `list1(T)`:

```
variadic operator union[T](sets: list1(powerset(T))) -> powerset(T)
{
    associative = true,
    math_macro = \unionop,
    prose_application = "the union of {sets}",
};
```

Operator definitions support the following attributes:

- `math_macro`: Optional LaTeX macro name used when rendering the operator.
- `prose_application`: A LaTeX string describing the operator applied to its
  arguments.
- `associative`: A Boolean attribute. When `true` on a variadic operator,
  rendering separates the arguments with the operator macro instead of rendering
  the macro as a function applied to a comma-separated argument list.
- `custom`: A Boolean attribute. When `true`, binary operators use the general
  custom-operator rendering form instead of the default infix rendering form.
- `typecast`: A Boolean attribute used to guide typechecking for operators
  that behave as type conversions. It is transparent for rendering.

# Expressions
Expressions are used in constant initializers, rule premises, transition
outputs, and rule conclusions. Applications are resolved after parsing: a named
application such as `r(a)` becomes a relation or operator application if `r`
names a relation or operator; it becomes a labelled tuple construction if `r`
names a tuple variant; and application of a non-name expression becomes a map
application.

aslspec supports the following expression forms:

- Variables: `x`.
- Tuples: `(e1, e2)` and labelled tuples such as `Pair(e1, e2)`.
- Relation and operator applications: `r(e1, e2)`, `op(e)`, and nullary
  operator applications such as `always_true()`.
- Map applications, where the left-hand side is itself an expression:
  `ctx.pred(a)`.
- Field access: `r.f`.
- List indexing: `xs[i]`.
- Record construction: `[f: e1, g: e2]` and labelled records such as
  `Rec[f: e1, g: e2]`.
- Record updates: `r(f: e1, g: e2)`, meaning the record value `r` with the
  listed fields updated.
- Conditional expressions: `if c then e1 else e2`.
- Multi-way conditional expressions: `cond(c1: e1, c2: e2)`.
- Infix operator forms:
  `lhs := rhs`, `lhs =: rhs`, `lhs = rhs`, `lhs - rhs`, `lhs + rhs`,
  `lhs * rhs`, `lhs / rhs`, `lhs ^ rhs`, `lhs && rhs`, `lhs || rhs`,
  `lhs in rhs`, `lhs not_in rhs`, `lhs <=> rhs`, `lhs <= rhs`, `lhs < rhs`,
  `lhs >= rhs`, `lhs > rhs`, and `lhs != rhs`.
- Transition judgments: `lhs -> rhs`. These are only rule judgments, not
  general sub-expressions.
- Indexed judgments: `INDEX(i, xs: step(xs[i]) -> ys[i])`. These are also only
  rule judgments.

Transition judgments can optionally override short-circuiting alternatives:

```
eval_expr(env, e) -> ResultExpr((v, g), _) | DynErrorConfig(), DivergingConfig();
```

Writing `| ;` after the transition gives an explicit empty list of alternatives.
If no `|` is written, aslspec inserts the alternatives from the relation
definition, when applicable.

Transition outputs intentionally use a smaller expression language than general
premises. They can contain variables, tuples, labelled tuple constructors, list
indexing, and infix operators.

# Inference rules
Relations can include inference rules after their attributes by adding `=`
followed by one or more rule elements:

```
relation if_then_else(flag: Bool, a: Num, b: Num) -> Num {} =
    case Left {
        flag = True;
        --
        a;
    }
    case Right {
        flag = False;
        --
        b;
    }
;
```

A rule element is either a judgment followed by `;`, or a named `case`.
Cases can be nested. When rules are rendered, nested case names form a
dot-separated path such as `Outer.Inner`.

Judgments that are not prefixed by `--` are premises. A judgment prefixed by
`--` is the rule conclusion. Each expanded rule path must have exactly one
conclusion, and it must be the final judgment on that path.

The conclusion is written as an expression, but it is checked and rendered as
an application of the surrounding relation to its inputs. For example, in:

```
relation id(x: Num) -> Num {} =
    --
    x;
;
```

the conclusion `x` is treated as the transition judgment `id(x) -> x`. This is
why a conclusion-level `math_layout` has a top-level left/right pair, as
described in the layout section below.

Premises can be ordinary Boolean expressions, transition judgments, or indexed
judgments. A transition judgment has the form `lhs -> rhs`:

```
relation step_one(x: Num) -> Num {} =
    id(x) -> y;
    --
    y;
;
```

An indexed judgment binds an index and a list variable for the body transition:

```
INDEX(i, xs: step(xs[i]) -> ys[i]);
```

Inference-rule checking type-checks every premise and conclusion. Ordinary
premises must have type `Bool`. Transition premises and conclusions must match
the signature of the relation they apply, including tuple-shaped outputs.
Judgment attributes currently support `math_layout`, which is checked against
the resolved judgment shape before rendering.

# Type rendering definitions
Type rendering controls how types and their variants are displayed.
They allow rendering several types together,
which produces better results than rendering types separately.
They also allow rendering types with just a subset of their variants.

For example:
- `render calls = expr(E_Call), stmt(S_Call);` defines `calls` as a handle for rendering
   how `expr` with just its `E_Call` variant together with `stmt` with just its `S_Call` variant.
- `render ty_int_constraint_and_kind = ty(T_Int), int_constraint(-), constraint_kind(-);`
  specifies three types, where the notation `(-)` for `int_constraint` is used to select
  all of its variants.

Type renders do not support any attributes.

# Inference rule rendering definitions
Rule rendering definitions select which inference rules are emitted as LaTeX
macros. The simplest form renders all expanded rules for a relation under the
same name:

```
render rule if_then_else;
```

This produces rule and prose macros named `if_then_else` for the relation `if_then_else`.
If a relation has inference rules and no explicit rule rendering definition,
aslspec adds this default form automatically.

The explicit form gives the generated macro a different name and selects a
case path from a relation:

```
render rule if_then_else_left = if_then_else(Left);
render rule if_then_else_nested = if_then_else(Outer.Inner);
render rule if_then_else_all = if_then_else(-);
```

The path in parentheses is a dot-separated prefix of the expanded case path.
For example, `if_then_else(Outer)` selects rules under `Outer` and
`if_then_else(Outer.Inner)` selects only rules under `Outer.Inner`. The special path
`-` is the empty path, which is a prefix of every expanded rule path, so
`if_then_else(-)` selects all rules of `if_then_else`.

# Laying out mathematical expressions

The `math_layout` attribute controls whether a compound mathematical object is
rendered horizontally or vertically, and how layouts are passed to its
sub-objects. A layout is structural: it must have the same shape as the term or
expression it is attached to.

Layouts use the following notation:

- `_`: no specific layout. This can always be used as the default layout for any
  term or expression.
- `(_, ..., _)`: a horizontal layout. Each cell is the layout for the
  corresponding sub-object.
- `[_, ..., _]`: a vertical layout. Each cell is the layout for the corresponding
  sub-object.

For example, `(a, (b, c)) { (_, (_, _)) }` lays out the outer tuple
horizontally, passes `_` to `a`, and passes `(_, _)` to the nested tuple
`(b, c)`.

## Default-layout shorthands

The single-cell layouts `(_)` and `[_]` are also accepted as default-layout
shorthands. For a list-shaped term or expression, the single `_` is repeated to
match the number of immediate components being rendered. Thus, `[_]` means
`[_, ..., _]`, with one `_` for each immediate component, and similarly `(_)`
means `(_, ..., _)`. The enclosing parentheses or brackets control only the
layout of those immediate components; each `_` gives its corresponding
component the default layout rather than recursively making it horizontal or
vertical. For example, `(a, (b, c)) { [_] }` is equivalent to
`(a, (b, c)) { [_, _] }`: `a` and `(b, c)` are arranged vertically, while the
nested tuple `(b, c)` uses its default layout. Consequently, `x { [_] }`,
`r(a, b) { (_) }`, and `make_set(a, b, c) { [_] }` are all valid.

## Layouts for type terms

For type terms, `_` is always valid. Any more specific layout follows the type
constructor:

- Atomic type terms such as `Num` or `Bool` accept only the default layouts `_`,
  `(_)`, and `[_]`.
- Tuple and labelled tuple terms use one layout cell per component:
  `Pair(Num, (Bool, Num))` can use `(_, (_, _))`. The shorthand `(_)` or `[_]`
  defaults all components.
- Record terms use one layout cell per field value, in field order:
  `[f: Num, g: (Bool, Num)]` can use `(_, (_, _))`. The shorthand `(_)` or
  `[_]` defaults all fields.
- Function terms use two layout cells, one for the left-hand side and one for
  the right-hand side: `fun (A, B) -> C` can use `((_, _), _)`. As with other
  pair renderers, `(_)` or `[_]` defaults both sides.
- Type operators such as `powerset(T)` and parameterized types pass their layout
  through to the enclosed term.

Relation `math_layout` attributes are checked against a pair consisting of the
relation inputs and outputs. For example, this relation has a top-level
input/output pair; the input side has two arguments, and the output side has two
components:

```
relation r(a: Num, b: Num) -> (c: Num, d: Num)
{
    math_layout = ((_, _), (_, _)),
};
```

## Layouts for rule expressions

For rule judgments and premises, `_` is always valid. Any more specific layout
follows the resolved expression:

- Variables are atomic leaves: `x { _ }` and `x { (_) }` are valid, but
  `x { (_, _) }` is not.
- Nullary operators are also atomic leaves: `op { _ }`, `op { (_) }`, and
  `op { [_] }` are valid, but `op { () }` and `op { [] }` are not.
- Unary operators pass their layout to their single argument.
- Relations, tuples, maps, and non-unary operators use one layout cell per
  argument: `r(a, (b, c)) { (_, (_, _)) }` is valid. The shorthand `(_)` or
  `[_]` defaults all arguments, so `r(a, b) { (_) }` is also valid, while
  `r(a, b) { (_, _, _) }` is not.
- Records use one layout cell per field value:
  `[f: a, g: (b, c)] { (_, (_, _)) }`. The shorthand `(_)` or `[_]` defaults
  all field values.
- Record updates are a pair of the base record expression and the update fields:
  `r(f: a, g: b) { (_, (_, _)) }`. Record updates do not use the pair-renderer
  shorthand, so `r(f: a) { (_) }` is not valid.
- List indexing and field access pass the layout through to the expression they
  contain: `xs[(a, b)] { (_, _) }` checks the index expression, and
  `r.f { _ }` checks the base expression.
- Transition judgments are a pair around the arrow:
  `r(a) -> (b, c) { (_, (_, _)) }`. The shorthand `(_)` or `[_]` defaults both
  sides.

Constant initializer layouts are checked as expression layouts. For example,
`constant c = (True, False) { math_layout = (_, _) };` is valid, and
`constant c = (True, False) { math_layout = (_, _, _) };` is not.

## Layouts for conclusion judgments

Output judgments in rules are written as expressions:

```
--
(b, c) { (_, (_, _)) };
```

Before rendering and layout checking, aslspec rewrites the output expression into
a transition judgment whose left-hand side is the relation applied to its input
arguments:

```
r(a) -> (b, c)
```

Therefore, the layout attached to an output judgment is checked against the
rewritten transition, not just against the output expression. A specific
top-level layout must be a pair: one cell for the left-hand side of `->`, and
one cell for the right-hand side. The single-cell layouts `(_)` and `[_]` are
also accepted as shorthands for defaulting both sides.

For a relation `r(a: Num) -> (b: Num, c: Num)`, this is legal:

```
--
(b, c) { (_, (_, _)) };
```

The first `_` is the layout for the implicit left-hand side `r(a)`, and
`(_, _)` is the layout for the output tuple `(b, c)`. This is illegal:

```
--
(b, c) { (_, _, _) };
```

because the rewritten transition has two top-level parts, not three.

# A note on attributes
A string with an unspecified attribute name, for example, `"description"` is shorthand
for `prose_description = "description"`.

# Using definitions in LaTeX
Running aslspec with `--render` writes a generated LaTeX file, named
`generated_macros.tex` by default. The generated file contains `\Define...`
commands for the checked specification. The wrapper macros in
[rendering_macros.sty](../doc/rendering_macros.sty) expose those definitions
through `\Render...` commands.

A standalone `.tex` file should load the wrapper macros and then input the
generated definitions:

```
\usepackage{rendering_macros}
\input{generated_macros.tex}
```

Each kind of definition produces a render command:

- A type `T` produces `\RenderType{T}`.
- A constant `c` produces `\RenderConstant{c}`.
- A relation `r` produces `\RenderRelation{r}`.
- A type rendering definition `d` produces `\RenderTypes{d}`.
- A rule rendering definition `rr` produces `\RenderRule{rr}` and
  `\RenderProse{rr}`.

For example, a `.tex` file that has loaded the generated macros can use:

```
\RenderType{expr}
\RenderTypes[remove_hypertargets]{expr_literal}
\RenderConstant{empty_tenv}
\RenderRelation{annotate_expr}
\RenderRule{annotate_expr_ELit}
\RenderProse{annotate_expr_ELit}
```

When the ASL document macros are also loaded,
`\RenderProseAndFormally{name}` renders the prose form followed by the formal
inference rule for the same rule-rendering name:

```
\RenderProseAndFormally{annotate_expr_ELit}
```

The optional argument `[remove_hypertargets]` is supported by `\RenderType`,
`\RenderTypes`, and `\RenderConstant`. It is not supported by `\RenderRule`,
`\RenderProse`, or `\RenderRelation`.
