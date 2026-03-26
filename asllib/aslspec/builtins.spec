operator cond_case[T](c: Bool, r: T) -> T
{
  math_macro = \condcase,
  custom = true,
  prose_application = "\item {r} if {c}",
};

// TODO: add a custom rendering where all conditions
// and all values are properly aligned.
// Perhaps this can be achieved by adding a raw attribute to macros, which doesn't wrap them with braces.
// TODO: add custom syntax for cases.
variadic operator cond_op[T](cases: list1(T)) -> T
{
  math_macro = \condop,
  custom = true,
  prose_application = "\begin{itemize}{cases}\end{itemize}",
};

constant None {
  "the  \hyperlink{constant-None}{empty} \optionalterm{}"
};

constant empty_set
{
    "the \hyperlink{constant-emptyset}{empty set}",
    math_macro = \emptyset,
};

constant empty_list
{
    "the \hyperlink{constant-emptylist}{empty list}",
    math_macro = \emptylist,
};

typedef Bool
{
  "Boolean",
  math_macro = \Bool,
} =
  | True
  { "\True{}", math_macro = \True }
  | False
  { "\False{}", math_macro = \False }
;

typedef N
{
  "natural number",
  math_macro = \N,
};

typedef Z
{
  "integer",
  math_macro = \Z,
};

operator is_integer(q: Q) -> Bool
{
  math_macro = \isintegerop,
  prose_application = "{q} is an integer",
};

operator is_not_integer(q: Q) -> Bool
{
  math_macro = \isnotintegerop,
  prose_application = "{q} is not an integer",
};

typedef Q
{
  "rational",
  math_macro = \Q,
};

typedef ASTLabels
{
  "\hyperlink{ASTLabels}{AST label}",
  math_macro = \ASTLabels,
};

operator some[T](v: T) -> option(T)
{
  prose_application = "the \optionalterm{} containing {v}",
  math_macro = \some,
};

operator assign[T](lhs: T, rhs: T) -> Bool
{
  math_macro = \eqdef,
  prose_application = "define {lhs} as {rhs}",
};

operator reverse_assign[T](lhs: T, rhs: T) -> Bool
{
  math_macro = \reverseeqdef,
  prose_application = "{lhs} is {rhs}",
};

operator equal[T](a: T, b: T) -> (c: Bool)
{
  math_macro = \equal,
  prose_application = "{a} is equal to {b}",
};

operator not_equal[T](a: T, b: T) -> Bool
{
  math_macro = \notequal,
  prose_application = "{a} is not equal to {b}",
};

operator if_then_else[T](c: Bool, r_true: T, r_false: T) -> T
{
  math_macro = \ifthenelseop,
  prose_application = "{r_true} if {c}, otherwise {r_false}"
};

variadic operator and(conjuncts: list1(Bool)) -> Bool
{
  associative = true,
  math_macro = \land,
  prose_application = "the \hyperlink{relation-land}{conjunction} of: {conjuncts}",
};

variadic operator or(disjuncts: list1(Bool)) -> Bool
{
  associative = true,
  math_macro = \lor,
  prose_application = "the \hyperlink{relation-lor}{disjunction} of: {disjuncts}",
};

operator binary_or(d1: Bool, d2: Bool) -> Bool
{
  associative = true,
  math_macro = \lor,
  prose_application = "{d1} or {d2}",
};

operator binary_and(d1: Bool, d2: Bool) -> Bool
{
  associative = true,
  math_macro = \land,
  prose_application = "{d1} and {d2}",
};

operator list_and(conditions: list0(Bool)) -> Bool
{
  math_macro = \listand,
  prose_application = "the \hyperlink{relation-land}{conjunction} of all Boolean values in {conditions}",
};

operator list_or(conditions: list0(Bool)) -> Bool
{
  math_macro = \listor,
  prose_application = "the \hyperlink{relation-lor}{disjunction} of all Boolean values in {conditions}",
};

operator not(b: Bool) -> Bool
{
  math_macro = \opnot,
  prose_application = "the \hyperlink{relation-neg}{logical negation} of: {b}",
};

// This is negation, specialized to a single variable to allow
// the macro to drop the parenthesis around the variable.
operator not_single(b: Bool) -> Bool
{
  math_macro = \opnotvar,
  prose_application = "the \hyperlink{relation-neg}{logical negation} of {b}",
};

operator iff(lhs: Bool, rhs: Bool) -> Bool
{
  math_macro = \IFF,
  prose_application = "{lhs} \hyperlink{relation-IFF}{if and only if} {rhs}",
};

operator implies(lhs: Bool, rhs: Bool) -> Bool
{
  math_macro = \implies,
  prose_application = "{lhs} \hyperlink{relation-implies}{implies} {rhs}",
};

variadic operator num_plus[NumType](addends: list1(NumType)) -> NumType
{
  associative = true,
  math_macro = \numplus,
  prose_application = "the sum of {addends}"
};

operator num_minus[NumType](lhs: NumType, rhs: NumType) -> NumType
{
  math_macro = \numminus,
  prose_application = "{lhs} minus {rhs}",
};

operator num_negate[NumType](x: NumType) -> NumType
{
  math_macro = \numnegate,
  prose_application = "minus {x}",
};

variadic operator num_times[NumType](numbers: list1(NumType)) -> NumType
{
  math_macro = \numtimes,
  associative = true,
  prose_application = "the multiplication of {numbers}",
};

operator num_divide[NumType](lhs: NumType, rhs: NumType) -> NumType
{
  math_macro = \numdivide,
  prose_application = "{lhs} divided by {rhs}",
};

operator num_exponent[NumType](lhs: NumType, rhs: NumType) -> NumType
{
  math_macro = \numexponent,
  custom = true,
  prose_application = "{lhs} to the power of {rhs}",
};

operator less_than[NumType](lhs: NumType, rhs: NumType) -> Bool
{
  math_macro = \lessthan,
  prose_application = "{lhs} is less than {rhs}",
};

operator less_or_equal[NumType](lhs: NumType, rhs: NumType) -> Bool
{
  math_macro = \lessorequal,
  prose_application = "{lhs} is less than or equal to {rhs}",
};

operator greater_than[NumType](lhs: NumType, rhs: NumType) -> Bool
{
  math_macro = \greaterthan,
  prose_application = "{lhs} is greater than {rhs}",
};

operator greater_or_equal[NumType](lhs: NumType, rhs: NumType) -> Bool
{
  math_macro = \greaterorequal,
  prose_application = "{lhs} is greater than or equal to {rhs}",
};

// Notice that the operator returns True when domain is empty.
operator forall[T](bound_variable: T, domain: powerset(T), condition: Bool) -> Bool
{
  math_macro = \forallop,
  prose_application = "for all bindings of {bound_variable} in {domain}: {condition}",
};

operator exists[T](bound_variable: T, domain: powerset(T), condition: Bool) -> Bool
{
  math_macro = \existsop,
  prose_application = "for some binding of {bound_variable} in {domain}: {condition}",
};

operator list_forall[T](bound_variable: T, domain: list0(T), condition: Bool) -> Bool
{
  math_macro = \listforall,
  prose_application = "for all bindings of {bound_variable} in {domain}: {condition}",
};

operator list_exists[T](bound_variable: T, domain: list0(T), condition: Bool) -> Bool
{
  math_macro = \listexists,
  prose_application = "for some binding of {bound_variable} in {domain}: {condition}",
};
