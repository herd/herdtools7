operator cond_case[T](c: Bool, r: T) -> T
{
  math_macro = \condcase,
  custom = true,
  prose_application = "if {c} holds then {r}",
};

// TODO: add a custom rendering where all conditions
// and all values are properly aligned.
// Perhaps this can be achieved by adding a raw attribute to macros, which doesn't wrap them with braces.
// TODO: add custom syntac for cases.
variadic operator cond_op[T](cases: list1(T)) -> T
{
  math_macro = \condop,
  custom = true,
  prose_application = "{cases}",
};

// This constant is for internal use only.
constant bot { "bottom", math_macro = \bot };

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
  { "true", math_macro = \True }
  | False
  { "false", math_macro = \False }
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
  "{q} is an integer",
  math_macro = \isintegerop,
};

operator is_not_integer(q: Q) -> Bool
{
  "{q} is not an integer",
  math_macro = \isnotintegerop,
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
  prose_application = "if {c} then {r_true} else {r_false}"
};

variadic operator and(conjuncts: list1(Bool)) -> Bool
{
  associative = true,
  math_macro = \land,
  prose_application = "all of the following hold: {conjuncts}",
};

variadic operator or(disjuncts: list1(Bool)) -> Bool
{
  associative = true,
  math_macro = \lor,
  prose_application = "one of the following holds: {disjuncts}",
};

operator list_and(conditions: list0(Bool)) -> Bool
{
  math_macro = \listand,
  prose_application = "the \hyperlink{relation-land}{conjunction} of all Booleans in {conditions}",
};

operator list_or(conditions: list0(Bool)) -> Bool
{
  math_macro = \listor,
  prose_application = "the \hyperlink{relation-lor}{disjunction} of all Booleans in {conditions}",
};

operator not(b: Bool) -> Bool
{
  math_macro = \opnot,
  prose_application = "the \hyperlink{relation-neg}{logical negation} of {b}",
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
  prose_application = "the sum of all numbers in {addends}"
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
  prose_application = "the multiplication of all numbers in {numbers}",
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
  prose_application = "{lhs} is less or equal to {rhs}",
};

operator greater_than[NumType](lhs: NumType, rhs: NumType) -> Bool
{
  math_macro = \greaterthan,
  prose_application = "{lhs} is greater than {rhs}",
};

operator greater_or_equal[NumType](lhs: NumType, rhs: NumType) -> Bool
{
  math_macro = \greaterorequal,
  prose_application = "{lhs} is greater or equal {rhs}",
};

// Notice that the operator returns True when domain is empty.
operator forall[T](bound_variable: T, domain: powerset(T), condition: Bool) -> Bool
{
  math_macro = \forallop,
  prose_application = "{condition} holds for all bindings of {bound_variable} in {domain}",
};

operator exists[T](bound_variable: T, domain: powerset(T), condition: Bool) -> Bool
{
  math_macro = \existsop,
  prose_application = "{condition} holds for some binding of {bound_variable} in {domain}",
};

operator list_forall[T](bound_variable: T, domain: list0(T), condition: Bool) -> Bool
{
  math_macro = \listforall,
  prose_application = "{condition} holds for all bindings of {bound_variable} in {domain}",
};

operator list_exists[T](bound_variable: T, domain: list0(T), condition: Bool) -> Bool
{
  math_macro = \listexists,
  prose_application = "{condition} holds for some binding of {bound_variable} in {domain}",
};
