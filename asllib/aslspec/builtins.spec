operator cond_case[T](Bool, T) -> T
{
  math_macro = \condcase,
  custom = true,
};

// TODO: add a custom rendering where all conditions
// and all values are properly aligned.
// Perhaps this can be achieved by adding a raw attribute to macros, which doesn't wrap them with braces.
// TODO: add custom syntac for cases.
variadic operator cond_op[T](list1(T)) -> T
{
  math_macro = \condop,
  custom = true,
};

constant bot { "bottom", math_macro = \bot };

constant None { "the empty \optionalterm{}" };

constant empty_set
{
    "the empty set",
    math_macro = \emptyset,
};

constant empty_list
{
    "the empty list",
    math_macro = \emptylist,
};

typedef Bool
{  "Boolean",
    math_macro = \Bool,
} =
  | True
  { "true", math_macro = \True }
  | False
  { "false", math_macro = \False }
;

typedef N
{  "natural number",
    math_macro = \N,
};

typedef Z
{ "integer",
    math_macro = \Z,
};

operator is_integer(q: Q) -> Bool
{
  "{q} is an integer",
  math_macro = \isintegerop,
};

typedef Q
{ "rational",
   math_macro = \Q,
};

typedef ASTLabels
{ "AST label",
   math_macro = \ASTLabels,
};

operator some[T](T) -> option(T)
{
  math_macro = \some,
};

operator assign[T](lhs: T, rhs: T) -> Bool
{
  math_macro = \eqdef,
};

operator reverse_assign[T](lhs: T, rhs: T) -> Bool
{
  math_macro = \eqname,
};

operator equal[T](a: T, b: T) -> (c: Bool)
{
  math_macro = \equal,
  prose_application = "equating {a} to {b} yields {c}",
};

operator not_equal[T](T, T) -> Bool
{
  math_macro = \notequal,
};

operator if_then_else[T](Bool, T, T) -> T
{
  math_macro = \ifthenelseop,
};

variadic operator and(list1(Bool)) -> Bool
{
  associative = true,
  math_macro = \land,
};

variadic operator or(list1(Bool)) -> Bool
{
  associative = true,
  math_macro = \lor,
};

operator list_and(list0(Bool)) -> Bool
{
  math_macro = \listand,
};

operator list_or(list0(Bool)) -> Bool
{
  math_macro = \listor,
};

operator not(Bool) -> Bool
{
  math_macro = \opnot,
};

operator iff(Bool, Bool) -> Bool
{
  math_macro = \IFF,
};

operator implies(Bool, Bool) -> Bool
{
  math_macro = \implies,
};

variadic operator num_plus[NumType](list1(NumType)) -> NumType
{
  associative = true,
  math_macro = \numplus,
};

operator num_minus[NumType](NumType, NumType) -> NumType
{
  math_macro = \numminus,
};

operator num_negate[NumType](NumType) -> NumType
{
  math_macro = \numnegate,
};

// Negation for number types.
operator negate[NumType](NumType) -> NumType
{
  math_macro = \negate,
};

variadic operator num_times[NumType](list1(NumType)) -> NumType
{
  math_macro = \numtimes,
  associative = true,
};

operator num_divide[NumType](NumType, NumType) -> NumType
{
  math_macro = \numdivide,
};

operator num_exponent[NumType](NumType, NumType) -> NumType
{
  math_macro = \numexponent,
  custom = true,
};

operator less_than[NumType](NumType, NumType) -> Bool
{
  math_macro = \lessthan,
};

operator less_or_equal[NumType](NumType, NumType) -> Bool
{
  math_macro = \lessorequal,
};

operator greater_than[NumType](NumType, NumType) -> Bool
{
  math_macro = \greaterthan,
};

operator greater_or_equal[NumType](NumType, NumType) -> Bool
{
  math_macro = \greaterorequal,
};

// Notice that the operator returns True when domain is empty.
operator forall[T](bound_variable: T, domain: powerset(T), condition: Bool) -> Bool
{
  "true if and only if {condition} holds for all values of {bound_variable} in {domain}",
  math_macro = \forallop,
};

operator exists[T](bound_variable: T, domain: powerset(T), Bool) -> Bool
{
  math_macro = \existsop,
};

operator list_forall[T](bound_variable: T, domain: list0(T), Bool) -> Bool
{
  math_macro = \listforall,
};

operator list_exists[T](bound_variable: T, domain: list0(T), Bool) -> Bool
{
  math_macro = \listexists,
};
