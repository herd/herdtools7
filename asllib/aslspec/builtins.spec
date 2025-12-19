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

typedef Q
{ "rational",
   math_macro = \Q,
};

operator assign[T](lhs: T, rhs: T) -> Bool
{
  math_macro = \eqdef,
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

operator and(list1(Bool)) -> Bool
{
  associative = true,
  math_macro = \land,
};

operator or(list1(Bool)) -> Bool
{
  associative = true,
  math_macro = \lor,
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

operator num_plus[NumType](list1(NumType)) -> NumType
{
  associative = true,
  math_macro = \numplus,
};

operator num_minus[NumType](list1(NumType)) -> NumType
{
  associative = true,
  math_macro = \numminus,
};

// Negation for number types.
operator negate[NumType](NumType) -> NumType
{
  math_macro = \negate,
};

operator num_times[NumType](list1(NumType)) -> NumType
{
  math_macro = \numtimes,
};

operator num_divide[NumType](NumType, NumType) -> NumType
{
  math_macro = \numdivide,
};

operator num_exponent[NumType](NumType, NumType) -> NumType
{
  math_macro = \numexponent,
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
