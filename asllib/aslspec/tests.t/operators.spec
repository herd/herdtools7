typedef N;
typedef Bool;

operator assign[T](lhs: T, rhs: T) -> Bool
{
  math_macro = \eqdef,
  prose_application = "define {lhs} as: {rhs}",
};

operator if_then_else[T](c: Bool, r_true: T, r_false: T) -> T
{
  math_macro = \ifthenelseop,
  prose_application = "if {c} then {r_true} else {r_false}"
};

operator equal[T](a: T, b: T) -> (c: Bool)
{
  math_macro = \equal,
  prose_application = "{a} is equal to {b}",
};

operator member[T](x: T, s: powerset(T)) -> Bool
{
  math_macro = \member,
  prose_application = "{x} is in {s}",
};

variadic operator num_plus(addends: list1(N)) -> N
{
  associative = true,
  math_macro = \intplus,
  prose_application = "the sum of {addends}"
};

typing relation f(a: N, b: N, c: N, S: powerset(N)) -> N {
  prose_application = "operating on {a}, {b}, {c}, and {S} yields"
} =
    a + b = c;
    d := if a in S then b else c;
    --
    d;
;

render rule f;
