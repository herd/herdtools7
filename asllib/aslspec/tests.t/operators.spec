typedef N;
typedef Bool;

operator assign[T](T, T) -> Bool
{
  math_macro = \eqdef,
};

operator if_then_else[T](Bool, T, T) -> T
{
  math_macro = \ifthenelseop,
};

operator equal[T](a: T, b: T) -> (c: Bool)
{
  math_macro = \equal,
  prose_application = "equating {a} to {b} yields {c}",
};

operator member[T](x: T, s: powerset(T)) -> Bool
{
  math_macro = \member,
};

operator int_plus(list1(N)) -> N
{
  associative = true,
  math_macro = \intplus,
};

typing relation f(a: N, b: N, c: N, S: powerset(N)) -> N {} =
    a + b = c;
    d := if a in S then b else c;
    --
    d;
;

render rule f;
