typedef A { "A" };

relation combine((left: A, right: A)) -> (result: A)
{
  "combines {left} and {right} into {result}",
  prose_transition = "combining {left} and {right} yields",
};

relation caller(pair: (A, A)) -> (result: A)
{
  "calls combine for {pair}, yielding {result}",
  prose_transition = "calling combine for {pair} yields",
} =
  combine(pair) -> result;
  --
  result;
;
