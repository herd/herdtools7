type MyType of (integer, boolean); // Tuple types can be aliased.

func foo(x: (integer, boolean)) => (integer, boolean)
begin
  let (z, y): (integer, boolean) = x;
  return (z + 1, FALSE ==> y);
end;

func main() => integer
begin
  var x: (integer, boolean);
  x = (3, TRUE);
  x = foo(x as(integer, boolean));
  let y: (integer, boolean) = x;

  let (x0, x1) = x as (integer, boolean); // Tuples can be deconstructed.
  assert x0 == 4 && x1 == TRUE;
  // Tuple elements can be accessed via field-like notation.
  assert x0 == x.item0 && x1 == x.item1;
  return 0;
end;
