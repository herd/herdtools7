type MyType of (integer, boolean);

func foo (x: (integer, boolean)) => (integer, boolean)
begin
  let (z, y): (integer, boolean) = x;
  return (z + 1, FALSE --> y);
end

func main () => integer
begin
  var x: (integer, boolean);

  x = (3, TRUE);
  x = foo (x as (integer, boolean));
  
  let y: (integer, boolean) = x;

  let (x0, x1) = x as (integer, boolean);
  assert x0 == 4 && x1 == TRUE;

  return 0;
end
