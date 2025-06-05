type foo of (integer{1}, boolean);

func good_basic() => integer{42}
begin
  let x : integer{} = 42;
  return x;
end;

func good_tuple() => (integer{42}, integer{43})
begin
  let y : (integer{}, boolean, integer{}) = (42, TRUE, 43);
  return (y.item0, y.item2);
end;

func good_named_tuple() => (integer{1}, boolean)
begin
  var f : foo;
  let z : (integer{}, boolean) = f;
  return z;
end;

// Globals
var A : integer{} = 1;
let B : integer{} = 1;
constant C : integer{} = 1;

func acceptOnlyOne(x: integer{1})
begin
  pass;
end;

func main() => integer
begin
  acceptOnlyOne(A);
  acceptOnlyOne(B);
  acceptOnlyOne(C);

  return 0;
end;
