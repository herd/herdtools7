type MyType of integer{1..12}; // Name a well-constrained integer type.

func foo(x: integer{1..7}) => integer{1..12}
begin
  return x;
end;

func main () => integer
begin
  var x: integer{1..12};
  x = 4;
  x = foo(x as integer{1..7});

  let y: integer{1..12} = x;
  let x2 = x as integer{1..11};
  assert x2 == x;

  let z : integer{2..24} = x + y;
  // The type of 'w' is inferred to be integer{2..24}.
  var w = x + y;
  w = z;

  var c = 3; // The type of 'c' is inferred to be integer{3}.

  // The following is illegal as '2 as integer{3}'
  // is considered side-effecting, which is not allowed in type
  // definitions.
  // var - = 3 as integer{2 as integer{3}};
  return 0;
end;
