type MyType of integer {1..12};

func foo (x: integer {1..12}) => integer {1..12}
begin
  return x;
end;

func main () => integer
begin
  var x: integer {1..12};

  x = 4;
  x = foo (x as integer {1..12});
  
  let y: integer {1..12} = x;

  let x2 = x as integer {1..11};
  assert x2 == x;

  return 0;
end;

