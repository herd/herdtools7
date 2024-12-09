var X: integer = 0;

func write_X () => integer
begin
  let x = X;
  X = x + 1;
  return x;
end;

func main () => integer
begin
  var x: integer = 10;
  var y: integer = 0;

  for i = 0 to write_X () do
    let - = y * y + x ;
  end;

  return 0;
end;

