var X: integer = 0;

func read_X () => integer
begin
  let x = X;
  return x;
end;

func main () => integer
begin
  var x: integer = 10;
  var y: integer = 0;

  for i = 0 to read_X () do
    X = y * y + x ;
  end;

  return 0;
end;



