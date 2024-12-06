var X: integer = 0;
var Y: integer = 0;

func set_and_return_X () => integer
begin
  X = 2;
  return 3;
end;

func main () => integer
begin
  let y = set_and_return_X () + Y;

  return 0;
end;

