var X: integer = 0;

func write_X () => integer
begin
  let x = X;
  X = x + 1;
  return x;
end;

type T of integer {write_X ()};

