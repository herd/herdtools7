var X: integer = 0;

func set_and_return () => integer
begin
  X = 4;
  return 3;
end

func main () => integer
begin
  let x = set_and_return () + UNKNOWN: integer {0..3};

  return 0;
end
