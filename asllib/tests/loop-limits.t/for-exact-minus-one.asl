func run_for (n: integer)
begin
  var counter : integer = 0;
  for i = 1 to n looplimit 10 do
    counter = counter + 1;
  end;

  assert counter == n;
end;

func main () => integer
begin
  run_for (9);

  return 0;
end;

