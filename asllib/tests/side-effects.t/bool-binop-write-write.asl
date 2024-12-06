var X: integer = 0;

func foo (x : integer) => boolean
begin
  if X == x then return FALSE;
  else
    X = x;
    return TRUE;
  end;
end;

func main () => integer
begin
  X = 2;

  if foo (2) && foo (3) then
    println("Impossible");
  end;

  if foo (2) || foo (3) then
    println("Should print.");
  end;

  if foo (2) --> foo (3) then
    println ("Should print.");
  end;

  return 0;
end;
