func main() => integer
begin
  if TRUE then
    print("A");
  elsif FALSE then // OK
    print("B");
  else
    print("C");
  end;

  let x = if TRUE then 1 else if FALSE then 2 else 3; // OK
  let y = if TRUE then 1 elsif FALSE then 2 else 3;   // ERROR

  return 0;
end;
