var C1 : collection { field: bits(1) };
var C2 : collection { field: bits(1) };

func main() => integer begin
  let x = (if TRUE then C1 else C2).field;
  return 0;
end;
