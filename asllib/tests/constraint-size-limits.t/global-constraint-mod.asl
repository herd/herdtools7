
constant A = 1 << 10;
constant B = 1 << 20;

let a = ARBITRARY: integer {1..A};
let b = ARBITRARY: integer {1..B};

var z = a MOD b;

func main () => integer
begin
  return 0;
end;
