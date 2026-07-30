func f() => (integer, integer, integer) begin
  return (1, 2, 3);
end;

func main() => integer begin
  var a : integer;
  var b : integer;
  (a, b) = f();
  return 0;
end;
