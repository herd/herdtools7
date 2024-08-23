
func foo (N: integer, value : bits(N*8))
begin
  let half = N DIV 2;
  var a, b : bits(half*8);
  [a, b] = value;
end

