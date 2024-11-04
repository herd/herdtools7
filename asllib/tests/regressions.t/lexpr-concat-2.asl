
func foo {N}(value : bits(N*8))
begin
  let half = N DIV 2;
  var (a, b) = (Zeros{half*8}, Zeros {half*8});
  [a, b] = value;
end;

