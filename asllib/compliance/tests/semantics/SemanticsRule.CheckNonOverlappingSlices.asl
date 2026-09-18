func set_bits{N}(x: bits(N)) => bits(N)
begin
    var y = x;
    y[N-1:N-2, 0] = '111';
    return y;
end;

func main () => integer
begin
  var x = '0000';
  x = set_bits{4}(x);
  assert x == '1101';
  return 0;
end;
