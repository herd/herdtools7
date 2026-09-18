func set_bits{N}(x: bits(N)) => bits(N)
begin
    var y = x;
    y[N-1:N-2, 0] = '111';
    return y;
end;

func main () => integer
begin
  var x = '00';
  x = set_bits{2}(x);
  assert x == '11';
  return 0;
end;
