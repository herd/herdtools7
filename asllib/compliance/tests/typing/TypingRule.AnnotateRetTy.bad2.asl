func BadFunc{N}() => bits(N DIV 2)
begin
  var a: bits(N DIV 2);
  return a;
end;

func main() => integer
begin
  var x = BadFunc{3}();
  return 0;
end;
