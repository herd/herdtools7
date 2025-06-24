func main() => integer
begin
  var x : integer = 1;
  let t: integer{x..x+1} = 2; // illegal as 'x' is not symbolically evaluable.
  return 0;
end;
