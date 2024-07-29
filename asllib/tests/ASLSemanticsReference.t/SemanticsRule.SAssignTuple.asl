func main () => integer
begin
  var x : integer;
  var b : boolean;

  (b,x) = (TRUE,42);

  assert (b && x == 42);
  return 0;
end
