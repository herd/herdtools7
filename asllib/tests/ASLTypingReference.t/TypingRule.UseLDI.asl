func main () => integer
begin
  let i = 32; // { Other(i) }
  var (x, y, z) = (0, 1, 2); // { Other(x), Other(y), Other(z) }

  return 0;
end;
