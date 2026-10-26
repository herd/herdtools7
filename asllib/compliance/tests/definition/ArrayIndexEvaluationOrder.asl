var g : integer = 0;

// Increment g before returning an array of increasing values.
func make_array() => array[[8]] of integer
begin
  var arr : array[[8]] of integer;
  arr[[1]] = 1;
  arr[[2]] = 2;
  arr[[3]] = 3;
  g = g + 1;
  return arr;
end;

// Read g while calculating an array index.
readonly func index(n: integer) => integer
begin
  return n + g;
end;

func main() => integer
begin
  // The base expression is evaluated before the index expression, so this
  // reads element 3 rather than element 2.
  println make_array()[[index(2)]];
  return 0;
end;
