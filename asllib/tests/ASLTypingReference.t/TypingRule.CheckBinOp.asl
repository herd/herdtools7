func main() => integer
begin
  var a: bits(10);
  var b: bits(10);
  var c: bits(10) = a XOR b; // Check that XOR operator is recognized by the lexer.
  return 0;
end