//I_YHRP: IYHRP The calculation of constraints shall cause an error if
//necessary, for example where a division by zero occurs, etc.

func helloworld(y : bits(2))
begin
    // x = { 8, 16, 32, 64}
    // y = {00, 01, 10, 11}
    var x = 8 << UInt(y);
    var zz = x DIV 16;
    var xyz = 100 MOD zz;
end

func main() => integer
begin
  return 0;
end
