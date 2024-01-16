// RUN: interp %s | FileCheck %s

var a: bits(10);
config b = 10;
var bb: bits(b);
constant c = 10;
var cc: bits(c);

func main() => integer
begin
    return 0;
end
