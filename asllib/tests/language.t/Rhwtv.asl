// RUN: interp %s | FileCheck %s

var a = 10;
var b = 10;

func c(d: integer, e: integer)
begin
    var f = a + b + d + e;
end

func main() => integer
begin
    return 0;
end
