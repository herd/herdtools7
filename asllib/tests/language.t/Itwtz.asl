// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    var a: bits(2);
    var b: bits(4) = a;
    return 0;
end
