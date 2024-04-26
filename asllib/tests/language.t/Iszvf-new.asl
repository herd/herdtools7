// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a: bits(4);
    var b = a as bits(4);
    return 0;
end