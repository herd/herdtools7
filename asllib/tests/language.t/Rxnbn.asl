// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a : bits(10 + 10);
    var b : bits(20) = a;
    return 0;
end
