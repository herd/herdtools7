// RUN: interp %s | FileCheck %s

func test(a: integer) => bits(a)
begin
    var b: bits(a);
    return b;
end

func main() => integer
begin
    return 0;
end
