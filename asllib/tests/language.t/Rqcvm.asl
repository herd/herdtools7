// RUN: interp %s | FileCheck %s

func test(a: integer, b: integer) => integer
begin
    return 2 * a + b;
end

func main() => integer
begin
    var a = test(10 + 4, test(1, 2));
    return 0;
end
