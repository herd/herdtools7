// RUN: not interp %s | FileCheck %s

func test(a: integer) => integer
begin
    return a;
end

func test(a: integer) => real
begin
    return 0.0;
end

func main() => integer
begin
    var a: integer = test(10);
    var b: real = test(10);
    return 0;
end
