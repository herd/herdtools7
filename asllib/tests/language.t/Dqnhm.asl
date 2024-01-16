// RUN: interp %s | FileCheck %s

func compiletime(a: integer, b: integer)
begin
    var c = 10;
end

func test{N:integer}(a: bits(N))
begin
    pass;
end

func main() => integer
begin
    compiletime(10, 10);
    test(Zeros(10));
    return 0;
end
