// RUN: interp %s | FileCheck %s

func test(a: integer{})
begin
    pass;
end

func test2{a: integer}(b: bits(a))
begin
    test(a);
end

func main() => integer
begin
    return 0;
end
