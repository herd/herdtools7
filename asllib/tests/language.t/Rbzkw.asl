// RUN: interp %s | FileCheck %s

func test(a: integer{}, b: integer{0..3})
begin
    var c = a + b;
end

func main() => integer
begin
    return 0;
end
