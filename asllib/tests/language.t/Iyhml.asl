// RUN: interp %s | FileCheck %s

func test(a: integer{0..10}, b: integer{4..7})
begin
    var c: integer{0..70} = a * b;
    var d: integer{-7..6} = a - b;
end

func main() => integer
begin
    return 0;
end
