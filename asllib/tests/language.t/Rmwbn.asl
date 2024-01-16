// RUN: interp %s | FileCheck %s

config c: integer{} = 10;

func test(N: integer) => bits(N)
begin
    return Zeros(N);
end

func main() => integer
begin
    var a: bits(10) = test(10);
    var b: bits(5) = test(5);

    var cc: bits(c) = test(c);
    return 0;
end
