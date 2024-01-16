// RUN: interp %s | FileCheck %s

func test{N:integer{}}(a: bits(N))
begin
    var b: integer{0..N};
    pass;
end

func main() => integer
begin
    return 0;
end
