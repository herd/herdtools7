// RUN: not interp %s | FileCheck %s

func foo{N, M}(bv1 : bits(N), bv2 : bits(M))
begin
    var a : integer{0..N};
    var b : integer{0..M};
    a = b; // illegal
end

func main() => integer
begin
    return 0;
end
