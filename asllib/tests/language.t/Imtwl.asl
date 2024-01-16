// RUN: interp %s | FileCheck %s

func a{N:integer{0..10}}(b: bits(N))
begin
    pass;
end

func main() => integer
begin
    a('1111 1100 00');
    return 0;
end
