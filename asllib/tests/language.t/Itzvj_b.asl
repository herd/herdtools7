// RUN: interp %s | FileCheck %s

func valid{N:integer{4, 8}}(a: bits(N)) => bits(N)
begin
    return a;
end

func main() => integer
begin
    return 0;
end
