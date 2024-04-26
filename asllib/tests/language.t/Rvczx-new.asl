// RUN: interp %s | FileCheck %s

func b{N}(n: bits(N))
begin
    pass;
end

func main() => integer
begin
    return 0;
end