// RUN: interp %s | FileCheck %s

func testa{N: integer}(a: bits(N))
begin
    pass;
end

func testb(a: bits(N), N: integer)
begin
    pass;
end

func main() => integer
begin
    return 0;
end
