// RUN: not interp %s | FileCheck %s

config a: integer{} = 10;

func test{}(N: integer{0..(a as integer{0..10})}, b: bits(N))
begin
    pass;
end

func main() => integer
begin
    return 0;
end
