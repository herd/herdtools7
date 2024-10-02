//R_RHTN: The width of any part of a formal argument or return type which is
//a bitvector type must be a constrained integer.

// RUN: not interp %s | FileCheck %s

config a: integer = 10;

func test(b: bits(a)) => bits(a)
begin
    return b;
end

func main() => integer
begin
    return 0;
end