//R_SNQJ: An expression or subexpression which may result in a zero-length
//bitvector must not be side-effecting.
//The above rule is intended to allow implementations to transform
//expressions to a form where zero-length bitvectors do not exist.

// RUN: not interp %s | FileCheck %s

var t: integer = 0;

func a() => bits(0)
begin
    t = t + 1;
    return Zeros(0);
end

func main() => integer
begin
    return 0;
end
