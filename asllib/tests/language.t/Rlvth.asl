//R_LVTH: Checked type conversions shall not be used on bitwidth expressions
//in subprogram formals or subprogram return types.

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
