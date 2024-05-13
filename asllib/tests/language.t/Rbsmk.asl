//R_BSMK: The values in a constrained integer’s constraint must all be
//statically evaluable, constrained integer expressions.

// RUN: not interp %s | FileCheck %s

func length(a: integer)
begin
    var b: integer{0..a};
end

func main() => integer
begin
    return 0;
end
