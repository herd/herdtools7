//R_TPHR: The integer type with an empty constraint is called the
//under-constrained integer.

// RUN: interp %s | FileCheck %s

func under_constrained(a: integer{})
begin
    pass;
end

func main() => integer
begin
    return 0;
end
