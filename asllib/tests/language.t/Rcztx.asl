//R_CZTX: An integer type with a non-empty constraint is called a
//well-constrained integer.

// RUN: interp %s | FileCheck %s

type well_constrained of integer {10, 11, 30};

func main() => integer
begin
    return 0;
end
