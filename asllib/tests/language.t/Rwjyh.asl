//R_WJYH: The integer type with no constraint is called the unconstrained
//integer.

// RUN: interp %s | FileCheck %s

type unconstrained of integer;

func main() => integer
begin
    return 0;
end
