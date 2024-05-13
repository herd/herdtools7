//R_PMQB: The domain of a constrained width bitvector is the union of the
//domains of all bitvectors whose width is equal to a member of the
//bitvector’s width constraint.

// RUN: interp %s | FileCheck %s

// ! There is nothing to test here

func main() => integer
begin
    return 0;
end
