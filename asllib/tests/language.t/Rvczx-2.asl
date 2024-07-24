//R_VCZX: A constrained width bitvector of undetermined width may only be 
//used in the following places:
//- As part of a checked type conversion.
//- As part of the type of a storage element’s declaration where an
//initialization expression is given.
//- As part of the type of a formal argument in a subprogram declaration.

// RUN: interp %s | FileCheck %s

func b{N}(n: bits(N))
begin
    pass;
end

func main() => integer
begin
    return 0;
end
