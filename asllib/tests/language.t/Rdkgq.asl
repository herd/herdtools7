//R_DKGQ: The domain of every bitvector type other than the
//under-constrained width bitvector, is not a subset of the domain of the
//under-constrained width bitvector.

// RUN: interp %s | FileCheck %s

// ! There is nothing to test here

func main() => integer
begin
    return 0;
end
