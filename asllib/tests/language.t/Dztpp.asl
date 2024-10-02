//D_ZTPP: Constrained integer types are either well-constrained or
//under-constrained.

// RUN: interp %s | FileCheck %s

// ! Nothing to test
func main() => integer
begin
    return 0;
end
