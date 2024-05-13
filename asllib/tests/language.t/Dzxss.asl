//D_ZXSS: Integer types are either unconstrained or constrained.

// RUN: interp %s | FileCheck %s

// ! Nothing to test
func main() => integer
begin
    return 0;
end
