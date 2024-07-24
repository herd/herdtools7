//R_FWMM: The domain of the under-constrained integer type is a proper
//subset of the domain of the unconstrained integer type.

// RUN: interp %s | FileCheck %s

// ! Nothing to test here

func main() => integer
begin
    return 0;
end
