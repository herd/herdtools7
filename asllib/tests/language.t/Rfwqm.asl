//R_FWQM: The domain of the under-constrained integer type is a proper
//subset of the domain of the unconstrained integer type.

// RUN: not interp %s | FileCheck %s

var a : integer = b;
func main() => integer
begin
    return 0;
end
