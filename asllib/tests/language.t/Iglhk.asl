//I_GLHK: When initializing global variables without initializers, different
//values may be used for different variables, different values may be used
//for the same variable in different implementations, and different values
//may be used for the same variable in the same implementation in different
//executions. An implementation may, but is not required to, generate
//initial values using a pseudorandom generator whose seed is provided as an
//input parameter.

// RUN: interp %s | FileCheck %s

// ! TODO 

func main() => integer
begin
    return 0;
end
