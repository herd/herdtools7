//D_VPZZ: A type T type-clashes with S if any of the following hold:
//- they both have the structure of integers
//- they both have the structure of reals
//- they both have the structure of strings
//- they both have the structure of enumeration types with the same
//enumeration literals 
// - they both have the structure of bit vectors
//- they both have the structure of arrays whose element types type-clash
//- they both have the structure of tuples of the same length whose
//corresponding element types type-clash 
//- S is either a subtype or a supertype of T

// RUN: interp %s | FileCheck %s

// ! Nothing to test here

func main() => integer
begin
    return 0;
end
