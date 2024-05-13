//I_CVXB: Note that a named type whose structure is a bitvector type is not
//itself a bitvector type and is therefore not identical to any other
//bitvector type.

// RUN : interp %s | FileCheck %s

// ! TODO 
func main() => integer
begin
     return 0;
end
