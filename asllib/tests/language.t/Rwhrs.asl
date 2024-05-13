//R_WHRS: If the declared type of a setter’s RHS argument has the structure
//of a bitvector or a type with fields, then if a bitslice or field
//selection is applied to a setter invocation, then the assignment to that
//bitslice is implemented using the following Read-Modify-Write (RMW)
//behavior:
//- invoking the getter of the same name as the setter, with the same actual
//arguments as the setter invocation 
//- performing the assignment to the bitslice or field of the result of the
//getter invocation
//- invoking the setter to assign the resulting value

// RUN : interp %s | FileCheck %s

// ! I am not sure what this means

func main() => integer
begin
     return 0;
end
