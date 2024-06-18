//R_PBLF: The syntax
//cexpr as ty 
//is an asserted type conversion indicating that expr_atom shall be treated
//as the required type ty.

// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a: integer{0..10} = 5;

    var b: integer{5..6} = a as integer{5..6};

    return 0;
end
