//R_MXPS: Boolean literals are written using TRUE or FALSE.
//Definition of a boolean:
//  <boolean_lit> ::= ( {"TRUE"} | {"FALSE"} )

// RUN: interp %s | FileCheck %s
// CHECK: FALSE
// CHECK-NEXT: TRUE

func main() => integer
begin
    print(FALSE);
    print(TRUE);
    return 0;
end
