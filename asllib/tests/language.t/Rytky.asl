//R_YTKY: Boolean literals have type boolean. See R_MXPS.
//
//R_MXPS: Boolean literals are written using TRUE or FALSE.
// Definition of a boolean
//<boolean_lit> ::= ( {"TRUE"} | {"FALSE"} )

// RUN: interp %s | FileCheck %s

var a: boolean = TRUE;
var b: boolean = FALSE;

func main() => integer
begin
    return 0;
end
