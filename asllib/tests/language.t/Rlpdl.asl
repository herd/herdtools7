//R_LPDL: If type id1 of ty subtypes id2; is declared, it is an error if the
//named type id2 is not declared.

// RUN: not interp %s | FileCheck %s

type b of integer subtypes a;

func main() => integer
begin
    return 0;
end
