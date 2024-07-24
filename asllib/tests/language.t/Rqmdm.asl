//R_QMDM: Reserved identifiers and the elements of boolean_lit cannot be
//used as identifiers.

// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    var newevent: integer;
    return 0;
end
