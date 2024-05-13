//R_FMLK: One or more variables, all of the same type, may be declared
//simultaneously with no initialization expression. In this case, a single
//type annotation must be included after the last variable name in the
//declaration.
 
// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a, b, c: integer;

    return 0;
end
