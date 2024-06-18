//R_LCFD: A local declaration shall not declare an identifier which is
//already in scope at the point of declaration.

// RUN: not interp %s | FileCheck %s

var a = 10;

func main() => integer
begin
    var a = 4;
    print(a);

    return 0;
end
