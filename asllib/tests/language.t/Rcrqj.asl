//R_CRQJ: There are division operations div_int and fdiv_int and a remainder
//operation frem_int.

// RUN: interp %s | FileCheck %s


func main() => integer
begin
    var a: integer = div_int(10, 2);
    var b: integer = fdiv_int(10, 3);
    var c: integer = frem_int(10, 2);

    return 0;
end
