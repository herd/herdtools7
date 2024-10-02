//I_GLWM: When used as part of the type of a storage element’s declaration,
//a constrained width bitvector of undetermined width allows that part of
//the storage element’s width to be implicitly determined by the
//initialization expression.

// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a: bits(2) = '00';
    return 0;
end
