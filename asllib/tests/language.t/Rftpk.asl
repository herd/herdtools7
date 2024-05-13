//R_FTPK: A value of type T may be used in a return statement if and only if
//T type-satisfies the enclosing function’s return type.

// RUN: interp %s | FileCheck %s

type a of integer;
type b subtypes a;

func aa() => a
begin
    var bb: b;
    return bb;
end

func main() => integer
begin
    return 0;
end
