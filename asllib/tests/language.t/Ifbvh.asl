//I_FBVH: Setters are procedure-like subprograms whose invocation
//syntactically looks like a variable write or array write. See also R_YDFQ.
//R_YDFQ: Where the left hand side of an assignment is to an identifier
//which is declared as a setter, the assignment is treated in the same way
//as a procedure invocation. In this case, if a sequence of
//null_or_slice_lists is present, each must consist of a single expression.
//The sequence of null_or_slice_lists shall be used as the actual
//expressions for the invocation of the setter.


//RUN: interp %s | FileCheck %s

var b: integer;

getter a[] =>integer
begin
    return b;
end

getter c[value1 : integer] =>integer
begin
    return b;
end

setter a[] = value: integer
begin
    b = value;
    return;
end

setter c[value1 : integer] = value: integer
begin
    b = value * value1;
    return;
end

func main() => integer
begin
    a[] = 10;
    c[10] = 10;

    return 0;
end
