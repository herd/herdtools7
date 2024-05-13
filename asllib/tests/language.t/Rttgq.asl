//R_TTGQ: It is a type-checking error if an expression which invokes a
//primitive operator does not match exactly one primitive operation.

// RUN: not interp %s | FileCheck %s

type a of boolean;

func main() => integer
begin
    var aa: a;
    var aaa: integer;

    var b = aa || aaa;
    return 0;
end
