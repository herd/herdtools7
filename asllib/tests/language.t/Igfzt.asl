//I_GFZT: These conditions [listed in I_QJTN] are sufficient but not
//necessary to ensure that evaluation order does not affect the result of an
//expression, including any side-effects

//I_QJTN: For any function call F (e1 , ...em ), tuple (e1 , ...em ), or
//operation e1 op e2 (with the exception of &&, || and -->), it is an error
//if the subexpressions conflict with each other by:
//• both writing to the same variable.
//• one writing to a variable and the other reading from that same variable 
//• one writing to a variable and the other throwing an exception
//• both throwing exceptions


// RUN: interp %s | FileCheck %s
// CHECK: TRUE

var a: integer = 10;

func write(b: integer) => integer
begin
    var c: integer = a;
    a = b;
    return c;
end

func main() => integer
begin
    var aa: integer = write(20) + 10;

    a = 10;

    var bb: integer = 10 + write(20);

    print((aa == bb));
    return 0;
end
