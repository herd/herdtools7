//I_QJTN: For any function call F (e1 , ...em ), tuple (e1 , ...em ), or 
//operation e1 op e2 (with the exception of &&, || and -->), it is an error
//if the subexpressions conflict with each other by:
//- both writing to the same variable.
//- one writing to a variable and the other reading from that same variable 
//- one writing to a variable and the other throwing an exception
//- both throwing exceptions

// RUN: not interp %s 2>&1| FileCheck %s
// CHECK-NOT: Exception

var globe_var: integer = 0;

func write_to_var(a: integer) => integer
begin
    globe_var = a;
    return a;
end

type exc of exception{err_code:integer};

func throwing() => integer
begin
    throw exc{err_code=0};
    return 10;
end

func main() => integer
begin
    var a: integer = write_to_var(10) + throwing();
    return 0;
end
