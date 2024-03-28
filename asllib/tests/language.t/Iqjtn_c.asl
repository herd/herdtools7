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
