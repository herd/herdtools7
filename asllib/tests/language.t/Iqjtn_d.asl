// RUN: not interp %s 2>&1| FileCheck %s
// CHECK-NOT: Exception

type exc of exception{err_code:integer};

func throwing() => integer
begin
    throw exc{err_code=0};
    return 10;
end

func main() => integer
begin
    var a: integer = throwing() + throwing();
    return 0;
end
