//D_KCYT: A procedure invocation statement calls a procedure subprogram
//using the given actual arguments. The subprogram must not have a return
//type.

// RUN: not interp %s | FileCheck %s

func a() => integer
begin
    return 10;
end

func main() => integer
begin
    a();
    return 0;
end
