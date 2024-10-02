//D_CSFT: A subprogram declaration is an execution-time
//declaration if it makes use of any of the following:
// - an execution-time storage element
// - an execution-time expression
// - an execution-time subprogram invocation

// RUN: interp %s | FileCheck %s

var counter: integer = 10;

func storage() => integer
begin
    return counter;
end

func expression_func()
begin
    counter = counter + 1;
end

func function() => integer
begin
    expression_func();
    return storage();
end

func main() => integer
begin
    return 0;
end
