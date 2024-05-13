//D_CCTY: A subprogram invocation is an execution-time
//invocation if the invoked subprogram has an execution-time
//declaration or if the invocation contains any of the following:
// - an execution-time storage element
// - an execution-time expression
// - a bitvector whose width is an execution-time expression

// RUN: interp %s | FileCheck %s

var counter: integer = 0;
config width = 10;

func nonexecution(a: integer) => integer
begin
    return 10;
end

func execution(a: integer) => integer
begin
    counter = counter + a;
    return counter;
end

func bitsfunc{N: integer}(a: bits(N)) => integer
begin
    return 10;
end

func main() => integer
begin
    let a = execution(10);
    var t = 10;
    let b = nonexecution(t);
    let c = nonexecution(execution(10));
    var tt = Zeros(width);
    let d = bitsfunc(tt);

    return 0;
end
