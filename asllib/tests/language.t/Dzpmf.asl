//D_ZPMF: An expression is an execution-time expression if either:
//- it contains an execution-time storage element identifier
//- it contains an execution-time function or getter invocation

// RUN: interp %s | FileCheck %s

var a : integer = 10;

getter aa => integer
begin
    a = a + 1;
    return a;
end

func aaa() => integer
begin
    a = a + 2;
    return a;
end

func main() => integer
begin
    var b = a + 1;
    var bb = aa + 1;
    var bbb = aaa() + 1;

    return 0;
end
