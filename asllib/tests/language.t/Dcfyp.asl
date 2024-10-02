//D_CFYP: A function invocation consists of an identifier which
//denotes the name of the invoked function, followed by a
//parenthesised list of zero or more expressions which denote
//the actual arguments of the invocation.

// RUN: interp %s | FileCheck %s

func test(a: integer) => integer
begin
    return a;
end

func main() => integer
begin
    var a = test(10);
    return 0;
end
