//D_XRBT: An expression is a compile-time-constant expression if each one of its atomic expressions is one of:
//- a literal constant
//- a compile-time-constant storage element identifier
//- an immutable storage element identifier with a compile-time-constant
//initializer expression. 
//- compile-time-constant function or getter invocations

// RUN: interp %s | FileCheck %s

constant a : integer = 10;

getter aa => integer
begin
    return a;
end

func aaa() => integer
begin
    return a;
end

func main() => integer
begin
    var b = 10 + 1;
    var c = a + 1;
    var d = aa + 1;
    var e = aaa() + 1;

    return 0;
end
