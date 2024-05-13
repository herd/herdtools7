//D_LJLW: A getter invocation consists of an identifier which denotes the
//name of the invoked getter, either on its own, or followed by a
//bracketed list of zero or more expressions which denote the actual
//arguments of the call. 

// RUN: interp %s | FileCheck %s

getter test => integer
begin
    return 10;
end

func main() => integer
begin
    var a = test;
    return 0;
end
