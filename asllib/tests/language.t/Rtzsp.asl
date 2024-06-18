//R_TZSP: A subprogram invocation matches a subprogram declaration if all of
//the following hold:
//- the name of the invoked subprogram matches the name of the declared
//subprogram
//- every formal argument’s declared type is type-satisfied by its
//invocation type (Ed: now redundant?) 
//- every formal argument’s invocation type is type-satisfied by the
//corresponding actual argument
//- if the subprogram has a return type then it is type satisfied by its
//invocation type.

// RUN: not interp %s | FileCheck %s

func test()
begin
    pass;
end

func main() => integer
begin
    test2();
    return 0;
end
