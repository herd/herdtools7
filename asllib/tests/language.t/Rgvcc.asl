//R_GVCC: When an exception is thrown, execution proceeds to the catch of
//the innermost try of the enclosing subprogram in the call stack. If there
//is no enclosing try statement when an exception is thrown then execution
//terminates.

// RUN: not interp %s | FileCheck %s

type a of exception {};

func main() => integer
begin
    throw a{};
    return 0;
end
