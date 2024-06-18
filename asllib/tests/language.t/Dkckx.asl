//D_KCKX: A subprogram declaration is a compile-time-constant declaration
//if all of the following are true:
// - the subprogram is side-effect-free
// - all assignments in the subprogram are to the subprogram’s local
// variables
// - all expressions in the subprogram are compile-time-constant
// expressions
// - any subprogram invocations made in the subprogram are
// compile-time-constant subprogram invocations

// RUN: interp %s | FileCheck %s

func localassigns()
begin
    var a = 0;
    var b = 0;
end

func compiletimeexpressions()
begin
    var a = 0 + 0 + 0 + 0 + 0;
end

func compiletimecalls()
begin
    localassigns();
end

func main() => integer
begin
    return 0;
end
