//R_WKHC: All control flow paths in a function or getter must terminate with
//a return statement, a throw statement, a call to Unreachable() or a call
//to an always-throw procedure.

// RUN: interp %s | FileCheck %s

func a()
begin
    throw;
end

func b()
begin
    a();
end

func main() => integer
begin
    return 0;
end
