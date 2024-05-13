//D_VFTV: An always-throw procedure is one in which all control flow
//paths terminate with a throw statement or a call to an always-throw
//procedure. Members of a set of mutually recursive procedures in which
//all control flow paths terminate with a throw statement, a call to a
//member of the set, or a call to an always-throw procedure outside the
//set are always-throw.

// RUN: interp %s | FileCheck %s

func alwaysThrow(a: integer)
begin
    if (a == 0) then
        throw;
    else
        throw;
    end
end

func main() => integer
begin
    return 0;
end
